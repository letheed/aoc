use crate::{Date, Day, OkOrFail, Puzzle, Result};
use failure::bail;
use serde_derive::Deserialize;
use std::{
    cmp::max,
    ops::{Generator, GeneratorState::Yielded},
};

const DATE: Date = Date::new(Day::D21, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

#[cfg_attr(feature = "cargo-clippy", allow(collapsible_if))]
#[cfg_attr(feature = "cargo-clippy", allow(needless_pass_by_value))]
fn solve(input: String) -> Result {
    let (player, boss, shop) = parse(&input)?;
    let mut item_sets = shop.item_sets();
    let mut min_win_cost = u16::max_value();
    let mut max_lose_cost = u16::min_value();
    while let Yielded(set) = unsafe { item_sets.resume() } {
        let set_cost = set.cost();
        let mut player = player.clone();
        player.equip_set(set);
        if player.kills(boss.clone()) {
            if set_cost < min_win_cost {
                min_win_cost = set_cost;
            }
        } else {
            if set_cost > max_lose_cost {
                max_lose_cost = set_cost;
            }
        }
    }
    answer!(min_win_cost, max_lose_cost);
}

#[derive(Copy, Clone, Deserialize)]
#[serde(rename_all = "PascalCase")]
struct Item {
    damage: u16,
    armor: u16,
    cost: u16,
}

struct Shop {
    weapons: Vec<Item>,
    armors: Vec<Item>,
    rings: Vec<Item>,
}

impl Shop {
    fn item_sets(&'a self) -> impl Generator<Yield = ItemSet, Return = ()> + 'a {
        move || {
            for w in self.weapons.iter().cloned() {
                yield ItemSet(w, [None; 3]);
                for a in self.armors.iter().cloned() {
                    yield ItemSet(w, [Some(a), None, None]);
                    let mut rings = self.rings.iter().cloned();
                    while let Some(r1) = rings.next() {
                        yield ItemSet(w, [None, Some(r1), None]);
                        yield ItemSet(w, [Some(a), Some(r1), None]);
                        for r2 in rings.clone() {
                            yield ItemSet(w, [None, Some(r1), Some(r2)]);
                            yield ItemSet(w, [Some(a), Some(r1), Some(r2)]);
                        }
                    }
                }
            }
        }
    }
}

struct ItemSet(Item, [Option<Item>; 3]);

impl ItemSet {
    fn cost(&self) -> u16 {
        let mut cost = self.0.cost;
        for item in &self.1 {
            if let Some(item) = item {
                cost += item.cost;
            }
        }
        cost
    }
}

#[derive(Clone, Deserialize)]
struct Character {
    #[serde(rename = "Hit Points")]
    hp: u16,
    #[serde(rename = "Damage")]
    attack: u16,
    #[serde(rename = "Armor")]
    defense: u16,
}

impl Character {
    fn equip(&mut self, item: Item) {
        self.attack += item.damage;
        self.defense += item.armor;
    }

    #[cfg_attr(feature = "cargo-clippy", allow(needless_pass_by_value))]
    fn equip_set(&mut self, set: ItemSet) {
        let ItemSet(weapon, items) = set;
        self.equip(weapon);
        for &item in &items {
            if let Some(item) = item {
                self.equip(item);
            }
        }
    }

    #[cfg_attr(feature = "cargo-clippy", allow(needless_pass_by_value))]
    fn kills(self, other: Self) -> bool {
        let damage_given = max(1, self.attack.saturating_sub(other.defense));
        let damage_taken = max(1, other.attack.saturating_sub(self.defense));
        other.hp / damage_given <= self.hp / damage_taken
    }
}

fn parse(s: &str) -> Result<(Character, Character, Shop)> {
    match *s.split("\n\n").collect::<Vec<_>>() {
        [characters, weapons, armors, rings] => {
            let (player, boss) = match &*parse_csv::<Character>(characters)? {
                [player, boss] => (player.clone(), boss.clone()),
                _ => bail!("expected player and boss characters"),
            };
            let weapons = parse_csv(weapons)?;
            let armors = parse_csv(armors)?;
            let rings = parse_csv(rings)?;
            Ok((player, boss, Shop { weapons, armors, rings }))
        }
        _ => bail!("no empty lines"),
    }
}

fn parse_csv<T>(s: &str) -> Result<Vec<T>>
where T: for<'a> serde::Deserialize<'a> {
    let idx_header = s.find('\n').ok_or_fail("no newline found")? + 1;
    let csv_str = s.get(idx_header..).ok_or_fail("end of input string")?;
    let mut csv = csv::ReaderBuilder::new().from_reader(csv_str.as_bytes());
    Ok(csv.deserialize().collect::<std::result::Result<Vec<_>, _>>()?)
}
