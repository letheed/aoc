use self::{
    Difficulty::{Hard, Normal},
    Spell::{Drain, Missile, Poison, Recharge, Shield},
};
use crate::{parse::*, Date, Day, Puzzle, Result};
use std::cmp::max;

const DATE: Date = Date::new(Day::D22, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

#[cfg_attr(feature = "cargo-clippy", allow(needless_pass_by_value))]
fn solve(input: String) -> Result {
    let boss = parse_boss(&input)?;
    let player = Player::new();
    let mut min_mana_spent = u16::max_value();
    let mut min_mana_spent_hard = u16::max_value();
    let mut fights = vec![
        Fight::new(player.clone(), boss.clone(), Normal),
        Fight::new(player.clone(), boss.clone(), Hard),
    ];
    while let Some(mut fight) = fights.pop() {
        let min_mana_spent = if let Hard = fight.difficulty {
            if fight.player.hp == 1 {
                continue;
            }
            fight.player.hp -= 1;
            &mut min_mana_spent_hard
        } else {
            &mut min_mana_spent
        };
        if let BossOutcome::BossDied = fight.apply_effects() {
            *min_mana_spent = fight.mana_spent;
            continue;
        }
        for &spell in &SPELLS {
            let new_mana_spent = fight.mana_spent + spell.mana_cost();
            if new_mana_spent >= *min_mana_spent {
                continue;
            }
            match spell {
                Recharge if fight.player.effect_recharge != 0 => continue,
                Shield if fight.player.effect_shield != 0 => continue,
                Poison if fight.boss.effect_poison != 0 => continue,
                _ => {}
            }
            let mut fight = fight.clone();
            match fight.player.cast(spell, &mut fight.boss) {
                Outcome::Pending => fight.mana_spent = new_mana_spent,
                Outcome::PlayerDied => continue,
                Outcome::BossDied => {
                    // fight.mana_spent = new_mana_spent;
                    *min_mana_spent = new_mana_spent;
                    continue;
                }
            }
            if let BossOutcome::BossDied = fight.apply_effects() {
                *min_mana_spent = fight.mana_spent;
                continue;
            }
            if let PlayerOutcome::PlayerDied = fight.boss.attack(&mut fight.player) {
                continue;
            }
            fights.push(fight);
        }
    }
    answer!(min_mana_spent, min_mana_spent_hard);
}

#[derive(Clone)]
enum Difficulty {
    Normal,
    Hard,
}

enum Outcome {
    Pending,
    PlayerDied,
    BossDied,
}

enum PlayerOutcome {
    Pending,
    PlayerDied,
}

enum BossOutcome {
    Pending,
    BossDied,
}

#[derive(Clone)]
struct Fight {
    player: Player,
    boss: Boss,
    mana_spent: u16,
    difficulty: Difficulty,
}

impl Fight {
    fn new(player: Player, boss: Boss, difficulty: Difficulty) -> Self {
        Self { player, boss, mana_spent: 0, difficulty }
    }

    fn apply_effects(&mut self) -> BossOutcome {
        self.player.apply_effects();
        self.boss.apply_effects()
    }
}

#[derive(Clone, Default)]
struct Player {
    hp: u8,
    defense: u8,
    mana: u16,
    effect_shield: u8,
    effect_recharge: u8,
}

impl Player {
    fn new() -> Self {
        Self { hp: 50, mana: 500, ..Self::default() }
    }

    fn apply_effects(&mut self) {
        if self.effect_shield != 0 {
            self.effect_shield -= 1;
            if self.effect_shield == 0 {
                self.defense -= 7;
            }
        }
        if self.effect_recharge != 0 {
            self.mana += 101;
            self.effect_recharge -= 1;
        }
    }

    fn cast(&mut self, spell: Spell, boss: &mut Boss) -> Outcome {
        if self.mana < spell.mana_cost() {
            return Outcome::PlayerDied;
        }
        self.mana -= spell.mana_cost();
        match spell {
            Missile => if boss.hp <= 4 {
                return Outcome::BossDied;
            } else {
                boss.hp -= 4;
            },
            Drain => if boss.hp <= 2 {
                return Outcome::BossDied;
            } else {
                boss.hp -= 2;
                self.hp += 2;
            },
            Shield => {
                self.effect_shield = 6;
                self.defense += 7;
            }
            Poison => boss.effect_poison = 6,
            Recharge => self.effect_recharge = 5,
        }
        Outcome::Pending
    }
}

#[derive(Clone, Default)]
struct Boss {
    hp: u8,
    attack: u8,
    effect_poison: u8,
}

impl Boss {
    fn apply_effects(&mut self) -> BossOutcome {
        if self.effect_poison != 0 {
            if self.hp <= 3 {
                return BossOutcome::BossDied;
            }
            self.hp -= 3;
            self.effect_poison -= 1;
        }
        BossOutcome::Pending
    }

    fn attack(&self, player: &mut Player) -> PlayerOutcome {
        let damage = max(1, self.attack.saturating_sub(player.defense));
        if player.hp <= damage {
            PlayerOutcome::PlayerDied
        } else {
            player.hp -= damage;
            PlayerOutcome::Pending
        }
    }
}

const SPELLS: [Spell; 5] = [Recharge, Shield, Missile, Drain, Poison];
const MANA_COST: [u16; 5] = [53, 73, 113, 173, 229];

#[derive(Copy, Clone)]
enum Spell {
    Missile,
    Drain,
    Shield,
    Poison,
    Recharge,
}

impl Spell {
    fn mana_cost(self) -> u16 {
        MANA_COST[self as usize]
    }
}

#[rustfmt::skip]
fn parse_boss(s: &str) -> Result<Boss> {
    nom!(
        do_parse!(Str(s),
            tag!("Hit Points:") >> space >> hp: uint!(u8) >> char!('\n') >>
            tag!("Damage:") >> space >> attack: uint!(u8) >>
            (Boss { hp, attack, ..Boss::default() })
        )
    )
}
