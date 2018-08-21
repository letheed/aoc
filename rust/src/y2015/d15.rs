use crate::{parse::*, Date, Day, OkOrFail, Puzzle, Result};
use derive_more::Add;
use std::{cmp::max, convert::TryFrom, ops::Mul};

const DATE: Date = Date::new(Day::D15, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

#[cfg_attr(feature = "cargo-clippy", allow(needless_pass_by_value))]
fn solve(input: String) -> Result {
    let ingredients = parse_ingredients(&input)?;
    let mut recipes = SubsetSums::new(u8::try_from(ingredients.len())?, 100)
        .ok_or_fail("couldn't create subset sums")?;
    let mut highscore = 0;
    let mut highscore_500cal = 0;
    while let Some(recipe) = recipes.next() {
        let dish = ingredients
            .iter()
            .zip(recipe.iter())
            .fold(Ingredient::default(), |dish, (&ing, &n)| dish + (ing * n));
        let score = dish.score();
        if score > highscore {
            highscore = score;
        }
        if dish.cal == 500 && score > highscore_500cal {
            highscore_500cal = score;
        }
    }
    answer!(highscore, highscore_500cal);
}

#[derive(Copy, Clone, Default, Add)]
struct Ingredient {
    cap: i32,
    dur: i32,
    fla: i32,
    tex: i32,
    cal: i32,
}

impl Mul<u8> for Ingredient {
    type Output = Self;

    fn mul(mut self, other: u8) -> Self::Output {
        let other = i32::from(other);
        self.cap *= other;
        self.dur *= other;
        self.fla *= other;
        self.tex *= other;
        self.cal *= other;
        self
    }
}

impl Ingredient {
    #[cfg_attr(feature = "cargo-clippy", allow(cast_sign_loss))]
    fn score(&self) -> u32 {
        let cap = max(0, self.cap) as u32;
        let dur = max(0, self.dur) as u32;
        let fla = max(0, self.fla) as u32;
        let tex = max(0, self.tex) as u32;
        cap * dur * fla * tex
    }
}

fn parse_ingredients(s: &str) -> Result<Vec<Ingredient>> {
    s.lines().map(parse_ingredient).collect()
}

#[rustfmt::skip]
fn parse_ingredient(s: &str) -> Result<Ingredient> {
    named_args!(property<'a>(tag: &str)<Str<'a>, i32>, do_parse!(
        tag!(tag) >> space1 >> n: int!(i32) >> (n)
    ));
    nom!(
        do_parse!(Str(s),
            alpha >> char!(':') >> space1 >>
            cap: call!(property, "capacity")   >> char!(',') >> space1 >>
            dur: call!(property, "durability") >> char!(',') >> space1 >>
            fla: call!(property, "flavor")     >> char!(',') >> space1 >>
            tex: call!(property, "texture")    >> char!(',') >> space1 >>
            cal: call!(property, "calories")   >>
            (Ingredient { cap, dur, fla, tex, cal })
        )
    )
}

struct SubsetSums {
    sum: u8,
    set: Box<[u8]>,
}

impl SubsetSums {
    fn new(elements: u8, sum: u8) -> Option<Self> {
        if sum >= elements && elements >= 2 {
            let mut set = vec![1; usize::from(elements)].into_boxed_slice();
            *set.last_mut()? = sum - (elements - 2);
            Some(Self { sum, set })
        } else {
            None
        }
    }

    #[cfg_attr(feature = "cargo-clippy", allow(cast_possible_truncation))]
    fn next(&mut self) -> Option<&[u8]> {
        let mut i = self.set[1..].iter().position(|&n| n != 1)? + 1;
        self.set[i] -= 1;
        let partial_sum = self.set[i..].iter().sum::<u8>();
        i -= 1;
        self.set[i] = self.sum - partial_sum - i as u8;
        for n in &mut self.set[0..i] {
            *n = 1;
        }
        Some(&self.set)
    }
}
