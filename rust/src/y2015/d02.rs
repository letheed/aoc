use crate::{parse::*, Date, Day, Puzzle, Result};
use std::str::FromStr;

const DATE: Date = Date::new(Day::D02, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

#[allow(clippy::needless_pass_by_value)]
fn solve(input: String) -> Result {
    let (mut paper_area, mut ribbon_length) = (0, 0);
    for line in input.lines() {
        let gift = line.parse::<Gift>()?;
        paper_area += gift.paper_area();
        ribbon_length += gift.ribbon_length();
    }
    answer!(paper_area, ribbon_length);
}

#[derive(Copy, Clone)]
struct Gift(u32, u32, u32);

impl FromStr for Gift {
    type Err = failure::Error;

    #[rustfmt::skip]
    fn from_str(s: &str) -> Result<Self> {
        let mut sizes = nom!(
            do_parse!(Str(s),
                a: uint!(u32) >> char!('x') >>
                b: uint!(u32) >> char!('x') >>
                c: uint!(u32) >> ([a, b, c])
            )
        )?;
        sizes.sort_unstable();
        let [a, b, c] = sizes;
        Ok(Gift(a, b, c))
    }
}

impl Gift {
    fn paper_area(self) -> u32 {
        let Gift(a, b, c) = self;
        3 * (a * b) + 2 * c * (a + b)
    }

    fn ribbon_length(self) -> u32 {
        let Gift(a, b, c) = self;
        2 * (a + b) + a * b * c
    }
}
