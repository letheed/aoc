use crate::{Date, Day, Puzzle, Result};

const DATE: Date = Date::new(Day::D24, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

#[cfg_attr(feature = "cargo-clippy", allow(needless_pass_by_value))]
fn solve(_input: String) -> Result {
    answer!();
}
