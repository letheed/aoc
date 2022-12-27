use crate::{Date, Day, Puzzle, Result};

const DATE: Date = Date::new(Day::D19, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

#[allow(clippy::needless_pass_by_value)]
#[allow(clippy::missing_const_for_fn)]
fn solve(_input: String) -> Result {
    answer!();
}
