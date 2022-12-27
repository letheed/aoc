use failure::bail;
use mod_exp::mod_exp;

use crate::{parse::*, Date, Day, Puzzle, Result};

const DATE: Date = Date::new(Day::D25, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

#[allow(clippy::needless_pass_by_value)]
#[allow(clippy::inconsistent_digit_grouping)]
fn solve(input: String) -> Result {
    let (row, col) = parse_row_column(&input)?;
    let diagonal = row + col - 1;
    let first_code: u64 = 2015_11_25;
    let (base, modulus) = (252_533, 33_554_393);
    let exp = (diagonal - 1) * diagonal / 2 + col - 1;
    let code = (mod_exp(base, exp, modulus) * first_code) % modulus;
    answer!(code);
}

#[rustfmt::skip]
fn parse_row_column(s: &str) -> Result<(u64, u64)> {
    let (row, col) = nom!(do_parse!(Str(s),
        take_until_and_consume!("row") >> space >> row: uint!(u64) >>
        take_until_and_consume!("column") >> space >> col: uint!(u64) >>
        ((row, col))
    ))?;
    if row == 0 || col == 0 {
        bail!("row/column cannot be 0");
    }
    Ok((row, col))
}
