use failure::bail;

use crate::{Date, Day, Puzzle, Result};

const DATE: Date = Date::new(Day::D10, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

#[allow(clippy::needless_pass_by_value)]
fn solve(input: String) -> Result {
    let mut digits = parse_digits(&input)?;
    let mut new_digits = Vec::with_capacity(digits.len());
    for _ in 0..40 {
        look_and_say(&digits, &mut new_digits);
        std::mem::swap(&mut digits, &mut new_digits);
    }
    let n1 = digits.len();
    for _ in 0..10 {
        look_and_say(&digits, &mut new_digits);
        std::mem::swap(&mut digits, &mut new_digits);
    }
    let n2 = digits.len();
    answer!(n1, n2);
}

fn look_and_say(digits: &[u8], new_digits: &mut Vec<u8>) {
    new_digits.clear();
    if let Some((&c, digits)) = digits.split_first() {
        let (mut n, mut current) = (1, c);
        for &d in digits {
            if d == current {
                n += 1;
            } else {
                new_digits.push(n);
                new_digits.push(current);
                current = d;
                n = 1;
            }
        }
        new_digits.push(n);
        new_digits.push(current);
    }
}

fn parse_digits(s: &str) -> Result<Vec<u8>> {
    s.chars()
        .map(|c| {
            c.to_digit(10).map_or_else(
                || bail!("unexpected character '{}' not a digit", c),
                #[allow(clippy::cast_possible_truncation)]
                |d| Ok(d as u8),
            )
        })
        .collect::<Result<Vec<u8>>>()
}
