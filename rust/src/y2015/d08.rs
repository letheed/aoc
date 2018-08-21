use crate::{Date, Day, Puzzle, Result};

const DATE: Date = Date::new(Day::D08, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

#[cfg_attr(feature = "cargo-clippy", allow(needless_pass_by_value))]
fn solve(input: String) -> Result {
    let (mut n1, mut n2) = (0, 0);
    for line in input.lines() {
        n1 += decoded_length_decrease(line);
        n2 += encoded_length_increase(line);
    }
    answer!(n1, n2);
}

fn decoded_length_decrease(s: &str) -> usize {
    let mut n = 2;
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            if let Some(c) = chars.next() {
                match c {
                    '\\' | '"' => n += 1,
                    'x' => {
                        n += 3;
                        chars.next();
                        chars.next();
                    }
                    _ => {}
                }
            }
        }
    }
    n
}

fn encoded_length_increase(s: &str) -> usize {
    2 + s.chars().filter(|&c| c == '\\' || c == '"' || c == '\n').count()
}
