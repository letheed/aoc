use crate::{Date, Day, Puzzle, Result};

const DATE: Date = Date::new(Day::D05, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

#[allow(clippy::needless_pass_by_value)]
fn solve(input: String) -> Result {
    let (mut n1, mut n2) = (0, 0);
    for string in input.lines() {
        if is_nice1(string) {
            n1 += 1;
        }
        if is_nice2(string.as_bytes()) {
            n2 += 1;
        }
    }
    answer!(n1, n2);
}

fn is_nice1(string: &str) -> bool {
    let vowels = b"aeiou";
    if string.bytes().filter(|b| vowels.contains(b)).count() < 3 {
        return false;
    }
    let mut nice = false;
    let forbidden: [&[u8]; 4] = [b"ab", b"cd", b"pq", b"xy"];
    for pair in string.as_bytes().windows(2) {
        if forbidden.contains(&pair) {
            return false;
        }
        if pair[0] == pair[1] {
            nice = true
        }
    }
    nice
}

#[rustfmt::skip]
fn is_nice2(string: &[u8]) -> bool {
        string.windows(3).any(|bs| bs[0] == bs[2])
    &&  string[..string.len() - 2].windows(2).enumerate()
          .any(|(i, pair1)| string[i + 2..].windows(2).any(|pair2| pair1 == pair2))
}
