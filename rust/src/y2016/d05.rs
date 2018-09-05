use crate::{Date, Day, Puzzle, Result};
use std::collections::BTreeMap;

const DATE: Date = Date::new(Day::D05, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

#[allow(clippy::needless_pass_by_value)]
fn solve(input: String) -> Result {
    let mut password1 = String::with_capacity(8);
    let mut password2 = BTreeMap::new();
    let mut buffer = input.into_bytes();
    let start = buffer.len();
    buffer.push(b'0');
    while password2.len() < 8 {
        let mut hasher = md5::Md5::default();
        hasher.consume(&buffer);
        let digest = hasher.hash();
        if digest[..2] == [0, 0] && digest[2] < 0x10 {
            if password1.len() < 8 {
                let c = hex_digit(digest[2]);
                password1.push(c);
            }
            if digest[2] < 8 {
                password2.entry(digest[2]).or_insert(digest[3] >> 4);
            }
        }
        increment(&mut buffer, start);
    }
    let password2 = password2.values().map(|&b| hex_digit(b)).collect::<String>();
    answer!(password1, password2);
}

fn hex_digit(n: u8) -> char {
    if n < 10 {
        (n + b'0') as char
    } else {
        (n - 10 + b'a') as char
    }
}

fn increment(buffer: &mut Vec<u8>, start: usize) {
    for b in buffer[start..].iter_mut().rev() {
        if let b'9' = b {
            *b = b'0';
        } else {
            *b += 1;
            return;
        };
    }
    buffer.insert(start, b'1');
}
