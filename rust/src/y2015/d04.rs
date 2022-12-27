use md5::{Digest, Md5};

use crate::{Date, Day, Puzzle, Result};

const DATE: Date = Date::new(Day::D04, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

#[allow(clippy::needless_pass_by_value)]
fn solve(input: String) -> Result {
    let mut buffer = input.into_bytes();
    let start = buffer.len();
    buffer.push(b'0');
    let mut hasher = Md5::new();
    let n5 = loop {
        hasher.update(&buffer);
        let digest = hasher.finalize_reset();
        if digest[..2] == [0, 0] && digest[2] < 0x10 {
            break String::from_utf8(buffer[start..].to_owned())?;
        }
        increment(&mut buffer, start);
    };
    let n6 = loop {
        hasher.update(&buffer);
        let digest = hasher.finalize_reset();
        if digest[..3] == [0, 0, 0] {
            break std::str::from_utf8(&buffer[start..])?;
        }
        increment(&mut buffer, start);
    };
    answer!(n5, n6);
}

fn increment(buffer: &mut Vec<u8>, start: usize) {
    for b in buffer[start..].iter_mut().rev() {
        if b == &b'9' {
            *b = b'0';
        } else {
            *b += 1;
            return;
        };
    }
    buffer.insert(start, b'1');
}
