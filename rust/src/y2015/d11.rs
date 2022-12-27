use std::convert::TryFrom;

use failure::bail;

use crate::{Date, Day, Puzzle, Result};

const DATE: Date = Date::new(Day::D11, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

#[allow(clippy::needless_pass_by_value)]
fn solve(input: String) -> Result {
    let mut password = Password::try_from(input)?;
    password.next();
    let password1 = String::try_from(password.clone())?;
    password.next();
    let password2 = String::try_from(password)?;
    answer!(password1, password2);
}

#[derive(Clone)]
struct Password {
    bytes: Vec<u8>,
}

impl TryFrom<String> for Password {
    type Error = failure::Error;

    fn try_from(string: String) -> Result<Self> {
        let bytes = string.into_bytes();
        if bytes.iter().copied().all(|b| b.is_ascii_lowercase()) {
            Ok(Self { bytes })
        } else {
            bail!("password uses letters other than a..z");
        }
    }
}

impl TryFrom<Password> for String {
    type Error = failure::Error;

    fn try_from(password: Password) -> Result<Self> {
        let Password { bytes } = password;
        Ok(Self::from_utf8(bytes)?)
    }
}

impl Password {
    fn is_valid(&self) -> bool {
        if !self.bytes.windows(3).any(|bs| bs[0] + 1 == bs[1] && bs[1] + 1 == bs[2]) {
            return false;
        }
        if self.bytes.iter().any(|b| b"iol".contains(b)) {
            return false;
        }
        let mut pairs = self.bytes.windows(2);
        let mut n = 0;
        while let Some(pair) = pairs.next() {
            if pair[0] == pair[1] {
                if n == 1 {
                    return true;
                }
                n += 1;
                pairs.next();
            }
        }
        false
    }

    fn next(&mut self) {
        self.increment();
        while !self.is_valid() {
            self.increment();
        }
    }

    fn increment(&mut self) {
        for b in self.bytes.iter_mut().rev() {
            if b == &b'z' {
                *b = b'a';
            } else {
                *b += 1;
                break;
            };
        }
    }
}
