use std::{convert::TryFrom, fmt::Write};

use anyhow::bail;

use self::Move::{Down, Left, Right, Up};
use crate::{Date, Day, Puzzle, Result};

const DATE: Date = Date::new(Day::D02, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

#[allow(clippy::needless_pass_by_value)]
fn solve(input: String) -> Result {
    let mut code1 = String::new();
    let mut code2 = String::new();
    let mut keypad1 = Keypad1::new();
    let mut keypad2 = Keypad2::new();
    for line in input.lines() {
        for c in line.chars() {
            let move_ = Move::try_from(c)?;
            keypad1 = keypad1.selected(move_);
            keypad2 = keypad2.selected(move_);
        }
        write!(code1, "{}", keypad1.button()?)?;
        write!(code2, "{}", keypad2.button()?)?;
    }
    answer!(code1, code2);
}

#[derive(Copy, Clone)]
enum Move {
    Up,
    Down,
    Right,
    Left,
}

impl TryFrom<char> for Move {
    type Error = anyhow::Error;

    fn try_from(c: char) -> Result<Self> {
        let dir = match c {
            'U' => Up,
            'D' => Down,
            'L' => Left,
            'R' => Right,
            _ => bail!("unexpected character '{}'", c),
        };
        Ok(dir)
    }
}

#[derive(Copy, Clone)]
struct Keypad1 {
    row: u8,
    col: u8,
}

impl Keypad1 {
    const fn new() -> Self {
        Self { row: 1, col: 1 }
    }

    const fn selected(mut self, move_: Move) -> Self {
        #[rustfmt::skip]
        match move_ {
            Up    if self.row != 0 => self.row -= 1,
            Down  if self.row < 2  => self.row += 1,
            Left  if self.col != 0 => self.col -= 1,
            Right if self.col < 2  => self.col += 1,
            _ => {}
        }
        self
    }

    fn button(self) -> Result<u8> {
        if self.row > 2 || self.col > 2 {
            bail!("Keypad1: no button for these coordinates")
        }
        Ok(self.row * 3 + self.col + 1)
    }
}

const PAD: char = '.';
const KEYPAD_MAP: [[char; 7]; 7] = [
    [PAD, PAD, PAD, PAD, PAD, PAD, PAD],
    [PAD, PAD, PAD, '1', PAD, PAD, PAD],
    [PAD, PAD, '2', '3', '4', PAD, PAD],
    [PAD, '5', '6', '7', '8', '9', PAD],
    [PAD, PAD, 'A', 'B', 'C', PAD, PAD],
    [PAD, PAD, PAD, 'D', PAD, PAD, PAD],
    [PAD, PAD, PAD, PAD, PAD, PAD, PAD],
];

#[derive(Copy, Clone)]
struct Keypad2 {
    row: usize,
    col: usize,
}

impl Keypad2 {
    const fn new() -> Self {
        Self { row: 3, col: 1 }
    }

    const fn selected(mut self, move_: Move) -> Self {
        let old_self = self;
        match move_ {
            Up => self.row -= 1,
            Down => self.row += 1,
            Left => self.col -= 1,
            Right => self.col += 1,
        }
        if KEYPAD_MAP[self.row][self.col] == PAD { old_self } else { self }
    }

    fn button(self) -> Result<char> {
        match KEYPAD_MAP[self.row][self.col] {
            PAD => bail!("Keypad2: no button for these coordinates"),
            c => Ok(c),
        }
    }
}
