use self::Instruction::{Rect, RotCol, RotRow};
use crate::{parse::*, Date, Day, OkOrFail, Puzzle, Result};
use fnv::FnvHashMap as HashMap;
use std::fmt::{self, Display, Write};

const DATE: Date = Date::new(Day::D08, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

#[cfg_attr(feature = "cargo-clippy", allow(needless_pass_by_value))]
fn solve(input: String) -> Result {
    let mut screen = Screen::default();
    for line in input.lines() {
        let instruction = parse_instruction(line)?;
        screen.apply(instruction);
    }
    let lit_pixels =
        screen.grid.iter().map(|row| row.iter().filter(|pix| **pix).count()).sum::<usize>();
    let font = FONT.iter().cloned().collect::<HashMap<_, _>>();
    let mut code = String::with_capacity(Screen::SEGMENTS);
    for i in 0..Screen::SEGMENTS {
        let encoding = screen.encode_segment(i).ok_or_fail("segment index out of bounds")?;
        let c = *font.get(&encoding).ok_or_fail("unknown glyph")?;
        code.push(c);
    }
    answer!(lit_pixels, code);
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum Instruction {
    Rect(usize, usize),
    RotRow(usize, usize),
    RotCol(usize, usize),
}

struct Screen {
    grid: [[bool; Screen::WIDTH]; Screen::HEIGHT],
}

impl Default for Screen {
    fn default() -> Self {
        Self { grid: [[false; 50]; 6] }
    }
}

impl Display for Screen {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for row in &self.grid {
            for &pix in row.iter() {
                f.write_char(if pix { '█' } else { ' ' })?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

impl Screen {
    const HEIGHT: usize = 6;
    const SEGMENTS: usize = 10;
    const SEGMENT_WIDTH: usize = 5;
    const WIDTH: usize = Self::SEGMENTS * Self::SEGMENT_WIDTH;

    fn apply(&mut self, instruction: Instruction) {
        match instruction {
            Rect(col, row) => {
                for row in &mut self.grid[..row] {
                    for pix in &mut row[..col] {
                        *pix = true;
                    }
                }
            }
            RotRow(row, mut n) => {
                if n >= Self::WIDTH {
                    n %= Self::WIDTH;
                }
                let mid = Self::WIDTH - n;
                let mut buffer = [false; Self::WIDTH];
                buffer.copy_from_slice(&self.grid[row]);
                self.grid[row][..n].copy_from_slice(&buffer[mid..]);
                self.grid[row][n..].copy_from_slice(&buffer[..mid]);
            }
            RotCol(col, mut n) => {
                if n >= Self::HEIGHT {
                    n %= Self::HEIGHT;
                }
                let mid = Self::HEIGHT - n;
                let mut buffer = [false; Self::HEIGHT];
                #[cfg_attr(feature = "cargo-clippy", allow(needless_range_loop))]
                for row in 0..Self::HEIGHT {
                    buffer[row] = self.grid[row][col];
                }
                for row in 0..n {
                    self.grid[row][col] = buffer[mid + row];
                }
                for row in n..Self::HEIGHT {
                    self.grid[row][col] = buffer[row - n];
                }
            }
        }
    }

    fn encode_segment(&self, i: usize) -> Option<u32> {
        if i >= Self::SEGMENTS {
            return None;
        }
        let seg_start = i * Self::SEGMENT_WIDTH;
        let seg_end = seg_start + Self::SEGMENT_WIDTH;
        let seg_enc = self
            .grid
            .iter()
            .flat_map(|row| row[seg_start..seg_end].iter())
            .fold(0, |n, &b| (n << 1) + u32::from(b));
        Some(seg_enc)
    }
}

#[rustfmt::skip]
const FONT: [(u32, char); 18] = [
    (0x1929_7A52, 'A'), (0x392E_4A5C, 'B'), (0x1928_424C, 'C'), (0x3D0E_421E, 'E'),
    (0x3D0E_4210, 'F'), (0x1928_5A4E, 'G'), (0x252F_4A52, 'H'), (0x1C42_108E, 'I'),
    (0x0C21_0A4C, 'J'), (0x254C_5292, 'K'), (0x2108_421E, 'L'), (0x1929_4A4C, 'O'),
    (0x3929_7210, 'P'), (0x3929_7292, 'R'), (0x1D08_305C, 'S'), (0x2529_4A4C, 'U'),
    (0x2315_1084, 'Y'), (0x3C22_221E, 'Z'),
];

#[rustfmt::skip]
fn parse_instruction(s: &str) -> Result<Instruction> {
    named!(rect(Str<'_>) -> Instruction, do_parse!(
        tag!("rect") >> space >>
        col: uint!(usize) >> char!('x') >>
        row: uint!(usize) >> (Rect(col, row))
    ));
    named!(rotrow(Str<'_>) -> Instruction, do_parse!(
        tag!("rotate") >> space >> tag!("row") >> space >> tag!("y=") >>
        row: uint!(usize) >> space >> tag!("by") >> space >>
        n: uint!(usize) >> (RotRow(row, n))
    ));
    named!(rotcol(Str<'_>) -> Instruction, do_parse!(
        tag!("rotate") >> space >> tag!("column") >> space >> tag!("x=") >>
        col: uint!(usize) >> space >> tag!("by") >> space >>
        n: uint!(usize) >> (RotCol(col, n))
    ));
    nom!(alt!(Str(s), rect | rotrow | rotcol))
}
