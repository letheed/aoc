use self::Action::{Toggle, TurnOff, TurnOn};
use crate::{parse::*, Date, Day, Puzzle, Result};
use std::{
    cmp::{max, min},
    str::FromStr,
};

const DATE: Date = Date::new(Day::D06, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

const SIZE: usize = 1000;

#[allow(clippy::needless_pass_by_value)]
fn solve(input: String) -> Result {
    let instructions = parse_instructions(&input)?;
    let mut grid = Grid::default();
    for instruction in instructions {
        grid.apply(instruction);
    }
    let lit = grid.lit.iter().filter(|&&lit| lit).count();
    let brightness = grid.brightness.iter().sum::<u32>();
    answer!(lit, brightness);
}

fn parse_instructions(s: &str) -> Result<Vec<Instruction>> {
    s.lines().map(|line| line.parse()).collect()
}

enum Action {
    TurnOff,
    Toggle,
    TurnOn,
}

struct Coord {
    row: usize,
    col: usize,
}

struct Instruction {
    action: Action,
    start: Coord,
    end: Coord,
}

impl FromStr for Instruction {
    type Err = failure::Error;

    #[rustfmt::skip]
    fn from_str(s: &str) -> Result<Self> {
        named!(action(Str<'_>) -> Action, alt!(
            tag!("turn off") => { |_| TurnOff } |
            tag!("toggle")   => { |_| Toggle  } |
            tag!("turn on")  => { |_| TurnOn  }
        ));
        named!(coord(Str<'_>) -> Coord, do_parse!(
            col: uint!(usize) >> tag!(",") >>
            row: uint!(usize) >>
            (Coord { row, col })
        ));
        nom!(
            sep!(Str(s), space0, do_parse!(
                action: action >>
                start: coord >> tag!("through") >>
                end: coord >>
                ({
                    let Coord { row: row1, col: col1 } = start;
                    let Coord { row: row2, col: col2 } = end;
                    let start = Coord {
                        row: min(row1, row2),
                        col: min(col1, col2),
                    };
                    let end = Coord {
                        row: max(row1, row2),
                        col: max(col1, col2),
                    };
                    Self { action, start, end }
                })
            ))
        )
    }
}

struct Grid {
    lit: Box<[bool]>,
    brightness: Box<[u32]>,
}

impl Default for Grid {
    fn default() -> Self {
        Self {
            lit: vec![false; SIZE * SIZE].into_boxed_slice(),
            brightness: vec![0; SIZE * SIZE].into_boxed_slice(),
        }
    }
}

impl Grid {
    fn apply(&mut self, instruction: Instruction) {
        let start = instruction.start;
        let end = instruction.end;
        for row in start.row..=end.row {
            let i = row * SIZE + start.col;
            let j = row * SIZE + end.col;
            for lit in &mut self.lit[i..=j] {
                match instruction.action {
                    TurnOff => *lit = false,
                    Toggle => *lit = !*lit,
                    TurnOn => *lit = true,
                }
            }
            for brightness in &mut self.brightness[i..=j] {
                match instruction.action {
                    TurnOff => *brightness = brightness.saturating_sub(1),
                    Toggle => *brightness += 2,
                    TurnOn => *brightness += 1,
                }
            }
        }
    }
}
