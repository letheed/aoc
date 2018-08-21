use self::{
    Cardinal::{East, North, South, West},
    Side::{Left, Right},
};
use crate::{parse::*, Date, Day, Puzzle, Result};
use fnv::FnvHashSet as HashSet;

const DATE: Date = Date::new(Day::D01, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

#[cfg_attr(feature = "cargo-clippy", allow(needless_pass_by_value))]
fn solve(input: String) -> Result {
    let steps = parse_steps(&input)?;
    let mut direction = North;
    let mut coord = Coord::default();
    let mut coords = HashSet::default();
    let mut coord_already_visited = None;
    for (side, n) in steps {
        match side {
            Left => direction = direction.turned_left(),
            Right => direction = direction.turned_right(),
        }
        if coord_already_visited.is_none() {
            coord_already_visited = insert_visited_coords(&mut coords, coord, direction, n);
        }
        coord = coord.stepped(direction, n);
    }
    answer!(coord.blocks_away(), coord_already_visited.unwrap_or_default().blocks_away());
}

type Step = (Side, i16);

#[derive(Copy, Clone)]
enum Side {
    Left,
    Right,
}

#[derive(Copy, Clone)]
enum Cardinal {
    North,
    East,
    South,
    West,
}

impl Cardinal {
    fn turned_left(self) -> Self {
        match self {
            North => West,
            East => North,
            South => East,
            West => South,
        }
    }

    fn turned_right(self) -> Self {
        match self {
            North => East,
            East => South,
            South => West,
            West => North,
        }
    }
}

#[derive(Copy, Clone, Default, PartialEq, Eq, Hash)]
struct Coord {
    x: i16,
    y: i16,
}

impl Coord {
    fn stepped(mut self, direction: Cardinal, steps: i16) -> Self {
        match direction {
            North => self.y += steps,
            East => self.x += steps,
            South => self.y -= steps,
            West => self.x -= steps,
        }
        self
    }

    fn blocks_away(self) -> i16 {
        self.x.abs() + self.y.abs()
    }
}

fn insert_visited_coords(
    coords: &mut HashSet<Coord>,
    mut coord: Coord,
    direction: Cardinal,
    steps: i16,
) -> Option<Coord>
{
    match direction {
        North => for _ in 0..steps {
            coord.y += 1;
            if !coords.insert(coord) {
                return Some(coord);
            }
        },
        East => for _ in 0..steps {
            coord.x += 1;
            if !coords.insert(coord) {
                return Some(coord);
            }
        },
        South => for _ in 0..steps {
            coord.y -= 1;
            if !coords.insert(coord) {
                return Some(coord);
            }
        },
        West => for _ in 0..steps {
            coord.x -= 1;
            if !coords.insert(coord) {
                return Some(coord);
            }
        },
    }
    None
}

#[rustfmt::skip]
fn parse_steps(s: &str) -> Result<Vec<Step>> {
    named!(instruction(Str<'_>) -> Step, do_parse!(
        side: alt!(char!('R') => { |_| Right } | char!('L') => { |_| Left }) >>
        n: uint!(i16) >> ((side, n))
    ));
    nom!(separated_nonempty_list_complete!(Str(s), tag!(", "), instruction))
}
