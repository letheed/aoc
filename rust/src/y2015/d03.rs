use failure::bail;

use crate::{Date, Day, Puzzle, Result};

const DATE: Date = Date::new(Day::D03, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

#[allow(clippy::needless_pass_by_value)]
fn solve(input: String) -> Result {
    let moves = parse_moves(&input)?;
    let mut houses = houses_solo(&moves);
    houses.sort_unstable();
    houses.dedup();
    let count_solo = houses.len();
    houses.clear();
    let (mut pos0, mut pos1) = ((0, 0), (0, 0));
    for mov in moves.chunks(2) {
        pos0.0 += mov[0].0;
        pos0.1 += mov[0].1;
        pos1.0 += mov[1].0;
        pos1.1 += mov[1].1;
        houses.push(pos0);
        houses.push(pos1);
    }
    houses.sort_unstable();
    houses.dedup();
    let count_duo = houses.len();
    answer!(count_solo, count_duo);
}

type Coord = (i32, i32);

fn parse_moves(s: &str) -> Result<Vec<Coord>> {
    s.chars().map(parse_move).collect()
}

fn parse_move(c: char) -> Result<Coord> {
    match c {
        '>' => Ok((1, 0)),
        '<' => Ok((-1, 0)),
        '^' => Ok((0, 1)),
        'v' => Ok((0, -1)),
        _ => bail!("unexpected character '{}'", c),
    }
}

fn houses_solo(moves: &[Coord]) -> Vec<Coord> {
    moves
        .iter()
        .scan((0, 0), |pos, mov| {
            pos.0 += mov.0;
            pos.1 += mov.1;
            Some(*pos)
        })
        .collect()
}
