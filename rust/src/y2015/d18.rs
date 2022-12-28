use std::{
    ops::{Index, IndexMut},
    str::FromStr,
};

use anyhow::bail;

use crate::{Date, Day, Puzzle, Result};

const DATE: Date = Date::new(Day::D18, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

const SIZE: usize = 100;

#[allow(clippy::needless_pass_by_value)]
fn solve(input: String) -> Result {
    let mut grid1 = input.parse::<Grid>()?;
    let mut grid2 = grid1.clone();
    for _ in 0..100 {
        grid1.next();
    }
    grid2.keep_corners_alive();
    for _ in 0..100 {
        grid2.next();
        grid2.keep_corners_alive();
    }
    let n1 = grid1.cells.iter().filter(|c| c.alive).count();
    let n2 = grid2.cells.iter().filter(|c| c.alive).count();
    answer!(n1, n2);
}

#[derive(Clone)]
struct Cell {
    alive: bool,
    lives: bool,
}

impl Cell {
    #[inline]
    const fn new_alive() -> Self {
        Self { alive: true, lives: true }
    }

    #[inline]
    const fn new_dead() -> Self {
        Self { alive: false, lives: false }
    }
}

#[derive(Clone)]
struct Grid {
    cells: Box<[Cell]>,
}

impl Index<(usize, usize)> for Grid {
    type Output = Cell;

    #[inline]
    fn index(&self, (i, j): (usize, usize)) -> &Self::Output {
        &self.cells[i * SIZE + j]
    }
}

impl IndexMut<(usize, usize)> for Grid {
    #[inline]
    fn index_mut(&mut self, (i, j): (usize, usize)) -> &mut Self::Output {
        &mut self.cells[i * SIZE + j]
    }
}

impl FromStr for Grid {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        let lines = s.lines().collect::<Vec<_>>();
        if lines.len() != SIZE || lines.iter().any(|line| line.len() != SIZE) {
            bail!("grid is not {}x{}", SIZE, SIZE);
        }
        let mut grid = Vec::with_capacity(SIZE * SIZE);
        for line in lines {
            for c in line.chars() {
                match c {
                    '#' => grid.push(Cell::new_alive()),
                    '.' => grid.push(Cell::new_dead()),
                    _ => bail!("unexpected character '{}'", c),
                }
            }
        }
        Ok(Self { cells: grid.into_boxed_slice() })
    }
}

impl Grid {
    #[inline]
    fn keep_corners_alive(&mut self) {
        self[(0, 0)] = Cell::new_alive();
        self[(0, SIZE - 1)] = Cell::new_alive();
        self[(SIZE - 1, 0)] = Cell::new_alive();
        self[(SIZE - 1, SIZE - 1)] = Cell::new_alive();
    }

    fn next(&mut self) {
        use std::cmp::min;

        for i in 0..SIZE {
            for j in 0..SIZE {
                let mut live_neighbours = 0_u8;
                for m in i.saturating_sub(1)..min(i + 2, SIZE) {
                    for n in j.saturating_sub(1)..min(j + 2, SIZE) {
                        if self[(m, n)].alive && (m != i || n != j) {
                            live_neighbours += 1;
                        }
                    }
                }
                let cell = &mut self[(i, j)];
                if !cell.alive {
                    if live_neighbours == 3 {
                        cell.lives = true;
                    }
                } else if live_neighbours != 2 && live_neighbours != 3 {
                    cell.lives = false;
                }
            }
        }
        for cell in &mut *self.cells {
            cell.alive = cell.lives;
        }
    }
}
