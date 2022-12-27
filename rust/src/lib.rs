#![feature(generators, generator_trait)]
#![feature(stmt_expr_attributes)]
#![warn(clippy::pedantic)]
#![warn(clippy::cargo)]
#![warn(clippy::nursery)]
#![allow(clippy::similar_names)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::wildcard_imports)]

pub use failure::Error;

pub use crate::common::{Answer, Answers, Date, Day, Puzzle, Puzzles, Result, Solver, Year, YearPuzzles};
pub(crate) use crate::utils::OkOrFail;

pub const PUZZLES: Puzzles = Puzzles::new(PUZZLES_ARRAY);

#[rustfmt::skip]
const PUZZLES_ARRAY: &[YearPuzzles] = &[
    y2015::PUZZLES,
    y2016::PUZZLES,
];

#[macro_use]
mod macros;
#[macro_use]
mod parse;
mod common;
mod utils;
mod y2015;
mod y2016;
