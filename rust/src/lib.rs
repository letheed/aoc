#![feature(const_fn, const_slice_len)]
#![feature(generators, generator_trait)]
#![feature(in_band_lifetimes)]
#![feature(nll)]
#![feature(stmt_expr_attributes)]
#![feature(tool_lints)]
#![feature(try_from)]
#![warn(rust_2018_idioms, rust_2018_compatibility)]
#![deny(clippy::correctness)]
#![warn(clippy::pedantic)]
#![warn(clippy::style)]
#![warn(clippy::complexity)]
#![warn(clippy::perf)]
#![allow(clippy::indexing_slicing)]
#![allow(clippy::similar_names)]
#![allow(clippy::double_parens)]

pub use crate::common::{
    Answer, Answers, Date, Day, Puzzle, Puzzles, Result, Solver, Year, YearPuzzles,
};
pub use failure::Error;

crate use crate::utils::OkOrFail;

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
