use crate::{Puzzle, Year, YearPuzzles};

pub const PUZZLES: YearPuzzles = YearPuzzles::new(YEAR, &PUZZLES_ARRAY);

const YEAR: Year = Year::Y2016;

const PUZZLES_ARRAY: [Option<Puzzle>; 25] = [
    Some(d01::PUZZLE),
    Some(d02::PUZZLE),
    Some(d03::PUZZLE),
    Some(d04::PUZZLE),
    Some(d05::PUZZLE),
    Some(d06::PUZZLE),
    Some(d07::PUZZLE),
    Some(d08::PUZZLE),
    Some(d09::PUZZLE),
    Some(d10::PUZZLE),
    None, // Some(d11::PUZZLE),
    None, // Some(d12::PUZZLE),
    None, // Some(d13::PUZZLE),
    None, // Some(d14::PUZZLE),
    None, // Some(d15::PUZZLE),
    None, // Some(d16::PUZZLE),
    None, // Some(d17::PUZZLE),
    None, // Some(d18::PUZZLE),
    None, // Some(d19::PUZZLE),
    None, // Some(d20::PUZZLE),
    None, // Some(d21::PUZZLE),
    None, // Some(d22::PUZZLE),
    None, // Some(d23::PUZZLE),
    None, // Some(d24::PUZZLE),
    None, // Some(d25::PUZZLE),
];

mod d01;
mod d02;
mod d03;
mod d04;
mod d05;
mod d06;
mod d07;
mod d08;
mod d09;
mod d10;
// mod d11;
// mod d12;
// mod d13;
// mod d14;
// mod d15;
// mod d16;
// mod d17;
// mod d18;
// mod d19;
// mod d20;
// mod d21;
// mod d22;
// mod d23;
// mod d24;
// mod d25;
