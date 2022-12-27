use crate::{Puzzle, Year, YearPuzzles};

pub const PUZZLES: YearPuzzles = YearPuzzles::new(YEAR, &PUZZLES_ARRAY);

const YEAR: Year = Year::Y2015;

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
    Some(d11::PUZZLE),
    Some(d12::PUZZLE),
    Some(d13::PUZZLE),
    Some(d14::PUZZLE),
    Some(d15::PUZZLE),
    Some(d16::PUZZLE),
    Some(d17::PUZZLE),
    Some(d18::PUZZLE),
    Some(d19::PUZZLE),
    Some(d20::PUZZLE),
    Some(d21::PUZZLE),
    Some(d22::PUZZLE),
    Some(d23::PUZZLE),
    Some(d24::PUZZLE),
    Some(d25::PUZZLE),
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
mod d11;
mod d12;
mod d13;
mod d14;
mod d15;
mod d16;
mod d17;
mod d18;
mod d19;
mod d20;
mod d21;
mod d22;
mod d23;
mod d24;
mod d25;
