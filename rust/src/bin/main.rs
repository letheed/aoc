#![feature(drain_filter)]
#![feature(in_band_lifetimes)]
#![feature(stmt_expr_attributes)]
#![feature(tool_attributes)]
#![warn(rust_2018_idioms, rust_2018_compatibility)]
#![cfg_attr(feature = "cargo-clippy", deny(clippy_correctness))]
#![cfg_attr(feature = "cargo-clippy", warn(clippy_pedantic))]
#![cfg_attr(feature = "cargo-clippy", warn(clippy_style))]
#![cfg_attr(feature = "cargo-clippy", warn(clippy_complexity))]
#![cfg_attr(feature = "cargo-clippy", warn(clippy_perf))]
#![cfg_attr(feature = "cargo-clippy", allow(indexing_slicing))]
#![cfg_attr(feature = "cargo-clippy", allow(similar_names))]
#![cfg_attr(feature = "cargo-clippy", allow(non_ascii_literal))]

use ansi_term::{
    Color::{Green, Red},
    Style,
};
use aoc::{nom, uint, Answers, Day, Puzzle, Puzzles, Year, YearPuzzles};
use failure::{format_err, Error};
use fnv::FnvHashMap as HashMap;
use nom::{types::CompleteStr as Str, *};
use num_traits::FromPrimitive;
use std::{
    fmt::{self, Display},
    time::Duration,
};

macro_rules! color_test_result {
    ($res:expr, $sol:expr) => {
        if $res == $sol {
            Green.paint($res)
        } else {
            Red.paint($res)
        }
    };
}

fn main() {
    let args = std::env::args().skip(1).collect::<Vec<_>>();
    if args.is_empty() {
        aoc::PUZZLES.present();
    } else {
        match args.iter().map(|s| parse_date_arg(s)).collect::<Result<Vec<_>, _>>() {
            Ok(date_args) => group_by_year(date_args).present(),
            Err(err) => eprintln!("{}", err),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
struct DateArg {
    year: Year,
    day_arg: DayArg,
}

#[derive(Clone, PartialEq, Eq)]
enum DayArg {
    All,
    Day(Day),
    DayList(Vec<Day>),
    DayRange(Day, Day),
}

fn parse_date_arg(s: &str) -> Result<DateArg, Error> {
    #[rustfmt::skip]
    named!(year(Str<'_>) -> Year, do_parse!(
        n: uint!(u16) >>
        year: alt!(expr_opt!(Year::from_u16(n)) | expr_opt!(Year::from_u16(2000 + n))) >> (year)
    ));
    #[rustfmt::skip]
    named!(day(Str<'_>) -> Day, do_parse!(
        n: uint!(u8) >> day: expr_opt!(Day::from_u8(n)) >> (day)
    ));
    fn day_arg(s: Str<'_>) -> IResult<Str<'_>, DayArg> {
        use nom::{Context::Code, Err::Failure, ErrorKind};

        if s.input_len() == 0 {
            return Ok((s, DayArg::All));
        }
        let (s_day1, _) = char!(s, '-')?;
        let (rest_d1, day1) = try_parse!(s_day1, day);
        if rest_d1.input_len() == 0 {
            return Ok((rest_d1, DayArg::Day(day1)));
        }
        let (rest_sep, sep) = alt!(rest_d1, tag!("..") | tag!(","))?;
        let (rest, day_arg) = match *sep {
            ".." => {
                let (rest, day2) = try_parse!(rest_sep, day);
                (rest, DayArg::DayRange(day1, day2))
            }
            "," => {
                let (rest, mut days) =
                    separated_nonempty_list_complete!(rest_sep, char!(','), day)?;
                days.insert(0, day1);
                (rest, DayArg::DayList(days))
            }
            _ => unreachable!(r#"separator must be ".." or ",""#),
        };
        if rest.input_len() == 0 {
            Ok((rest, day_arg))
        } else {
            Err(Failure(Code(rest, ErrorKind::NonEmpty)))
        }
    }
    #[rustfmt::skip]
    nom!(do_parse!(
        Str(s),
        year: year >> day_arg: day_arg >>
        (DateArg { year, day_arg })
    ))
}

fn group_by_year(mut date_args: Vec<DateArg>) -> HashMap<Year, Vec<DayArg>> {
    let mut year_map = HashMap::default();
    let mut years = date_args.iter().map(|date_arg| date_arg.year).collect::<Vec<_>>();
    years.sort_unstable();
    years.dedup();
    for year in years {
        let day_args = date_args
            .drain_filter(|date_arg| date_arg.year == year)
            .map(|date_arg| date_arg.day_arg)
            .collect::<Vec<_>>();
        year_map.insert(year, day_args);
    }
    year_map
}

fn not_solved(year: Year, day: Option<Day>) {
    if let Some(day) = day {
        println!("{}-{} {} {}", year, day, RunDuration(None), "puzzle not solved");
    } else {
        println!("{}    {} {}", year, RunDuration(None), "puzzles not solved");
    }
}

trait Present {
    fn present(self);
}

impl Present for &'a Puzzles {
    fn present(self) {
        let mut year_puzzles_iter = self.into_iter();
        if let Some(year_puzzles) = year_puzzles_iter.next() {
            year_puzzles.present();
            for year_puzzles in year_puzzles_iter {
                println!();
                year_puzzles.present();
            }
        }
    }
}

impl Present for &'a YearPuzzles {
    fn present(self) {
        for puzzle in self {
            puzzle.present();
        }
    }
}

impl<T> Present for &'a HashMap<Year, T>
where T: AsRef<[DayArg]>
{
    fn present(self) {
        let mut year_map_iter =
            self.into_iter().filter(|(_, day_args)| !day_args.as_ref().is_empty());
        if let Some((&year, day_args)) = year_map_iter.next() {
            (year, day_args.as_ref()).present();
            for (&year, day_args) in year_map_iter {
                println!();
                (year, day_args.as_ref()).present();
            }
        }
    }
}

impl Present for (Year, &'a [DayArg]) {
    fn present(self) {
        let (year, day_args) = self;
        if day_args.is_empty() {
            return;
        }
        let mut days = Vec::new();
        for day_arg in day_args {
            match *day_arg {
                DayArg::All => if let Some(year_puzzles) = aoc::PUZZLES.get(year) {
                    days.extend(year_puzzles.into_iter().map(|puzzle| puzzle.date().day()));
                },
                DayArg::Day(day) => days.push(day),
                DayArg::DayList(ref day_list) => days.extend_from_slice(&day_list),
                DayArg::DayRange(start, end) => days.extend(
                    (start as u8..=end as u8)
                        .map(|n| Day::from_u8(n).expect("day cannot be out of range")),
                ),
            }
        }
        if days.is_empty() && day_args.contains(&DayArg::All) {
            not_solved(year, None);
            return;
        }
        days.sort_unstable();
        days.dedup();
        (year, &*days).present();
    }
}

// impl Present for Year {
//     fn present(self) {
//         if let Some(year_puzzles) = aoc::PUZZLES.get(self) {
//             year_puzzles.present()
//         } else {
//             not_solved(year, None);
//         }
//     }
// }

impl Present for (Year, &'a [Day]) {
    fn present(self) {
        let (year, days) = self;
        if days.is_empty() {
            return;
        }
        if let Some(year_puzzles) = aoc::PUZZLES.get(year) {
            for &day in days {
                if let Some(puzzle) = year_puzzles.get(day) {
                    puzzle.present();
                } else {
                    not_solved(year_puzzles.year(), Some(day));
                }
            }
        } else {
            not_solved(year, None);
        }
    }
}

impl Present for &'a Puzzle {
    fn present(self) {
        let (dur, res, sol) = match self.solve() {
            Ok((Answers::None, _)) => (None, Ok(Answers::None), Ok(Answers::None)),
            Ok((result, duration)) => (Some(duration), Ok(result), self.read_solution()),
            Err(err) => (None, Err(err), Ok(Answers::None)),
        };
        println!("{} {} {}", self.date(), RunDuration(dur), RunReport(res, sol));
    }
}

struct RunDuration(Option<Duration>);

impl Display for RunDuration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let duration = match &self.0 {
            Some(dur) => dur,
            None => return write!(f, "          "),
        };
        let dur_secs = duration.as_secs();
        let dur_ms = duration.subsec_millis();
        if dur_secs == 0 {
            let dur_us = duration.subsec_micros();
            if dur_us < 1000 {
                write!(f, "{:7} μs", dur_us)?;
            } else {
                write!(f, "{:3}.{:03} ms", dur_us / 1000, dur_us % 1000)?;
            }
        } else {
            let bold = Style::default().bold();
            write!(f, "{}{:3}.{:03} s{} ", bold.prefix(), dur_secs, dur_ms, bold.suffix())?;
        }
        Ok(())
    }
}

struct RunReport(aoc::Result<Answers>, aoc::Result<Answers>);

#[cfg_attr(feature = "cargo-clippy", allow(if_not_else))]
impl Display for RunReport {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let result = match &self.0 {
            Ok(res) => res,
            Err(err) => return write!(f, "{}", Red.paint(err.to_string())),
        };
        let solution = match &self.1 {
            Ok(sol) => sol,
            Err(_) => &Answers::None,
        };
        match result {
            Answers::None => write!(f, "no result")?,
            Answers::One(res) => match solution {
                Answers::None => {
                    write!(f, "{}", res)?;
                }
                Answers::One(sol) | Answers::Two(sol, _) => {
                    write!(f, "{}", color_test_result!(res, sol))?;
                    if res != sol {
                        write!(f, " solution = {}", sol)?;
                    }
                }
            },
            Answers::Two(res1, res2) => match solution {
                Answers::None => {
                    write!(f, "({}, {})", res1, res2)?;
                }
                Answers::One(sol) => {
                    write!(f, "({}, {})", color_test_result!(res1, sol), res2)?;
                    if res1 != sol {
                        write!(f, " solution = {}", sol)?;
                    }
                }
                Answers::Two(sol1, sol2) => {
                    let res1_colored = color_test_result!(res1, sol1);
                    let res2_colored = color_test_result!(res2, sol2);
                    write!(f, "({}, {})", res1_colored, res2_colored)?;
                    if res1 != sol1 {
                        if res2 != sol2 {
                            write!(f, " solution = ({}, {})", sol1, sol2)?;
                        } else {
                            write!(f, " solution = {}", sol1)?;
                        }
                    } else if res2 != sol2 {
                        write!(f, " solution = {}", sol2)?;
                    }
                }
            },
        }
        if let Err(err) = &self.1 {
            write!(f, " {}", Red.paint(err.to_string()))?;
        }
        Ok(())
    }
}
