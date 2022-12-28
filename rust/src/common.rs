use std::result::Result as StdResult;

use lazy_static::lazy_static;

pub use self::{date::Date, day::Day, puzzle::Puzzle, puzzles::Puzzles, year::Year, year_puzzles::YearPuzzles};

pub type Answer = String;
pub type Result<T = Answers> = StdResult<T, anyhow::Error>;
pub type Solver = fn(String) -> Result;

lazy_static! {
    static ref PUZZLE_DIR: String = {
        let output = std::process::Command::new("git")
            .arg("rev-parse")
            .arg("--show-toplevel")
            .stderr(std::process::Stdio::null())
            .output()
            .expect("failed to run git command");
        assert!(output.status.success(), "git command exit status is not success");
        let mut root_dir = String::from_utf8(output.stdout).expect("git command output is not valid utf8");
        if root_dir.ends_with('\n') {
            root_dir.pop();
        }
        root_dir.push_str("/puzzles");
        root_dir
    };
}

pub enum Answers {
    None,
    One(Answer),
    Two(Answer, Answer),
}

mod day {
    use std::fmt::{self, Display};

    use num_derive::FromPrimitive;

    #[rustfmt::skip]
    const DAY_STR: [&str; 25] = [
        "01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
        "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
        "21", "22", "23", "24", "25",
    ];

    #[rustfmt::skip]
    #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Hash, FromPrimitive)]
    pub enum Day {
        D01 = 1, D02, D03, D04, D05, D06, D07, D08, D09, D10,
        D11, D12, D13, D14, D15, D16, D17, D18, D19, D20,
        D21, D22, D23, D24, D25
    }

    impl Display for Day {
        #[inline]
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.write_str(self.as_str())
        }
    }

    impl Day {
        #[inline]
        #[must_use]
        pub const fn as_str(self) -> &'static str {
            DAY_STR[self.as_index()]
        }

        #[inline]
        #[must_use]
        pub const fn as_index(self) -> usize {
            self as usize - Self::D01 as usize
        }
    }
}

mod year {
    use std::fmt::{self, Display};

    use num_derive::FromPrimitive;

    const YEAR_STR: [&str; 3] = ["2015", "2016", "2017"];

    #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Hash, FromPrimitive)]
    pub enum Year {
        Y2015 = 2015,
        Y2016,
        Y2017,
    }

    impl Display for Year {
        #[inline]
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.write_str(self.as_str())
        }
    }

    impl Year {
        #[inline]
        #[must_use]
        pub const fn as_str(self) -> &'static str {
            YEAR_STR[self.as_index()]
        }

        #[inline]
        #[must_use]
        pub const fn as_index(self) -> usize {
            self as usize - Self::Y2015 as usize
        }
    }
}

mod date {
    use std::fmt::{self, Display};

    use super::{Day, Year};

    #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Hash)]
    pub struct Date {
        year: Year,
        day: Day,
    }

    impl Display for Date {
        #[inline]
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}-{}", self.year, self.day)
        }
    }

    impl Date {
        #[inline]
        pub(crate) const fn new(day: Day, year: Year) -> Self {
            Self { year, day }
        }

        #[inline]
        #[must_use]
        pub const fn day(self) -> Day {
            self.day
        }

        #[inline]
        #[must_use]
        pub const fn year(self) -> Year {
            self.year
        }
    }
}

mod puzzle {
    use std::{
        fmt::Write,
        time::{Duration, Instant},
    };

    use anyhow::bail;

    use super::{Answers, Date, Result, Solver, PUZZLE_DIR};

    #[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
    pub struct Puzzle {
        date: Date,
        solver: Solver,
        input_filename: &'static str,
        solution_filename: Option<&'static str>,
    }

    impl Puzzle {
        #[inline]
        pub(crate) const fn new(date: Date, solver: Solver) -> Self {
            Self { date, solver, input_filename: "input", solution_filename: Some("solution") }
        }

        #[inline]
        #[allow(dead_code)]
        pub(crate) const fn example(date: Date, solver: Solver) -> Self {
            Self { date, solver, input_filename: "example", solution_filename: Some("example solution") }
        }

        #[inline]
        #[allow(dead_code)]
        pub(crate) const fn no_solution(date: Date, solver: Solver) -> Self {
            Self { date, solver, input_filename: "input", solution_filename: None }
        }

        #[inline]
        #[must_use]
        pub const fn date(&self) -> Date {
            self.date
        }

        pub(crate) fn read_to_string(&self, filename: &str) -> Result<String> {
            let mut path = PUZZLE_DIR.clone();
            write!(path, "/{}/{}/{}", self.date.year(), self.date.day(), filename)?;
            Ok(read_to_string!(&path)?)
        }

        pub fn solve(&self) -> Result<(Answers, Duration)> {
            let input = self.read_to_string(self.input_filename)?;
            let t = Instant::now();
            let answers = (self.solver)(input)?;
            let duration = t.elapsed();
            Ok((answers, duration))
        }

        pub fn read_solution(&self) -> Result<Answers> {
            let Some(solution_filename) = self.solution_filename else {
                return Ok(Answers::None);
            };
            let mut path = PUZZLE_DIR.clone();
            write!(path, "/{}/{}/{}", self.date.year(), self.date.day(), solution_filename)?;
            let solutions = read_to_string!(&path)?;
            let mut lines = solutions.lines().filter(|line| !line.is_empty());
            let sol1 = lines.next();
            let sol2 = lines.next();
            if lines.next().is_some() {
                bail!("too many solutions in {}", solution_filename);
            }
            match (sol1, sol2) {
                (Some(sol1), Some(sol2)) => Ok(Answers::Two(sol1.to_owned(), sol2.to_owned())),
                (Some(sol), None) => Ok(Answers::One(sol.to_owned())),
                (None, _) => Ok(Answers::None),
            }
        }
    }
}

mod year_puzzles {
    use super::{Day, Puzzle, Year};

    #[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
    pub struct YearPuzzles {
        year: Year,
        puzzles: &'static [Option<Puzzle>; 25],
    }

    impl YearPuzzles {
        #[inline]
        pub(crate) const fn new(year: Year, puzzles: &'static [Option<Puzzle>; 25]) -> Self {
            Self { year, puzzles }
        }

        #[inline]
        #[must_use]
        pub const fn year(self) -> Year {
            self.year
        }

        #[inline]
        #[must_use]
        pub const fn len(self) -> usize {
            self.puzzles.len()
        }

        #[inline]
        #[must_use]
        pub const fn is_empty(self) -> bool {
            self.puzzles.is_empty()
        }

        #[inline]
        #[must_use]
        pub const fn get(self, day: Day) -> Option<Puzzle> {
            self.puzzles[day.as_index()]
        }
    }

    impl<'a> IntoIterator for &'a YearPuzzles {
        type IntoIter = Iter<'a>;
        type Item = &'a Puzzle;

        #[inline]
        fn into_iter(self) -> Self::IntoIter {
            Iter { iter: self.puzzles.iter().flatten() }
        }
    }

    pub struct Iter<'a> {
        iter: std::iter::Flatten<std::slice::Iter<'a, Option<Puzzle>>>,
    }

    impl<'a> Iterator for Iter<'a> {
        type Item = &'a Puzzle;

        #[inline]
        fn next(&mut self) -> Option<Self::Item> {
            self.iter.next()
        }
    }
}

mod puzzles {
    use super::{Year, YearPuzzles};

    #[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
    pub struct Puzzles {
        year_puzzles: &'static [YearPuzzles],
    }

    impl Puzzles {
        #[inline]
        pub(crate) const fn new(year_puzzles: &'static [YearPuzzles]) -> Self {
            Self { year_puzzles }
        }

        #[inline]
        #[must_use]
        pub const fn len(self) -> usize {
            self.year_puzzles.len()
        }

        #[inline]
        #[must_use]
        pub const fn is_empty(self) -> bool {
            self.year_puzzles.is_empty()
        }

        #[inline]
        #[must_use]
        pub fn get(self, year: Year) -> Option<&'static YearPuzzles> {
            self.year_puzzles.get(year.as_index())
        }
    }

    impl<'a> IntoIterator for &'a Puzzles {
        type IntoIter = Iter<'a>;
        type Item = &'a YearPuzzles;

        #[inline]
        fn into_iter(self) -> Self::IntoIter {
            Iter { iter: self.year_puzzles.iter() }
        }
    }

    pub struct Iter<'a> {
        iter: std::slice::Iter<'a, YearPuzzles>,
    }

    impl<'a> Iterator for Iter<'a> {
        type Item = &'a YearPuzzles;

        #[inline]
        fn next(&mut self) -> Option<Self::Item> {
            self.iter.next()
        }
    }
}
