use self::DeerState::{Flying, Resting};
use crate::{parse::*, Date, Day, OkOrFail, Puzzle, Result};
use divrem::DivRem;
use failure::bail;
use std::cmp::min;

const DATE: Date = Date::new(Day::D14, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

#[allow(clippy::needless_pass_by_value)]
fn solve(input: String) -> Result {
    let mut lines = input.lines();
    let time: u16 = lines.next().ok_or_fail("input is empty")?.parse()?;
    let reindeers = lines.map(parse_reindeer).collect::<Result<Vec<_>>>()?;
    let max_travel =
        reindeers.iter().map(|deer| deer.travel(time)).max().ok_or_fail("no reindeers")?;
    let mut runners =
        reindeers.iter().cloned().map(Runner::new).collect::<Vec<_>>().into_boxed_slice();
    let max_points = race(&mut runners, time);
    answer!(max_travel, max_points);
}

#[derive(Copy, Clone)]
struct Reindeer {
    speed: u16,
    fly: u16,
    rest: u16,
}

impl Reindeer {
    fn travel(self, time: u16) -> u16 {
        let (n, rest) = time.div_rem(self.fly + self.rest);
        n * (self.fly * self.speed) + min(self.fly, rest) * self.speed
    }
}

#[rustfmt::skip]
fn parse_reindeer(s: &str) -> Result<Reindeer> {
    let reindeer = nom!(
        sep!(Bytes(s.as_bytes()), space0, do_parse!(
            alpha >> tag!("can fly") >>
            speed: uint!(u16) >> tag!("km/s for") >>
            fly:   uint!(u16) >> tag!("seconds, but then must rest for") >>
            rest:  uint!(u16) >> tag!("seconds") >>
            (Reindeer { speed, fly, rest })
        ))
    )?;
    if reindeer.fly + reindeer.rest == 0 {
        bail!("reindeer neither flies nor rests");
    } else {
        Ok(reindeer)
    }
}

#[derive(Copy, Clone)]
enum DeerState {
    Flying(u16),
    Resting(u16),
}

struct Runner {
    deer: Reindeer,
    state: DeerState,
    travel: u16,
    points: u16,
}

impl Runner {
    fn new(deer: Reindeer) -> Self {
        Self { deer, state: Flying(deer.fly), travel: 0, points: 0 }
    }

    fn step(&mut self) {
        match self.state {
            Resting(0) => {
                self.state = Flying(self.deer.fly);
                self.step();
            }
            Resting(t) => self.state = Resting(t - 1),
            Flying(0) => {
                self.state = Resting(self.deer.rest);
                self.step();
            }
            Flying(t) => {
                self.travel += self.deer.speed;
                self.state = Flying(t - 1);
            }
        }
    }
}

fn race(runners: &mut [Runner], time: u16) -> u16 {
    for _ in 0..time {
        runners.iter_mut().for_each(|r| r.step());
        let max_travel = runners.iter().map(|r| r.travel).max().unwrap_or(0);
        for runner in runners.iter_mut() {
            if runner.travel == max_travel {
                runner.points += 1;
            }
        }
    }
    runners.iter().map(|r| r.points).max().unwrap_or(0)
}
