use json::JsonValue::{self, Array, Number, Object};

use crate::{Date, Day, Puzzle, Result};

const DATE: Date = Date::new(Day::D12, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

#[allow(clippy::needless_pass_by_value)]
fn solve(input: String) -> Result {
    let json = json::parse(&input)?;
    let n1 = sum_numbers(&json);
    let n2 = sum_numbers_not_red(&json);
    answer!(n1, n2);
}

fn sum_numbers(json: &JsonValue) -> f64 {
    let mut n = 0.0;
    let mut stack = vec![json];
    while let Some(val) = stack.pop() {
        match val {
            Object(obj) => stack.extend(obj.iter().map(|kv| kv.1)),
            Array(array) => stack.extend(array),
            Number(num) => n += f64::from(*num),
            _ => {}
        }
    }
    n
}

fn sum_numbers_not_red(json: &JsonValue) -> f64 {
    let mut n = 0.0;
    let mut stack = vec![json];
    while let Some(val) = stack.pop() {
        match val {
            Object(obj) => {
                if !obj.iter().any(|kv| kv.1 == "red") {
                    stack.extend(obj.iter().map(|kv| kv.1));
                }
            }
            Array(array) => stack.extend(array),
            Number(num) => n += f64::from(*num),
            _ => {}
        }
    }
    n
}
