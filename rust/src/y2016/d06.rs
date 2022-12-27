use fnv::FnvHashMap as HashMap;

use crate::{Date, Day, OkOrFail, Puzzle, Result};

const DATE: Date = Date::new(Day::D06, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

#[allow(clippy::needless_pass_by_value)]
fn solve(input: String) -> Result {
    let first_line = input.lines().next().ok_or_fail("input is empty")?;
    let mut count_maps = vec![HashMap::default(); first_line.chars().count()];
    for line in input.lines() {
        for (c, count_map) in line.chars().zip(count_maps.iter_mut()) {
            count_map.entry(c).and_modify(|v| *v += 1).or_insert(1);
        }
    }
    #[rustfmt::skip]
    let message1 = count_maps.iter().map(|count_map: &HashMap<char, u16>| {
        count_map.iter().max_by_key(|kv| kv.1).expect("iterator cannot be empty").0
    }).collect::<String>();
    #[rustfmt::skip]
    let message2 = count_maps.iter().map(|count_map| {
        count_map.iter().min_by_key(|kv| kv.1).expect("iterator cannot be empty").0
    }).collect::<String>();
    answer!(message1, message2);
}
