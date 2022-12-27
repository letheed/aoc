use fnv::FnvHashMap as HashMap;

use crate::{parse::*, Date, Day, OkOrFail, Puzzle, Result};

const DATE: Date = Date::new(Day::D09, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

#[allow(clippy::needless_pass_by_value)]
fn solve(input: String) -> Result {
    let mut roads = HashMap::default();
    let mut cities = Vec::new();
    for line in input.lines() {
        let (from, to, dist) = parse_distance(line)?;
        roads.insert((from, to), dist);
        roads.insert((to, from), dist);
        cities.push(from);
        cities.push(to);
    }
    cities.sort_unstable();
    cities.dedup();
    let (mut dist_min, mut dist_max) = (u16::max_value(), u16::min_value());
    let mut heap = permutohedron::Heap::new(&mut *cities);
    while let Some(route) = heap.next_permutation() {
        let mut dist = 0;
        for road in route.windows(2) {
            dist += roads.get(&(road[0], road[1])).ok_or_fail("road does not exist")?;
        }
        if dist < dist_min {
            dist_min = dist;
        } else if dist > dist_max {
            dist_max = dist;
        }
    }
    answer!(dist_min, dist_max);
}

#[rustfmt::skip]
fn parse_distance(s: &str) -> Result<(&str, &str, u16)> {
    nom!(
        sep!(Str(s), space0, do_parse!(
            from: alpha >> tag!("to") >>
            to: alpha >> tag!("=") >>
            distance: uint!(u16) >>
            ((from.0, to.0, distance))
        ))
    )
}
