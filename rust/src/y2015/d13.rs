use crate::{parse::*, Date, Day, OkOrFail, Puzzle, Result};
use failure::bail;
use fnv::FnvHashMap as HashMap;

const DATE: Date = Date::new(Day::D13, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

#[cfg_attr(feature = "cargo-clippy", allow(needless_pass_by_value))]
fn solve(input: String) -> Result {
    let preferences = parse_preferences(&input)?;
    let mut guests = preferences.keys().map(|gn| gn.0).collect::<Vec<_>>();
    guests.sort_unstable();
    guests.dedup();
    if guests.len() < 3 {
        bail!("not enough guests");
    }
    let mut happiness_max = i16::min_value();
    let mut best_seating = guests.clone().into_boxed_slice();
    let pinned_guest = guests.pop().ok_or_fail("there are no guests")?;
    let mut heap = permutohedron::Heap::new(&mut guests);
    while let Some(seating) = heap.next_permutation() {
        seating.push(pinned_guest);
        let happiness = happiness(seating, &preferences).ok_or_fail("missing preference")?;
        if happiness > happiness_max {
            happiness_max = happiness;
            best_seating.copy_from_slice(seating);
        }
        seating.pop();
    }
    let biggest_dislike = best_seating
        .iter()
        .zip(best_seating.iter().cycle().skip(1))
        .map(|(&left, &right)| preferences[&(left, right)] + preferences[&(right, left)])
        .min()
        .ok_or_fail("best_seating is empty")?;
    answer!(happiness_max, happiness_max - biggest_dislike);
}

fn happiness(seating: &[&'a str], preferences: &HashMap<(&'a str, &'a str), i16>) -> Option<i16> {
    let mut happiness = 0;
    for (left, right) in seating.iter().zip(seating.iter().cycle().skip(1)) {
        happiness += preferences.get(&(left, right))?;
        happiness += preferences.get(&(right, left))?;
    }
    Some(happiness)
}

fn parse_preferences(s: &str) -> Result<HashMap<(&str, &str), i16>> {
    s.lines().map(parse_preference).collect()
}

#[rustfmt::skip]
fn parse_preference(s: &str) -> Result<((&str, &str), i16)> {
    nom!(
        sep!(Str(s), space0, do_parse!(
            guest: alpha >> tag!("would") >>
            sign: alt!(tag!("gain") | tag!("lose")) >>
            n: uint!(i16) >> tag!("happiness units by sitting next to") >>
            neighbor: alpha >>
            ({
                let mut n = n;
                if sign.0 == "lose" { n = -n; };
                ((guest.0, neighbor.0), n)
            })
        ))
    )
}
