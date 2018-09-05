use crate::{Date, Day, OkOrFail, Puzzle, Result};
use divrem::DivRem;

const DATE: Date = Date::new(Day::D20, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

#[allow(clippy::needless_pass_by_value)]
fn solve(input: String) -> Result {
    let presents_min = input.parse::<usize>()?;
    let n1 = first_house(presents_min, 10, None)?;
    let n2 = first_house(presents_min, 11, Some(50))?;
    answer!(n1, n2);
}

fn first_house(
    presents_min: usize,
    presents_per_elf: usize,
    max_houses: Option<usize>,
) -> Result<usize>
{
    let presents_min = presents_min / presents_per_elf;
    // Number of presents is presents_per_elf * Σ divisors(house)
    // Since we want to maximize that, we're going to assume
    // that the number we're looking for is a multiple of 2 and 3.
    let div = 2 * 3;
    let mut houses = vec![0; presents_min / div];
    for elf in 1..=presents_min {
        let mut step = elf;
        // Divide step by 2 if elf is a multiple of 2.
        if step % 2 == 0 {
            step /= 2;
        }
        // Divide step by 3 if elf is a multiple of 3.
        if let (q, 0) = step.div_rem(3) {
            step = q;
        }
        let mut house = step;
        let house_max = if let Some(max_houses) = max_houses {
            std::cmp::min(houses.len(), (max_houses + 1) * elf / div)
        } else {
            houses.len()
        };
        while house < house_max {
            houses[house] += elf;
            house += step;
        }
    }
    houses
        .iter()
        .position(|&presents| presents >= presents_min)
        .map(|n| n * div)
        .ok_or_fail("no house with enough presents")
}
