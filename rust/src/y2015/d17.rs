use crate::{Date, Day, Puzzle, Result};

const DATE: Date = Date::new(Day::D17, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

#[cfg_attr(feature = "cargo-clippy", allow(needless_pass_by_value))]
fn solve(input: String) -> Result {
    let mut jugs = parse_jugs(&input)?;
    let wanted_volume = jugs.swap_remove(0);
    jugs.sort_unstable();
    let mut combinations = 0;
    let mut combinations_of_fewest_jugs = 0;
    let mut fewest_jugs = u8::max_value();
    let mut volumes = vec![Volume::default()];
    let mut new_volumes = Vec::new();
    for jug in jugs {
        for &volume in &volumes {
            let new_total = volume.total + jug;
            if new_total == wanted_volume {
                combinations += 1;
                let jugs = volume.jugs + 1;
                if jugs == fewest_jugs {
                    combinations_of_fewest_jugs += 1;
                } else if jugs < fewest_jugs {
                    fewest_jugs = jugs;
                    combinations_of_fewest_jugs = 1;
                }
                new_volumes.push(volume);
            } else if new_total < wanted_volume {
                let mut new_volume = volume;
                new_volume.jugs += 1;
                new_volume.total = new_total;
                new_volumes.push(volume);
                new_volumes.push(new_volume);
            }
        }
        std::mem::swap(&mut volumes, &mut new_volumes);
        new_volumes.clear();
    }
    answer!(combinations, combinations_of_fewest_jugs);
}

type Jug = u8;

#[derive(Copy, Clone, Default)]
struct Volume {
    jugs: u8,
    total: Jug,
}

fn parse_jugs(s: &str) -> Result<Vec<Jug>> {
    Ok(s.lines().map(|line| line.parse::<Jug>()).collect::<std::result::Result<_, _>>()?)
}
