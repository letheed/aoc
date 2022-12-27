use crate::{parse::*, Date, Day, Puzzle, Result};

const DATE: Date = Date::new(Day::D03, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

#[allow(clippy::needless_pass_by_value)]
fn solve(input: String) -> Result {
    let triplets = parse_triplets(&input)?;
    let valid_triangles_horizontal = triplets.iter().filter(|t| t.is_valid_triangle()).count();
    let mut valid_triangles_vertical = 0;
    for triplets_123 in triplets.chunks(3) {
        match triplets_123 {
            [t1, t2, t3] => {
                let tx = Triplet(t1.0, t2.0, t3.0);
                let ty = Triplet(t1.1, t2.1, t3.1);
                let tz = Triplet(t1.2, t2.2, t3.2);
                if tx.is_valid_triangle() {
                    valid_triangles_vertical += 1;
                }
                if ty.is_valid_triangle() {
                    valid_triangles_vertical += 1;
                }
                if tz.is_valid_triangle() {
                    valid_triangles_vertical += 1;
                }
            }
            _ => continue,
        }
    }
    answer!(valid_triangles_horizontal, valid_triangles_vertical);
}

#[derive(Copy, Clone)]
struct Triplet(u16, u16, u16);

impl Triplet {
    const fn is_valid_triangle(self) -> bool {
        let Self(a, b, c) = self;
        a + b > c && b + c > a && c + a > b
    }
}

fn parse_triplets(s: &str) -> Result<Vec<Triplet>> {
    s.lines().map(parse_triplet).collect::<Result<Vec<_>>>()
}

#[rustfmt::skip]
fn parse_triplet(s: &str) -> Result<Triplet> {
    nom!(do_parse!(Bytes(s.as_bytes()),
        space >> x: uint!(u16) >>
        space >> y: uint!(u16) >>
        space >> z: uint!(u16) >>
        (Triplet(x, y, z))
    ))
}
