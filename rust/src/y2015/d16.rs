use self::Feature::{Akita, Car, Cat, Child, Goldfish, Perfume, Pomeranian, Samoyed, Tree, Vizsla};
use crate::{parse::*, Date, Day, OkOrFail, Puzzle, Result};
use failure::bail;
use fnv::{FnvBuildHasher as BuildHasher, FnvHashMap as HashMap};

const DATE: Date = Date::new(Day::D16, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

#[cfg_attr(feature = "cargo-clippy", allow(needless_pass_by_value))]
fn solve(input: String) -> Result {
    let mut aunts = parse_aunts(&input)?;
    if aunts.is_empty() {
        bail!("input is empty");
    }
    let sample_aunt = aunts.swap_remove(0);
    let aunt1 = find_aunt(&sample_aunt, &aunts, |_, sample_n, n| sample_n == n)?;
    let aunt2 = find_aunt(&sample_aunt, &aunts, |feature, sample_n, n| match feature {
        Cat | Tree => n > sample_n,
        Pomeranian | Goldfish => n < sample_n,
        _ => n == sample_n,
    })?;
    answer!(aunt1.id, aunt2.id);
}

fn find_aunt(
    sample_aunt: &Aunt,
    aunts: &'a [Aunt],
    p: impl Fn(Feature, u8, u8) -> bool,
) -> Result<&'a Aunt>
{
    aunts
        .iter()
        .find(|&aunt| {
            aunt.features.iter().all(|(&feature, &n)| {
                if let Some(&sample) = sample_aunt.features.get(&feature) {
                    p(feature, sample, n)
                } else {
                    false
                }
            })
        }).ok_or_fail("aunt not found")
}

type Features = HashMap<Feature, u8>;

struct Aunt {
    id: u16,
    features: Features,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
#[rustfmt::skip]
enum Feature {
    Child, Cat, Samoyed, Pomeranian, Akita,
    Vizsla, Goldfish, Tree, Car, Perfume,
}

fn parse_aunts(s: &str) -> Result<Vec<Aunt>> {
    s.lines().map(parse_aunt).collect()
}

#[rustfmt::skip]
fn parse_aunt(s: &str) -> Result<Aunt> {
    macro_rules! feature { ($name:ident, $feature:expr, $tag:expr) => {
            named!($name(Bytes<'_>) -> Feature, value!($feature, tag!($tag)))
    };}
    feature!(child, Child, "children");
    feature!(cat, Cat, "cats");
    feature!(samoyed, Samoyed, "samoyeds");
    feature!(pomeranian, Pomeranian, "pomeranians");
    feature!(akita, Akita, "akitas");
    feature!(vizsla, Vizsla, "vizslas");
    feature!(goldfish, Goldfish, "goldfish");
    feature!(tree, Tree, "trees");
    feature!(car, Car, "cars");
    feature!(perfume, Perfume, "perfumes");
    named!(feature(Bytes<'_>) -> Feature, alt!(
        child | cat | samoyed | pomeranian | akita |
        vizsla | goldfish | tree | car | perfume)
    );
    fn property(s: Bytes<'_>) -> IResult<Bytes<'_>, (Feature, u8)> {
        sep!(s, space0, do_parse!(
            feature: feature >> char!(':') >>
            n: uint!(u8) >> opt!(char!(',')) >>
            ((feature, n))
        ))
    }
    nom!(
        sep!(Bytes(s.as_bytes()), space0, do_parse!(
            tag!("Sue") >> id: uint!(u16) >> char!(':') >>
            features: fold_many1!(property, HashMap::with_capacity_and_hasher(3, BuildHasher::default()),
                |mut feats: Features, (feat, n)| {
                    feats.insert(feat, n);
                    feats
                }) >>
            (Aunt { id, features })
        ))
    )
}
