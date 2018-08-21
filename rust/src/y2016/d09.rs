use crate::{parse::*, Date, Day, Puzzle, Result};

const DATE: Date = Date::new(Day::D09, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

#[cfg_attr(feature = "cargo-clippy", allow(needless_pass_by_value))]
fn solve(input: String) -> Result {
    let data_v1 = parse_compressed_data_v1(&input)?;
    let decompressed_len_v1 = data_v1.iter().map(|stub| stub.decompressed_len()).sum::<usize>();
    let data_v2 = parse_compressed_data_v2(&input)?;
    let decompressed_len_v2 = data_v2.iter().map(|stub| stub.decompressed_len()).sum::<usize>();
    answer!(decompressed_len_v1, decompressed_len_v2);
}

#[derive(Debug)]
enum StubV1<'a> {
    Once(&'a str),
    Repeat(&'a str, usize),
}

impl StubV1<'_> {
    fn decompressed_len(&self) -> usize {
        match self {
            StubV1::Once(s) => s.len(),
            StubV1::Repeat(s, n) => n * s.len(),
        }
    }
}

#[derive(Debug)]
enum StubV2<'a> {
    Once(&'a str),
    Repeat(Vec<StubV2<'a>>, usize),
}

impl StubV2<'_> {
    fn decompressed_len(&self) -> usize {
        match self {
            StubV2::Once(s) => s.len(),
            StubV2::Repeat(stubs, n) => {
                n * stubs.iter().map(|stub| stub.decompressed_len()).sum::<usize>()
            }
        }
    }
}

#[cfg_attr(feature = "cargo-clippy", allow(redundant_closure))]
#[rustfmt::skip]
fn parse_compressed_data_v1(s: &str) -> Result<Vec<StubV1<'_>>> {
    named!(once(Str<'_>) -> StubV1<'_>, map!(take_till1!(|c| c == '('), |s| StubV1::Once(*s)));
    named!(marker(Str<'_>) -> (usize, usize), do_parse!(
        char!('(') >> len: uint!(usize) >>
        char!('x') >> n: uint!(usize) >>
        char!(')') >> ((len, n))
    ));
    named!(repeat(Str<'_>) -> StubV1<'_>, do_parse!(
        mark: marker >> s: take!(mark.0) >> (StubV1::Repeat(*s, mark.1))
    ));
    nom!(many0!(Str(s), alt!(once | repeat)))
}

#[cfg_attr(feature = "cargo-clippy", allow(redundant_closure))]
#[rustfmt::skip]
fn parse_compressed_data_v2(s: &str) -> Result<Vec<StubV2<'_>>> {
    named!(once(Str<'_>) -> StubV2<'_>, map!(take_till1!(|c| c == '('), |s| StubV2::Once(*s)));
    named!(marker(Str<'_>) -> (usize, usize), do_parse!(
        char!('(') >> len: uint!(usize) >>
        char!('x') >> n: uint!(usize) >>
        char!(')') >> ((len, n))
    ));
    fn repeat(s: Str<'_>) -> IResult<Str<'_>, StubV2<'_>> {
        let (s_marker, (len, n)) = marker(s)?;
        let (rest, slice) = take!(s_marker, len)?;
        let (_, stubs) = try_parse!(slice, stubs);
        Ok((rest, StubV2::Repeat(stubs, n)))
    }
    named!(stubs(Str<'_>) -> Vec<StubV2<'_>>, many1!(alt!(once | repeat)));
    nom!(stubs(Str(s)))
}
