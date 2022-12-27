use std::{collections::BTreeMap, str};

use failure::bail;

use crate::{Date, Day, Puzzle, Result};

const DATE: Date = Date::new(Day::D09, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

#[allow(clippy::needless_pass_by_value)]
fn solve(input: String) -> Result {
    let (decompressed_len_v1, decompressed_len_v2) = decompressed_lengths(&input)?;
    answer!(decompressed_len_v1, decompressed_len_v2);
}

macro_rules! parse_int {
    ($bytes:expr, $i:expr, $b:expr) => {{
        let mut n = 0;
        loop {
            $i += 1;
            match $bytes.next() {
                Some(b) if b.is_ascii_digit() => n = n * 10 + usize::from(b - b'0'),
                Some($b) => break,
                Some(b) => bail!("expected '{}', found {:x}", $b as char, b),
                None => bail!("unexpected end of input"),
            }
        }
        n
    }};
}

fn decompressed_lengths(s: &str) -> Result<(usize, usize)> {
    let mut decompressed_length_v1 = 0;
    let mut decompressed_length_v2 = 0;
    let mut i = 0;
    let mut weight = 1;
    let mut weights = BTreeMap::default();
    let mut next_span_end = usize::max_value();
    let mut top_level_span_end = 0;
    let bytes = &mut s.bytes();
    loop {
        match bytes.next() {
            Some(b'(') => {
                let len = parse_int!(bytes, i, b'x');
                let n = parse_int!(bytes, i, b')');
                let span_end = i + len;
                if i > top_level_span_end {
                    decompressed_length_v1 += n * len;
                    top_level_span_end = span_end;
                }
                if span_end < next_span_end {
                    next_span_end = span_end;
                }
                weight *= n;
                weights.entry(span_end).and_modify(|w| *w *= n).or_insert(n);
            }
            Some(_) => {
                decompressed_length_v2 += weight;
                #[allow(clippy::comparison_chain)]
                if i == next_span_end {
                    match weights.remove(&i) {
                        Some(w) => weight /= w,
                        None => bail!("span end not in weights"),
                    }
                    next_span_end = match weights.keys().next() {
                        Some(&j) => j,
                        None => usize::max_value(),
                    };
                } else if i > next_span_end {
                    bail!("missed a span end");
                }
            }
            None => break,
        }
        i += 1;
    }
    Ok((decompressed_length_v1, decompressed_length_v2))
}

#[allow(unused)]
mod alternative {
    use failure::bail;

    use crate::{parse::*, Date, Day, Puzzle, Result};

    #[allow(clippy::needless_pass_by_value)]
    pub(super) fn solve(input: String) -> Result {
        let data_v1 = parse_compressed_data_v1(&input)?;
        let decompressed_len_v1 = data_v1.iter().map(StubV1::decompressed_len).sum::<usize>();
        let data_v2 = parse_compressed_data_v2(&input)?;
        let decompressed_len_v2 = data_v2.iter().map(StubV2::decompressed_len).sum::<usize>();
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
                StubV2::Repeat(stubs, n) => n * stubs.iter().map(StubV2::decompressed_len).sum::<usize>(),
            }
        }
    }

    #[allow(clippy::redundant_closure)]
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

    #[allow(clippy::redundant_closure)]
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
}
