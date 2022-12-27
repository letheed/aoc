use lazy_static::lazy_static;

use crate::{parse::*, Date, Day, Puzzle, Result};

const DATE: Date = Date::new(Day::D04, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

#[allow(clippy::needless_pass_by_value)]
fn solve(input: String) -> Result {
    let mut real_room_ids = 0;
    let mut north_pole_id = 0;
    let mut buffer = Vec::new();
    for line in input.lines() {
        let room = parse_room(line)?;
        if room.is_a_real_room() {
            real_room_ids += room.id;
            room.decode(&mut buffer);
            if contains(&buffer, b"north") && contains(&buffer, b"pole") {
                north_pole_id = room.id;
            }
        }
    }
    answer!(real_room_ids, north_pole_id);
}

const LOWERCASE: &[u8; 26] = b"abcdefghijklmnopqrstuvwxyz";
const LOWERCASE_ROT: &[u8; 52] = b"abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz";

struct Room<'a> {
    name: &'a [u8],
    id: u32,
    checksum: &'a [u8],
}

impl<'a> Room<'a> {
    fn is_a_real_room(&self) -> bool {
        lazy_static! {
            static ref LOWERCASE_COUNT_MAP: [(u8, u8); 26] = {
                let mut lowercase_count_map = [(b'a', 0); 26];
                lowercase_count_map.iter_mut().zip(LOWERCASE.iter()).for_each(|(lc, &b)| lc.0 = b);
                lowercase_count_map
            };
        }
        let mut lowercase_count_map = *LOWERCASE_COUNT_MAP;
        for b in self.name {
            match b {
                b'-' => {}
                _ => lowercase_count_map[usize::from(b - b'a')].1 += 1,
            }
        }
        lowercase_count_map.sort_by(|a, b| b.1.cmp(&a.1));
        self.checksum.iter().zip(lowercase_count_map.iter()).all(|(b1, (b2, _))| b1 == b2)
    }

    #[allow(clippy::cast_possible_truncation)]
    fn decode(&self, buffer: &mut Vec<u8>) {
        buffer.clear();
        buffer.extend_from_slice(self.name);
        let rot = (self.id % 26) as u8;
        for b in buffer {
            match b {
                b'-' => *b = b' ',
                _ => *b = LOWERCASE_ROT[usize::from(*b + rot - b'a')],
            }
        }
    }
}

fn contains(buffer: &[u8], pattern: &[u8]) -> bool {
    if pattern.is_empty() {
        return true;
    }
    buffer.windows(pattern.len()).any(|window| window == pattern)
}

#[allow(clippy::double_comparisons)]
fn parse_room(s: &str) -> Result<Room<'_>> {
    const fn is_name(b: u8) -> bool {
        b.is_ascii_lowercase() || b == b'-'
    }
    #[rustfmt::skip]
    nom!(do_parse!(s.as_bytes(),
        name: take_while1!(is_name) >> id: uint!(u32) >> char!('[') >>
        checksum: take_while_m_n!(5, 5, |b:u8| b.is_ascii_lowercase()) >> char!(']') >>
        (Room { name, id, checksum })
    ))
}
