use crate::{Date, Day, Puzzle, Result};
use failure::bail;

const DATE: Date = Date::new(Day::D07, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

#[allow(clippy::needless_pass_by_value)]
fn solve(input: String) -> Result {
    let (mut support_tls, mut support_ssl) = (0, 0);
    let (mut supernets, mut hypernets, mut abas) = (vec![], vec![], vec![]);
    let mut bab = String::with_capacity(3);
    'ip: for line in input.lines() {
        supernets.clear();
        hypernets.clear();
        parse_supernets_and_hypernets(line, &mut supernets, &mut hypernets)?;
        if !hypernets.iter().any(|hypernet| has_abba(hypernet))
            && supernets.iter().any(|supernet| has_abba(supernet))
        {
            support_tls += 1;
        }
        for supernet in &supernets {
            abas.clear();
            parse_abas(supernet, &mut abas);
            for &(c0, c1) in &abas {
                bab.clear();
                bab.extend(&[c1, c0, c1]);
                if hypernets.iter().any(|hypernet| hypernet.contains(&bab)) {
                    support_ssl += 1;
                    continue 'ip;
                }
            }
        }
    }
    answer!(support_tls, support_ssl);
}

fn parse_supernets_and_hypernets(
    mut s: &'a str,
    supernets: &mut Vec<&'a str>,
    hypernets: &mut Vec<&'a str>,
) -> Result<()>
{
    loop {
        let mut split = s.splitn(2, '[');
        let supernet = split.next().expect("first `next()` cannot fail on `Split`");
        if !supernet.is_empty() {
            supernets.push(supernet);
        }
        let rest = match split.next() {
            Some(rest) => rest,
            None => return Ok(()),
        };
        let mut split = rest.splitn(2, ']');
        let hypernet = split.next().expect("first `next()` cannot fail on `Split`");
        let rest = match split.next() {
            Some(rest) => rest,
            None => bail!("input is missing closing ']'"),
        };
        if !hypernet.is_empty() {
            hypernets.push(hypernet);
        }
        s = rest
    }
}

fn has_abba(s: &str) -> bool {
    let mut chars = s.chars();
    let mut c0 = if let Some(c) = chars.next() { c } else { return false };
    let mut c1 = if let Some(c) = chars.next() { c } else { return false };
    let mut c2 = if let Some(c) = chars.next() { c } else { return false };
    for c3 in chars {
        if c0 != c1 && c1 == c2 && c0 == c3 {
            return true;
        }
        c0 = c1;
        c1 = c2;
        c2 = c3;
    }
    false
}

fn parse_abas(s: &str, abas: &mut Vec<(char, char)>) {
    let mut chars = s.chars();
    let mut c0 = if let Some(c) = chars.next() { c } else { return };
    let mut c1 = if let Some(c) = chars.next() { c } else { return };
    for c2 in chars {
        if c0 != c1 && c0 == c2 {
            abas.push((c0, c1));
        }
        c0 = c1;
        c1 = c2;
    }
}
