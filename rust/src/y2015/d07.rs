use failure::bail;
use fnv::{FnvBuildHasher as BuildHasher, FnvHashMap as HashMap};

use self::Gate::{And, And1, Input, Lshift, Not, Or, Repeat, Rshift};
use crate::{parse::*, Date, Day, OkOrFail, Puzzle, Result};

const DATE: Date = Date::new(Day::D07, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

#[allow(clippy::needless_pass_by_value)]
fn solve(input: String) -> Result {
    let mut circuit = Circuit::with_capacity(512);
    for line in input.lines() {
        let (wire, gate) = parse_gate(line)?;
        circuit.insert_gate(wire, gate)?;
    }
    let a1 = circuit.resolve("a")?;
    circuit.clear_values();
    circuit.replace_gate("b", Input(a1)).ok_or_fail("wire did not exist")?;
    let a2 = circuit.resolve("a")?;
    answer!(a1, a2);
}

#[derive(Clone, Copy)]
enum Gate<'a> {
    Input(u16),
    Repeat(&'a str),
    Not(&'a str),
    And1(&'a str),
    And(&'a str, &'a str),
    Or(&'a str, &'a str),
    Lshift(&'a str, u16),
    Rshift(&'a str, u16),
}

#[rustfmt::skip]
#[allow(clippy::cognitive_complexity)]
fn parse_gate(s: &str) -> Result<(&str, Gate<'_>)> {
    macro_rules! gate { ($name:ident, $($args:tt)*) => {
            named!($name(Str<'_>) -> Gate<'_>, sep!(space0, do_parse!( $($args)* )))
    };}
    gate!(input,                                  n: uint!(u16) >> (Input(n)));
    gate!(repeat,                                 in_: alpha    >> (Repeat(in_.0)));
    gate!(not,                  tag!("NOT")    >> in_: alpha    >> (Not(in_.0)));
    gate!(and1,   tag!("1")  >> tag!("AND")    >> in_: alpha    >> (And1(in_.0)));
    gate!(and,    in1: alpha >> tag!("AND")    >> in2: alpha    >> (And(in1.0, in2.0)));
    gate!(or,     in1: alpha >> tag!("OR")     >> in2: alpha    >> (Or(in1.0, in2.0)));
    gate!(lshift, in_: alpha >> tag!("LSHIFT") >> n: uint!(u16) >> (Lshift(in_.0, n)));
    gate!(rshift, in_: alpha >> tag!("RSHIFT") >> n: uint!(u16) >> (Rshift(in_.0, n)));
    nom!(
        sep!(Str(s), space0, do_parse!(
            gate: alt!(and | or   | lshift | rshift |
                       not | and1 | input  | repeat) >> tag!("->") >>
            out: alpha >>
            (out.0, gate)
        ))
    )
}

struct Circuit<'a> {
    gates: HashMap<&'a str, Gate<'a>>,
    wires: HashMap<&'a str, u16>,
}

impl<'a> Circuit<'a> {
    fn with_capacity(cap: usize) -> Self {
        Self {
            gates: HashMap::with_capacity_and_hasher(cap, BuildHasher::default()),
            wires: HashMap::with_capacity_and_hasher(cap, BuildHasher::default()),
        }
    }

    fn insert_gate(&mut self, wire: &'a str, gate: Gate<'a>) -> Result<()> {
        match self.gates.insert(wire, gate) {
            Some(_) => bail!("wire {} has more than one source", wire),
            None => Ok(()),
        }
    }

    fn insert_value(&mut self, wire: &'a str, value: u16) -> Result<()> {
        match self.wires.insert(wire, value) {
            Some(_) => bail!("wire {} already has a value", wire),
            None => Ok(()),
        }
    }

    fn get_gate(&self, wire: &str) -> Result<Gate<'a>> {
        match self.gates.get(wire) {
            Some(gate) => Ok(*gate),
            None => bail!("wire {} has no source", wire),
        }
    }

    fn get_value(&self, wire: &str) -> Option<u16> {
        self.wires.get(wire).copied()
    }

    fn clear_values(&mut self) {
        self.wires.clear();
    }

    fn replace_gate(&mut self, wire: &'a str, gate: Gate<'a>) -> Option<Gate<'a>> {
        self.gates.insert(wire, gate)
    }

    #[allow(clippy::shadow_unrelated)]
    fn resolve(&mut self, wire: &'a str) -> Result<u16> {
        macro_rules! unary_gate {
            ($stack:ident, $wire:ident, $in_:ident) => {
                unary_gate!($stack, $wire, $in_, |n| n)
            };
            ($stack:ident, $wire:ident, $in_:ident, $fn:expr) => {
                if let Some(n) = self.get_value($in_) {
                    self.insert_value($wire, $fn(n))?;
                    $stack.pop().ok_or_fail("resolve stack is empty")?;
                } else {
                    $stack.push($in_);
                }
            };
        }

        macro_rules! binary_gate {
            ($stack:ident, $wire:ident, $in1:ident, $in2:ident, $fn:expr) => {
                if let Some(n1) = self.get_value($in1) {
                    if let Some(n2) = self.get_value($in2) {
                        self.insert_value($wire, $fn(n1, n2))?;
                        $stack.pop().ok_or_fail("resolve stack is empty")?;
                    } else {
                        $stack.push($in2);
                    }
                } else {
                    $stack.push($in1);
                }
            };
        }

        let mut stack = vec![wire];
        while !stack.is_empty() {
            let wire = stack.last().copied().expect("stack cannot be empty");
            let gate = self.get_gate(wire)?;
            match gate {
                And(in1, in2) => binary_gate!(stack, wire, in1, in2, |n, m| n & m),
                Or(in1, in2) => binary_gate!(stack, wire, in1, in2, |n, m| n | m),
                Lshift(in_, m) => unary_gate!(stack, wire, in_, |n| n << m),
                Rshift(in_, m) => unary_gate!(stack, wire, in_, |n| n >> m),
                Not(in_) => unary_gate!(stack, wire, in_, |n: u16| !n),
                And1(in_) => unary_gate!(stack, wire, in_, |n| n % 2),
                Repeat(in_) => unary_gate!(stack, wire, in_),
                Input(n) => {
                    self.insert_value(wire, n)?;
                    stack.pop().ok_or_fail("resolve stack is empty")?;
                }
            }
        }
        self.get_value(wire).ok_or_fail("failed to resolve")
    }
}
