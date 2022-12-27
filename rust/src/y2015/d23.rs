use self::{
    Flow::{Jump, Next},
    Instruction::{Hlf, Inc, Jie, Jio, Jmp, Tpl},
    Register::{A, B},
};
use crate::{parse::*, Date, Day, Puzzle, Result};

const DATE: Date = Date::new(Day::D23, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

#[allow(clippy::needless_pass_by_value)]
fn solve(input: String) -> Result {
    let program = parse_program(&input)?;
    let mut machine = Machine::default();
    machine.run(&program);
    let b1 = machine.reg_b;
    machine.reg_a = 1;
    machine.reg_b = 0;
    machine.run(&program);
    let b2 = machine.reg_b;
    answer!(b1, b2);
}

type Program = Vec<Instruction>;

#[derive(Copy, Clone, Debug)]
enum Register {
    A,
    B,
}

enum Flow {
    Next,
    Jump(isize),
}

#[derive(Copy, Clone, Debug)]
enum Instruction {
    Hlf(Register),
    Tpl(Register),
    Inc(Register),
    Jmp(isize),
    Jie(Register, isize),
    Jio(Register, isize),
}

#[derive(Default)]
struct Machine {
    ptr: usize,
    reg_a: u32,
    reg_b: u32,
}

impl Machine {
    #[allow(clippy::ptr_arg)]
    fn run(&mut self, program: &Program) {
        self.ptr = 0;
        while let Some(&instruction) = program.get(self.ptr) {
            match self.execute(instruction) {
                Jump(offset) => {
                    if self.jump(offset) {
                        return;
                    }
                }
                Next => self.ptr += 1,
            }
        }
    }

    fn execute(&mut self, instruction: Instruction) -> Flow {
        match instruction {
            Hlf(reg) => *self.get_mut(reg) /= 2,
            Tpl(reg) => *self.get_mut(reg) *= 3,
            Inc(reg) => *self.get_mut(reg) += 1,
            Jmp(offset) => return Jump(offset),
            Jie(reg, offset) => {
                if self.get(reg) % 2 == 0 {
                    return Jump(offset);
                }
            }
            Jio(reg, offset) => {
                if self.get(reg) == 1 {
                    return Jump(offset);
                }
            }
        }
        Next
    }

    const fn get(&self, register: Register) -> u32 {
        match register {
            A => self.reg_a,
            B => self.reg_b,
        }
    }

    fn get_mut(&mut self, register: Register) -> &mut u32 {
        match register {
            A => &mut self.reg_a,
            B => &mut self.reg_b,
        }
    }

    #[allow(clippy::cast_sign_loss)]
    fn jump(&mut self, offset: isize) -> bool {
        if offset >= 0 {
            self.ptr += offset as usize;
            false
        } else {
            let offset = if offset == isize::min_value() { offset as usize } else { (-offset) as usize };
            let (ptr, overflow) = self.ptr.overflowing_sub(offset);
            self.ptr = ptr;
            overflow
        }
    }
}

#[rustfmt::skip]
#[allow(clippy::cognitive_complexity)]
fn parse_program(s: &str) -> Result<Program> {
    named!(register(Bytes<'_>) -> Register,
        alt!(value!(A, tag!("a")) | value!(B, tag!("b")))
    );
    named!(jmp(Bytes<'_>) -> Instruction, do_parse!(
        tag!("jmp") >> space >> offset: int!(isize) >> (Jmp(offset))
    ));
    macro_rules! reg_op { ($(($name:ident, $tag:expr, $var:ident)),*) => {$(
        named!($name(Bytes<'_>) -> Instruction, do_parse!(
            tag!($tag) >> space >> reg: register >> ($var(reg))));
    )*};}
    macro_rules! jmp_if { ($(($name:ident, $tag:expr, $var:ident)),*) => {$(
        named!($name(Bytes<'_>) -> Instruction, do_parse!(
            tag!($tag) >> space >>
            reg: register >> char!(',') >> space >>
            offset: int!(isize) >> ($var(reg, offset))));
    )*};}
    reg_op!((inc, "inc", Inc), (tpl, "tpl", Tpl), (hlf, "hlf", Hlf));
    jmp_if!((jie, "jie", Jie), (jio, "jio", Jio));
    nom!(separated_nonempty_list_complete!(
        Bytes(s.as_bytes()),
        char!('\n'),
        alt!(inc | tpl | hlf | jmp | jie | jio)
    ))
}
