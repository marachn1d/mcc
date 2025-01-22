pub mod tacky;
pub mod x86;
use super::Identifier;
use std::io::Write;
pub use tacky::Instruction as TackyInstruction;
pub use x86::Binary;
pub use x86::CondCode;
pub use x86::Op;
pub use x86::Pseudo;
pub use x86::PseudoOp;
pub use x86::X86;

use std::fmt::{self, Display, Formatter};

pub struct Program<T: InstructionSet>(pub Function<T>);

pub trait InstructionSet {}

pub struct Function<T: InstructionSet> {
    pub name: Identifier,
    pub body: Box<[T]>,
}

#[derive(Clone, Copy)]
pub enum Register {
    Ax,
    Cx,
    Dx,
    R10,
    R11,
}

pub fn emit(program: &Program<X86>) -> Box<[u8]> {
    emit_function(&program.0)
}

fn emit_function(function: &Function<X86>) -> Box<[u8]> {
    let mut bytes = Vec::new();
    let _ = write!(
        bytes,
        "\t.globl {name}\n{name}:\n\tpushq %rbp\n\tmovq %rsp, %rbp",
        name = format_args!("_{}", function.name)
    );
    for instruction in &function.body {
        let _ = write!(bytes, "\n\t");
        instruction.emit(&mut bytes);
    }
    bytes.into()
}

pub fn push_instructions<const N: usize, T: InstructionSet>(
    vec: &mut Vec<T>,
    instructions: [T; N],
) {
    vec.reserve(N);
    for instruction in instructions {
        vec.push(instruction);
    }
}

impl Register {
    const fn extended(&self) -> &'static str {
        match self {
            Register::Ax => "%eax",
            Register::Cx => "%ecx",
            Register::Dx => "%edx",
            Register::R10 => "%r10d",
            Register::R11 => "%r11d",
        }
    }

    const fn one_byte(&self) -> &'static str {
        match self {
            Register::Ax => "%al",
            Register::Cx => "%cl",
            Register::Dx => "%dl",
            Register::R10 => "%r10b",
            Register::R11 => "%r11b",
        }
    }
}
