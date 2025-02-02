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

#[derive(Debug)]
pub struct Program<T: InstructionSet>(pub Function<T>);

pub trait InstructionSet {}

#[derive(Debug)]
pub struct Function<T: InstructionSet> {
    pub name: Identifier,
    pub body: Box<[T]>,
}

#[derive(Clone, Copy, Debug)]
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
        let _ = write!(bytes, "\n\t{instruction}");
    }
    bytes.into()
}

#[derive(Default)]
pub struct OpVec<T: InstructionSet>(pub Vec<T>);

impl<T: InstructionSet> OpVec<T> {
    pub const fn new() -> Self {
        Self(Vec::new())
    }

    pub fn push_one(&mut self, op: impl Into<T>) {
        self.0.push(op.into())
    }

    pub fn push(&mut self, iter: impl IntoIterator<Item = T>) {
        self.0.extend(iter)
    }
}

impl<T: InstructionSet> From<Vec<T>> for OpVec<T> {
    fn from(vec: Vec<T>) -> Self {
        Self(vec)
    }
}

impl<T: InstructionSet> std::ops::Deref for OpVec<T> {
    type Target = Vec<T>;
    fn deref(&self) -> &Vec<T> {
        &self.0
    }
}

impl<T: InstructionSet> From<OpVec<T>> for Box<[T]> {
    fn from(other: OpVec<T>) -> Self {
        other.0.into()
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
