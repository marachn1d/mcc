pub mod tacky;
use std::rc::Rc;
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
pub struct Program<T: InstructionSet>(pub Box<[FunctionDefinition<T>]>);

pub trait InstructionSet {}
#[derive(Debug)]
pub struct FunctionDefinition<T: InstructionSet> {
    pub name: Rc<Identifier>,
    // empty for x86, which kinda sucks,
    pub params: Box<[Rc<Identifier>]>,
    pub body: Box<[T]>,
}

impl<T: InstructionSet> FunctionDefinition<T> {
    pub const fn stack_bytes(&self) -> usize {
        if self.params.len() <= 6 {
            return 0;
        }

        let padding_bytes = if self.params.len() % 2 == 0 { 0 } else { 8 };

        ((self.params.len() - 6) * 8) + padding_bytes
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Register {
    Ax,
    Cx,
    Dx,
    Di,
    Si,
    R8,
    R9,
    R10,
    R11,
}

pub fn emit(program: &Program<X86>) -> Box<[u8]> {
    emit_function(&program.0)
}

fn emit_function(functions: &[FunctionDefinition<X86>]) -> Box<[u8]> {
    let mut bytes = Vec::new();
    for FunctionDefinition {
        name,
        params: _,
        body,
    } in functions
    {
        let _ = write!(
            bytes,
            "\n.globl {name}\n{name}:\n\tpushq %rbp\n\tmovq %rsp, %rbp",
            name = format_args!("_{}", name)
        );
        for instruction in body {
            let _ = write!(bytes, "\n\t{instruction}");
        }
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
        self.0.push(op.into());
    }

    pub fn push(&mut self, iter: impl IntoIterator<Item = T>) {
        self.0.extend(iter);
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
    const fn extended(self) -> &'static str {
        match self {
            Register::Ax => "%eax",
            Register::Cx => "%ecx",
            Register::Dx => "%edx",
            Register::Di => "%edi",
            Register::Si => "%esi",

            Register::R8 => "%r8d",
            Register::R9 => "%r9d",
            Register::R10 => "%r10d",
            Register::R11 => "%r11d",
        }
    }

    const fn eight_byte(self) -> &'static str {
        match self {
            Register::Ax => "%rax",
            Register::Cx => "%rcx",
            Register::Dx => "%rdx",
            Register::Di => "%rdi",
            Register::Si => "%rsi",
            Register::R8 => "%r8",
            Register::R9 => "%r9",
            Register::R10 => "%r10",
            Register::R11 => "%r11",
        }
    }

    const fn one_byte(self) -> &'static str {
        match self {
            Register::Ax => "%al",
            Register::Cx => "%cl",
            Register::Dx => "%dl",
            Register::Di => "%dil",
            Register::Si => "%sil",
            Register::R8 => "%r8b",
            Register::R9 => "%r9b",
            Register::R10 => "%r10b",
            Register::R11 => "%r11b",
        }
    }
}
