pub mod tacky;
pub mod x86;
use super::Identifier;
pub use crate::parse::VarType;
use crate::semantics::Attr;
pub use crate::semantics::StaticInit;
use std::collections::HashMap;
use std::io::Write;
pub use tacky::Instruction as TackyInstruction;
use x86::AsmType;
pub use x86::Binary;
pub use x86::CondCode;
pub use x86::Op;
pub use x86::Pseudo;
pub use x86::PseudoOp;
pub use x86::X86;
pub type SymbolTable = HashMap<Identifier, BackendSymbol>;

pub fn update_table(mut table: crate::semantics::SymbolTable) -> SymbolTable {
    let mut tbl = SymbolTable::new();
    for (key, val) in table.drain() {
        tbl.insert(key, BackendSymbol::from(val));
    }
    tbl
}

const fn asm_type(ty: VarType) -> AsmType {
    match ty {
        VarType::Int => AsmType::Longword,
        VarType::Long => AsmType::Quadword,
    }
}

impl From<Attr> for BackendSymbol {
    fn from(attr: Attr) -> Self {
        match attr {
            Attr::Fn { defined, .. } => Self::Fn { defined },
            Attr::Static { typ: ty, .. } | Attr::Automatic(ty) => {
                let is_static = matches!(attr, Attr::Static { .. });
                Self::Obj {
                    ty: asm_type(ty),
                    is_static,
                }
            }
        }
    }
}

pub enum BackendSymbol {
    Obj {
        ty: AsmType,
        is_static: bool,
    },
    #[allow(dead_code)]
    Fn {
        defined: bool,
    },
}

#[derive(Debug)]
pub struct Program<T: InstructionSet>(pub Box<[TopLevel<T>]>);

#[derive(Debug)]
pub enum TopLevel<T: InstructionSet> {
    Fn(FunctionDefinition<T>),
    StaticVar(StaticVar),
}

impl<T: InstructionSet> TopLevel<T> {
    const fn global(&self) -> bool {
        match self {
            Self::StaticVar(StaticVar { global, .. })
            | Self::Fn(FunctionDefinition { global, .. }) => *global,
        }
    }

    const fn name(&self) -> &Identifier {
        match self {
            Self::StaticVar(StaticVar { name, .. }) | Self::Fn(FunctionDefinition { name, .. }) => {
                name
            }
        }
    }
}

#[derive(Debug)]
pub struct StaticVar {
    pub name: Identifier,
    pub global: bool,
    pub alignment: u32,
    pub init: StaticInit,
}

pub trait InstructionSet {}
#[derive(Debug)]
pub struct FunctionDefinition<T: InstructionSet> {
    pub name: Identifier,
    // empty for x86, which kinda sucks,
    pub params: Box<[Identifier]>,
    pub global: bool,
    pub body: Box<[T]>,
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
    Sp,
}

pub fn emit(program: &Program<X86>) -> Box<[u8]> {
    let mut bytes = Vec::new();
    for top_level in &program.0 {
        if top_level.global() {
            let _ = writeln!(bytes, "\t.globl _{}", top_level.name());
        }
        match top_level {
            TopLevel::Fn(FunctionDefinition {
                name,
                params: _,
                body,
                global: _,
            }) => {
                let _ = writeln!(
                    bytes,
                    "\t.text\n{name}:\n\t{prelude}",
                    name = format_args!("_{}", name),
                    prelude = format_args!("pushq %rbp\n\tmovq %rsp, %rbp")
                );
                for instruction in body {
                    let _ = writeln!(bytes, "\t{instruction}");
                }
            }

            TopLevel::StaticVar(StaticVar {
                name,
                global: _,
                init,
                alignment,
            }) => {
                let init_is_zero = matches!(*init, StaticInit::Long(0) | StaticInit::Int(0));

                bytes.extend_from_slice(if init_is_zero {
                    b"\t.bss\n"
                } else {
                    b"\t.data\n"
                });
                let _ = writeln!(bytes, "\t.balign {}\n", alignment);

                let _ = writeln!(bytes, "_{name}:\n \t {init}");
            }
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
            Register::Sp => "%esp",
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
            Register::Sp => "%rsp",
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
            Register::Sp => "%spl",
        }
    }
}
