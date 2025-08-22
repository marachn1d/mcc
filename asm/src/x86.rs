use super::tacky::Value;
use ast::parse::{StaticInit, UnOp};
use ast::semantics::Attr;
use ast::Ident;
use ast::VarType;
use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};
use std::io::Write;
pub type BackendTable = HashMap<Ident, BackendSymbol>;

const fn asm_type(ty: VarType) -> AsmType {
    match ty {
        VarType::Int => AsmType::Longword,
        VarType::Long => AsmType::Quadword,
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

pub type Pseudo = BaseX86<PseudoOp>;
pub type X86 = BaseX86<Op>;
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum AsmType {
    Longword,
    Quadword,
}

#[derive(Clone, Debug)]
pub enum BaseX86<T: Operand> {
    Mov {
        ty: AsmType,
        regs: OpPair<T>,
    },
    Movsx {
        ty: AsmType,
        regs: OpPair<T>,
    },
    Unary {
        operator: Unary,
        operand: T,
        ty: AsmType,
    },
    Binary {
        operator: Binary,
        regs: OpPair<T>,
        ty: AsmType,
    },
    Push(T),
    Call(Ident),
    Ret,
    Idiv {
        divisor: T,
        ty: AsmType,
    },
    Cdq(AsmType),
    Cmp {
        ty: AsmType,
        regs: OpPair<T>,
    },
    Jmp(Ident),
    JmpCC {
        condition: CondCode,
        label: Ident,
    },
    SetCC {
        condition: CondCode,
        op: T,
    },
    Label(Ident),
}

pub type OpPair<T> = (T, T);

#[derive(Clone, Debug)]
pub enum CondCode {
    E,
    NE,
    G,
    GE,
    L,
    LE,
}

impl Display for CondCode {
    fn fmt(&self, writer: &mut Formatter) -> fmt::Result {
        write!(
            writer,
            "{}",
            match self {
                Self::E => "e",
                Self::NE => "ne",
                Self::G => "g",
                Self::GE => "ge",
                Self::L => "l",
                Self::LE => "le",
            }
        )
    }
}

impl BaseX86<PseudoOp> {
    pub const fn allocate_stack(n: i64) -> Self {
        Self::binary(
            Binary::Sub,
            Op::Imm(n).pseudo(),
            Op::Register(Register::Sp).pseudo(),
            AsmType::Quadword,
        )
    }

    pub const fn deallocate_stack(n: i64) -> Self {
        Self::binary(
            Binary::Add,
            Op::Imm(n).pseudo(),
            Op::Register(Register::Sp).pseudo(),
            AsmType::Quadword,
        )
    }
}

impl BaseX86<Op> {
    pub const fn allocate_stack(n: i64) -> Self {
        Self::binary(
            Binary::Sub,
            Op::Imm(n),
            Op::Register(Register::Sp),
            AsmType::Quadword,
        )
    }
}

impl<T: Operand> BaseX86<T> {
    pub const fn mov(src: T, dst: T, ty: AsmType) -> Self {
        Self::Mov {
            regs: (src, dst),
            ty,
        }
    }

    pub const fn cmp(s1: T, s2: T, ty: AsmType) -> Self {
        Self::Cmp { regs: (s1, s2), ty }
    }

    #[allow(dead_code)]
    pub const fn unary(operator: Unary, operand: T, ty: AsmType) -> Self {
        Self::Unary {
            operator,
            operand,
            ty,
        }
    }

    pub const fn binary(operator: Binary, op: T, dst: T, ty: AsmType) -> Self {
        Self::Binary {
            operator,
            regs: (op, dst),
            ty,
        }
    }

    pub const fn idiv(divisor: T, ty: AsmType) -> Self {
        Self::Idiv { divisor, ty }
    }

    pub const fn movsx(src: T, dst: T) -> Self {
        Self::Movsx {
            ty: AsmType::Quadword,
            regs: (src, dst),
        }
    }
}

pub trait Operand {}

impl Display for AsmType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str(match self {
            Self::Longword => "l",
            Self::Quadword => "q",
        })
    }
}

impl Display for X86 {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Call(fun) => write!(f, "call _{fun}"),

            Self::Push(Op::Register(r)) => write!(f, "pushq {}", r.eight_byte()),
            Self::Push(op) => write!(f, "pushq {op}"),

            Self::Mov {
                regs: (src, dst),
                ty,
            } => {
                write!(f, "mov{ty}  {}, {}", src.sized_fmt(*ty), dst.sized_fmt(*ty))
            }
            Self::Ret => write!(f, "movq %rbp, %rsp\n\tpopq %rbp\n\tret"),
            Self::Unary {
                operator,
                operand,
                ty,
            } => write!(f, "{operator}{ty} {}", operand.sized_fmt(*ty)),
            Self::Binary {
                operator: operator @ (Binary::ShiftLeft | Binary::ShiftRight),
                regs: (Op::Register(r), by),
                ty,
            } => {
                write!(f, "{operator}{ty} {}, {}", r.one_byte(), by.sized_fmt(*ty),)
            }
            Self::Binary {
                operator,
                regs: (op, dst_op),
                ty,
            } => {
                write!(
                    f,
                    "{operator}{ty} {}, {}",
                    op.sized_fmt(*ty),
                    dst_op.sized_fmt(*ty)
                )
            }
            Self::Idiv { divisor, ty } => {
                write!(f, "idiv{ty} {}", divisor.sized_fmt(*ty))
            }
            Self::Cdq(AsmType::Longword) => {
                write!(f, "cdq")
            }
            Self::Cdq(AsmType::Quadword) => {
                write!(f, "cqo")
            }
            Self::Cmp {
                regs: (left, right),
                ty,
            } => {
                write!(
                    f,
                    "cmp{ty} {}, {}",
                    left.sized_fmt(*ty),
                    right.sized_fmt(*ty)
                )
            }
            Self::Jmp(label) => {
                write!(f, "jmp L{label}")
            }
            Self::JmpCC { label, condition } => {
                write!(f, "j{condition} L{label}")
            }
            Self::SetCC { op, condition } => {
                write!(f, "set{condition} {op}")
            }
            Self::Label(label) => {
                write!(f, "L{label}:")
            }
            Self::Movsx {
                regs: (src, dst),
                ty,
            } => {
                write!(
                    f,
                    "movslq {}, {}",
                    src.sized_fmt(AsmType::Longword),
                    dst.sized_fmt(*ty)
                )
            }
        }
    }
}

impl Op {
    pub const fn pseudo(self) -> PseudoOp {
        PseudoOp::Normal(self)
    }
}

pub mod op_regs {
    use super::Op;
    use super::Register;
    pub const AX: Op = Op::Register(Register::Ax);
    pub const CX: Op = Op::Register(Register::Cx);
    pub const DX: Op = Op::Register(Register::Dx);
    pub const DI: Op = Op::Register(Register::Di);
    pub const SI: Op = Op::Register(Register::Si);
    pub const R8: Op = Op::Register(Register::R8);
    pub const R9: Op = Op::Register(Register::R9);
    pub const R10: Op = Op::Register(Register::R10);
    pub const R11: Op = Op::Register(Register::R11);
    pub const SP: Op = Op::Register(Register::Sp);
}

#[derive(Clone, Debug)]
pub enum Op {
    Imm(i64),
    Register(Register),
    Stack(isize),
    Data(Ident),
}
impl Operand for Op {}

impl Operand for PseudoOp {}

impl From<Register> for Op {
    fn from(reg: Register) -> Self {
        Self::Register(reg)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Unary {
    Not,
    Neg,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Binary {
    Add,
    Sub,
    Mult,
    And,
    Or,
    Xor,
    ShiftLeft,
    ShiftRight,
}

impl Display for Unary {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            (match self {
                Self::Not => "not",
                Self::Neg => "neg",
            })
        )
    }
}

impl Display for Binary {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str(match self {
            Self::Add => "add",
            Self::Sub => "sub",
            Self::Mult => "imul",
            Self::And => "and",
            Self::Or => "or",
            Self::Xor => "xor",
            Self::ShiftLeft => "sal",
            Self::ShiftRight => "sar",
        })
    }
}
impl From<UnOp> for Unary {
    fn from(op: UnOp) -> Self {
        match op {
            UnOp::Negate => Self::Neg,
            UnOp::Complement => Self::Not,
            // PROBABLY BAD
            UnOp::Not => unreachable!(),
        }
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Imm(val) => write!(f, "${val}"),
            Self::Register(r) => write!(f, "{}", r.extended()),
            Self::Stack(n) => write!(f, "{n}(%rbp)"),
            Self::Data(name) => write!(f, "_{name}(%rip)"),
        }
    }
}

impl Op {
    fn sized_fmt(&self, size: AsmType) -> String {
        if let Self::Register(r) = self {
            match size {
                AsmType::Longword => r.extended().into(),
                AsmType::Quadword => r.eight_byte().into(),
            }
        } else {
            format!("{self}")
        }
    }
}

#[derive(Clone, Debug)]
pub enum PseudoOp {
    Normal(Op),
    PseudoRegister(Ident),
}
impl From<Value> for PseudoOp {
    fn from(val: Value) -> Self {
        match val {
            Value::Constant(c) => Self::imm(c.long()),

            Value::Var(v) => Self::PseudoRegister(v),
        }
    }
}

impl From<Ident> for PseudoOp {
    fn from(ident: Ident) -> Self {
        Self::PseudoRegister(ident.clone())
    }
}

impl<T: Into<Op>> From<T> for PseudoOp {
    fn from(val: T) -> Self {
        let operand: Op = val.into();
        match operand {
            Op::Imm(a) => Self::imm(a),
            Op::Register(a) => Self::register(a),
            Op::Stack(a) => Self::stack(a),
            Op::Data(a) => Self::data(a),
        }
    }
}

#[allow(dead_code)]
pub mod pseudo_regs {
    use super::op_regs as regs;
    use super::PseudoOp as Psu;
    pub const AX: Psu = Psu::Normal(regs::AX);
    pub const CX: Psu = Psu::Normal(regs::CX);
    pub const DX: Psu = Psu::Normal(regs::DX);
    pub const DI: Psu = Psu::Normal(regs::DI);
    pub const SI: Psu = Psu::Normal(regs::SI);
    pub const R8: Psu = Psu::Normal(regs::R8);
    pub const R9: Psu = Psu::Normal(regs::R9);
    pub const R10: Psu = Psu::Normal(regs::R10);
    pub const R11: Psu = Psu::Normal(regs::R11);
    pub const SP: Psu = Psu::Normal(regs::SP);
}

impl PseudoOp {
    pub const SYSV_ARG_REGS: [Register; 6] = [
        Register::Di,
        Register::Si,
        Register::Dx,
        Register::Cx,
        Register::R8,
        Register::R9,
    ];

    pub const fn imm(value: i64) -> Self {
        Self::Normal(Op::Imm(value))
    }
    pub const fn register(reg: Register) -> Self {
        Self::Normal(Op::Register(reg))
    }

    pub const fn data(name: Ident) -> Self {
        Self::Normal(Op::Data(name))
    }

    pub const fn stack(offset: isize) -> Self {
        Self::Normal(Op::Stack(offset))
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
    Sp,
}
#[derive(Debug)]
pub struct Program<T>(pub Box<[TopLevel<T>]>);

#[derive(Debug)]
pub enum TopLevel<T> {
    Fn(FunctionDefinition<T>),
    StaticVar(StaticVar),
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

impl<T> TopLevel<T> {
    const fn global(&self) -> bool {
        match self {
            Self::StaticVar(StaticVar { global, .. })
            | Self::Fn(FunctionDefinition { global, .. }) => *global,
        }
    }

    const fn name(&self) -> &Ident {
        match self {
            Self::StaticVar(StaticVar { name, .. }) | Self::Fn(FunctionDefinition { name, .. }) => {
                name
            }
        }
    }
}

#[derive(Debug)]
pub struct StaticVar {
    pub name: Ident,
    pub global: bool,
    pub alignment: u32,
    pub init: StaticInit,
}

#[derive(Debug)]
pub struct FunctionDefinition<T> {
    pub name: Ident,
    // empty for x86, which kinda sucks,
    pub params: Box<[Ident]>,
    pub global: bool,
    pub body: Box<[T]>,
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
