use super::tacky::Value;
use ast::parse::{StaticInit, UnOp};
use ast::semantics::Attr;
use ast::Ident;
use ast::VarType;
use ast::{Int, Long};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};
use std::io::Write;
pub type BackendTable = HashMap<Ident, BackendSymbol>;

const fn asm_type(ty: VarType) -> AsmType {
    match ty {
        VarType::Int(_) => AsmType::Longword,
        VarType::Long(_) => AsmType::Quadword,
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

impl BackendSymbol {
    fn ty(&self) -> Option<AsmType> {
        match self {
            BackendSymbol::Obj { ty, .. } => Some(*ty),
            BackendSymbol::Fn { .. } => None,
        }
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
    Div {
        divisor: T,
        ty: AsmType,
    },
    MovZeroExtend(OpPair<T>),
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
    A,
    AE,
    B,
    BE,
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
                Self::A => "a",
                Self::AE => "ae",
                Self::B => "b",
                Self::BE => "be",
            }
        )
    }
}

impl BaseX86<PseudoOp> {
    pub const fn allocate_stack(n: i64) -> Self {
        Self::binary(
            Binary::Sub,
            Op::imm(n).pseudo(),
            Op::Register(Register::Sp).pseudo(),
            AsmType::Quadword,
        )
    }

    pub const fn deallocate_stack(n: i64) -> Self {
        Self::binary(
            Binary::Add,
            Op::imm(n).pseudo(),
            Op::Register(Register::Sp).pseudo(),
            AsmType::Quadword,
        )
    }

    pub fn as_fixed(&self, regs: &[Op]) -> BaseX86<Op> {
        // not sure how I feel about this interface but it's deduplicated
        let left = regs.get(0).cloned();
        let right = regs.get(1).cloned();

        match self {
            BaseX86::Mov { ty, .. } => BaseX86::Mov {
                ty: *ty,
                regs: (left.unwrap(), right.unwrap()),
            },
            BaseX86::Movsx { ty, .. } => BaseX86::Movsx {
                ty: *ty,
                regs: (left.unwrap(), right.unwrap()),
            },
            BaseX86::Binary { operator, ty, .. } => BaseX86::Binary {
                operator: *operator,
                ty: *ty,
                regs: (left.unwrap(), right.unwrap()),
            },
            BaseX86::Unary { operator, ty, .. } => BaseX86::Unary {
                operator: *operator,
                operand: left.unwrap(),
                ty: *ty,
            },
            BaseX86::Idiv { ty, .. } => BaseX86::Idiv {
                divisor: left.unwrap(),
                ty: *ty,
            },
            BaseX86::Div { ty, .. } => BaseX86::Div {
                divisor: left.unwrap(),
                ty: *ty,
            },
            BaseX86::MovZeroExtend(_) => BaseX86::MovZeroExtend((left.unwrap(), right.unwrap())),
            BaseX86::Cmp { ty, .. } => BaseX86::Cmp {
                regs: (left.unwrap(), right.unwrap()),
                ty: *ty,
            },

            BaseX86::Push(_) => BaseX86::Push(left.unwrap()),
            BaseX86::Call(l) => BaseX86::Call(l.clone()),
            BaseX86::Ret => BaseX86::Ret,
            BaseX86::Cdq(asm_type) => BaseX86::Cdq(*asm_type),
            BaseX86::Jmp(l) => BaseX86::Jmp(l.clone()),
            BaseX86::JmpCC { condition, label } => BaseX86::JmpCC {
                condition: condition.clone(),
                label: label.clone(),
            },
            BaseX86::SetCC { condition, .. } => BaseX86::SetCC {
                condition: condition.clone(),
                op: left.unwrap(),
            },
            BaseX86::Label(l) => BaseX86::Label(l.clone()),
        }
    }
}

impl BaseX86<Op> {
    pub const fn allocate_stack(n: i64) -> Self {
        Self::binary(
            Binary::Sub,
            Op::imm(n),
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

    pub const fn div(divisor: T, ty: AsmType) -> Self {
        Self::Div { divisor, ty }
    }

    pub const fn movsx(src: T, dst: T) -> Self {
        Self::Movsx {
            ty: AsmType::Quadword,
            regs: (src, dst),
        }
    }

    pub const fn mov_zero_extend(src: T, dst: T) -> Self {
        Self::MovZeroExtend((src, dst))
    }

    pub fn map<
        U,
        F: FnOnce(T, T, Option<Binary>, AsmType) -> U,
        G: FnOnce(T, Option<Unary>, AsmType) -> U,
        H: FnOnce() -> U,
    >(
        self,
        bin_map: F,
        unary_map: G,
        other_map: H,
    ) -> U {
        match self {
            BaseX86::Mov { ty, regs } | BaseX86::Movsx { ty, regs } | BaseX86::Cmp { ty, regs } => {
                bin_map(regs.0, regs.1, None, ty)
            }
            BaseX86::MovZeroExtend((l, r)) => bin_map(l, r, None, AsmType::Longword),
            BaseX86::Binary { operator, regs, ty } => bin_map(regs.0, regs.1, Some(operator), ty),
            BaseX86::Idiv { divisor, ty } | BaseX86::Div { divisor, ty } => {
                unary_map(divisor, None, ty)
            }
            BaseX86::Unary {
                operator,
                operand,
                ty,
            } => unary_map(operand, Some(operator), ty),
            BaseX86::SetCC { op, .. } => unary_map(op, None, AsmType::Quadword),
            BaseX86::Push(op) => unary_map(op, None, AsmType::Quadword),
            BaseX86::Call(_)
            | BaseX86::Ret
            | BaseX86::Cdq(_)
            | BaseX86::Jmp(_)
            | BaseX86::JmpCC { .. }
            | BaseX86::Label(_) => other_map(),
        }
    }

    pub fn regs(&self) -> Box<[&T]> {
        match self {
            BaseX86::Mov { regs: (l, r), .. }
            | BaseX86::Movsx { regs: (l, r), .. }
            | BaseX86::Binary { regs: (l, r), .. }
            | BaseX86::MovZeroExtend((l, r))
            | BaseX86::Cmp { regs: (l, r), .. } => Box::new([l, r]),

            BaseX86::Idiv { divisor: l, .. }
            | BaseX86::Div { divisor: l, .. }
            | BaseX86::Push(l)
            | BaseX86::SetCC { op: l, .. }
            | BaseX86::Unary { operand: l, .. } => Box::new([l]),

            BaseX86::Cdq(_)
            | BaseX86::Call(_)
            | BaseX86::Ret
            | BaseX86::Jmp(_)
            | BaseX86::JmpCC { .. }
            | BaseX86::Label(_) => Box::new([]),
        }
    }

    pub const fn ty(&self) -> Option<AsmType> {
        match self {
            BaseX86::Mov { ty, .. }
            | BaseX86::Movsx { ty, .. }
            | BaseX86::Unary { ty, .. }
            | BaseX86::Binary { ty, .. }
            | BaseX86::Idiv { ty, .. }
            | BaseX86::Div { ty, .. }
            | BaseX86::Cdq(ty)
            | BaseX86::Cmp { ty, .. } => Some(*ty),

            BaseX86::Push(_) => Some(AsmType::Quadword),
            BaseX86::MovZeroExtend(_) => Some(AsmType::Longword),

            BaseX86::Call(_)
            | BaseX86::Ret
            | BaseX86::Jmp(_)
            | BaseX86::JmpCC { .. }
            | BaseX86::SetCC { .. }
            | BaseX86::Label(_) => None,
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

struct TargettedX86<'a> {
    inner: &'a X86,
    target: Target,
}

impl Display for TargettedX86<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let target = self.target;
        let fn_suffix = target.function_suffix().unwrap_or_default();
        let fn_prefix = target.function_prefix().unwrap_or_default();
        let local_prefix = target.local_label_prefix();
        match self.inner {
            X86::Call(fun) => write!(f, "call {fn_prefix}{fun}{fn_suffix}"),
            X86::Push(Op::Register(r)) => write!(f, "pushq {}", r.eight_byte()),
            X86::Push(op) => write!(f, "pushq {op}"),
            X86::Mov {
                regs: (src, dst),
                ty,
            } => {
                write!(
                    f,
                    "mov{ty}  {}, {}",
                    src.sized_fmt(*ty, target),
                    dst.sized_fmt(*ty, target)
                )
            }
            X86::Ret => write!(f, "movq %rbp, %rsp\n\tpopq %rbp\n\tret"),
            X86::Unary {
                operator,
                operand,
                ty,
            } => write!(f, "{operator}{ty} {}", operand.sized_fmt(*ty, target)),
            X86::Binary {
                operator: operator @ (Binary::ShiftLeft | Binary::ShiftRight),
                regs: (Op::Register(by), dst),
                ty,
            } => {
                write!(
                    f,
                    "{operator}{ty} {}, {}",
                    by.one_byte(),
                    dst.sized_fmt(*ty, target),
                )
            }
            X86::Binary {
                operator: operator @ (Binary::ShiftLeft | Binary::ShiftRight),
                regs: (by, dst),
                ty,
            } => {
                write!(
                    f,
                    "{operator}{ty} {}, {}",
                    by.sized_fmt(*ty, target),
                    dst.sized_fmt(*ty, target),
                )
            }
            X86::Binary {
                operator,
                regs: (op, dst_op),
                ty,
            } => {
                write!(
                    f,
                    "{operator}{ty} {}, {}",
                    op.sized_fmt(*ty, target),
                    dst_op.sized_fmt(*ty, target)
                )
            }
            X86::Idiv { divisor, ty } => {
                write!(f, "idiv{ty} {}", divisor.sized_fmt(*ty, target))
            }
            X86::Cdq(AsmType::Longword) => {
                write!(f, "cdq")
            }
            X86::Cdq(AsmType::Quadword) => {
                write!(f, "cqo")
            }
            X86::Cmp {
                regs: (left, right),
                ty,
            } => {
                write!(
                    f,
                    "cmp{ty} {}, {}",
                    left.sized_fmt(*ty, target),
                    right.sized_fmt(*ty, target)
                )
            }
            X86::Jmp(label) => {
                write!(f, "jmp {local_prefix}{label}")
            }
            X86::JmpCC { label, condition } => {
                write!(f, "j{condition} {local_prefix}{label}")
            }
            X86::SetCC { op, condition } => {
                write!(f, "set{condition} {op}")
            }
            X86::Label(label) => {
                write!(f, "{local_prefix}{label}:")
            }
            X86::Movsx {
                regs: (src, dst),
                ty,
            } => {
                write!(
                    f,
                    "movslq {}, {}",
                    src.sized_fmt(AsmType::Longword, target),
                    dst.sized_fmt(*ty, target)
                )
            }
            BaseX86::Div { divisor, ty } => {
                write!(f, "div{ty} {}", divisor.sized_fmt(*ty, target))
            }

            BaseX86::MovZeroExtend((l, r)) => write!(
                f,
                "movzx {}, {}",
                l.sized_fmt(AsmType::Longword, target),
                r.sized_fmt(AsmType::Quadword, target),
            ),
        }
    }
}

impl Op {
    pub const fn pseudo(self) -> PseudoOp {
        PseudoOp::Normal(self)
    }

    pub const fn is_register(&self) -> bool {
        matches!(self, Self::Register(_))
    }

    pub const fn is_memory(&self) -> bool {
        matches!(self, Self::Data(_) | Self::Stack(_))
    }

    pub const fn is_immediate(&self) -> bool {
        matches!(self, Self::Imm(_))
    }

    pub const fn imm(imm: i64) -> Self {
        Self::Imm(Immediate::new_i64(imm))
    }

    pub const fn uimm(imm: u64) -> Self {
        Self::Imm(Immediate::new_u64(imm))
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
    Imm(Immediate),
    Register(Register),
    Stack(isize),
    Data(Ident),
}

#[repr(C)]
pub union Immediate {
    long: i64,
    ulong: u64,
    int: i32,
    uint: u32,
}

impl Clone for Immediate {
    fn clone(&self) -> Self {
        Self {
            long: self.as_long(),
        }
    }
}

impl Copy for Immediate {}

impl fmt::Debug for Immediate {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        <u64 as fmt::Debug>::fmt(&self.as_ulong(), f)
    }
}

impl fmt::Display for Immediate {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        <u64 as fmt::Display>::fmt(&self.as_ulong(), f)
    }
}

impl Ord for Immediate {
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_long().cmp(&other.as_long())
    }
}

impl PartialOrd<i64> for Immediate {
    fn partial_cmp(&self, other: &i64) -> Option<Ordering> {
        Some(<i64 as Ord>::cmp(&self.as_long(), other))
    }
}

impl PartialOrd for Immediate {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(<Self as Ord>::cmp(&self, other))
    }
}

impl Eq for Immediate {}

impl PartialEq for Immediate {
    fn eq(&self, other: &Self) -> bool {
        self.as_ulong() == other.as_ulong()
    }
}

impl PartialEq<i64> for Immediate {
    fn eq(&self, other: &i64) -> bool {
        self.as_long() == *other
    }
}

impl Immediate {
    pub fn above_dword(&self, target: &Target) -> bool {
        self.as_ulong() > i32::MAX as u64
    }

    pub const fn as_long(&self) -> i64 {
        unsafe { self.long }
    }

    pub const fn as_ulong(&self) -> u64 {
        unsafe { self.ulong }
    }

    pub const fn as_int(&self) -> i32 {
        unsafe { self.int }
    }

    pub const fn as_uint(&self) -> u32 {
        unsafe { self.uint }
    }

    pub const fn new_int(int: Int) -> Self {
        match int {
            Int::U(u) => Self::new_u32(u),
            Int::I(i) => Self::new_i32(i),
        }
    }

    pub const fn new_long(long: Long) -> Self {
        match long {
            Long::U(u) => Self::new_u64(u),
            Long::I(i) => Self::new_i64(i),
        }
    }

    pub const fn new_i64(long: i64) -> Self {
        Self { long: long }
    }

    pub const fn new_u64(long: u64) -> Self {
        Self { ulong: long }
    }

    pub const fn new_u32(int: u32) -> Self {
        Self::new_u64(int as u64)
    }

    pub const fn new_i32(int: i32) -> Self {
        Self::new_i64(int as i64)
    }

    pub const fn set_int(&mut self, int: &Int) {
        match int {
            Int::U(u) => self.set_u32(*u),
            Int::I(i) => self.set_i32(*i),
        }
    }

    pub const fn set_long(&mut self, long: &Long) {
        match long {
            Long::U(u) => self.set_u64(*u),
            Long::I(i) => self.set_i64(*i),
        }
    }

    pub const fn set_i32(&mut self, int: i32) {
        self.set_i64(int as i64);
    }

    pub const fn set_u32(&mut self, int: u32) {
        self.set_u64(int as u64);
    }

    pub const fn set_i64(&mut self, long: i64) {
        self.long = long
    }

    pub const fn set_u64(&mut self, long: u64) {
        self.ulong = long
    }
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
            Self::Data(name) => write!(f, "{name}(%rip)"),
        }
    }
}

impl Op {
    fn sized_fmt(&self, size: AsmType, target: Target) -> String {
        if let Self::Register(r) = self {
            match size {
                AsmType::Longword => r.extended().into(),
                AsmType::Quadword => r.eight_byte().into(),
            }
        } else {
            format!(
                "{}{self}",
                if let Self::Data(_) = self
                    && let Some(prefix) = target.user_defined_name_prefix()
                {
                    prefix
                } else {
                    ""
                }
            )
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
            Value::Constant(c) => match c.long() {
                l if l.is_signed() => Self::imm(l.signed()),
                ul => Self::uimm(ul.unsigned()),
            },

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
            Op::Imm(a) => Self::imm(a.as_long()),
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
        Self::Normal(Op::imm(value))
    }

    pub const fn uimm(value: u64) -> Self {
        Self::Normal(Op::uimm(value))
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

    pub const fn is_register(&self) -> bool {
        match self {
            PseudoOp::Normal(op) => op.is_register(),
            PseudoOp::PseudoRegister(_) => false,
        }
    }

    pub const fn is_memory(&self) -> bool {
        match self {
            PseudoOp::Normal(op) => op.is_memory(),
            PseudoOp::PseudoRegister(_) => true,
        }
    }

    pub const fn is_immediate(&self) -> bool {
        match self {
            PseudoOp::Normal(op) => op.is_immediate(),
            PseudoOp::PseudoRegister(_) => false,
        }
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

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Target {
    Darwin,
    Linux,
}

impl Target {
    const fn alignment_directive(&self) -> &str {
        match self {
            Target::Darwin => ".balign",
            Target::Linux => ".align",
        }
    }

    const fn end_message(&self) -> Option<&str> {
        match self {
            Target::Darwin => None,
            Target::Linux => Some(".section .note.GNU-stack,\"\",@progbits"),
        }
    }

    const fn function_prefix(&self) -> Option<&str> {
        if let Self::Darwin = self {
            Some(unsafe { self.user_defined_name_prefix().unwrap_unchecked() })
        } else {
            None
        }
    }

    const fn function_suffix(&self) -> Option<&str> {
        if let Self::Linux = self {
            Some("@PLT")
        } else {
            None
        }
    }

    const fn local_label_prefix(&self) -> &str {
        match self {
            Target::Darwin => "L",
            Target::Linux => ".L",
        }
    }

    const fn user_defined_name_prefix(&self) -> Option<&str> {
        match self {
            Target::Darwin => Some("_"),
            Target::Linux => None,
        }
    }
}

pub fn emit(program: &Program<X86>, target: Target) -> Box<[u8]> {
    let mut bytes = Vec::new();
    let name_prefix = target.user_defined_name_prefix().unwrap_or_default();
    let alignment_dir = target.alignment_directive();
    for top_level in &program.0 {
        if top_level.global() {
            let _ = writeln!(bytes, "\t.globl {name_prefix}{}", top_level.name());
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
                    name = format_args!("{name_prefix}{}", name),
                    prelude = format_args!("pushq %rbp\n\tmovq %rsp, %rbp")
                );
                for instruction in body.iter().map(|inner| TargettedX86 { inner, target }) {
                    let _ = writeln!(bytes, "\t{instruction}");
                }
            }

            TopLevel::StaticVar(StaticVar {
                name,
                global: _,
                init,
                alignment,
            }) => {
                let init_is_zero = init.is_zero();

                bytes.extend_from_slice(if init_is_zero {
                    b"\t.bss\n"
                } else {
                    b"\t.data\n"
                });
                let _ = writeln!(bytes, "\t{alignment_dir} {}\n", alignment);

                let _ = writeln!(bytes, "{name_prefix}{name}:\n \t {init}");
            }
        }
    }

    if let Some(suffix) = target.end_message() {
        let _ = writeln!(bytes, "{suffix}");
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
