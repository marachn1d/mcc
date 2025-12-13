use ast::VarType;
use ast::parse::{StaticInit, UnOp};
use ast::{Constant, Ident};
use std::fmt;

//pub type Program = super::Program<Instruction>;
//pub type FunctionDefinition = super::FunctionDefinition<Instruction>;

pub struct Program(pub Box<[TopLevel]>);

impl fmt::Debug for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for tl in &self.0 {
            writeln!(f, "{tl:?}")?;
        }
        Ok(())
    }
}

pub enum TopLevel {
    Fn(FunctionDefinition),
    StaticVar(StaticVar),
}

impl fmt::Debug for TopLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Fn(fun) => <FunctionDefinition as fmt::Debug>::fmt(fun, f),
            Self::StaticVar(sv) => <StaticVar as fmt::Debug>::fmt(sv, f),
        }
    }
}

pub struct StaticVar {
    pub name: Ident,
    pub global: bool,
    pub init: StaticInit,
    pub typ: VarType,
}

impl fmt::Debug for StaticVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {ty :?} {name:?} = {init:?}",
            if self.global { "global" } else { "" },
            name = self.name,
            ty = self.typ,
            init = self.init,
        )
    }
}

pub struct FunctionDefinition {
    pub name: Ident,
    // empty for x86, which kinda sucks,
    pub params: Box<[Ident]>,
    pub global: bool,
    pub body: Box<[Instruction]>,
}

impl fmt::Debug for FunctionDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use fmt::Write;
        write!(
            f,
            "{} fn {name:?}(",
            if self.global { "global" } else { "" },
            name = self.name
        )?;
        f.debug_list().entries(&self.params).finish()?;
        write!(f, "){{\n")?;
        for instruction in &self.body {
            f.write_char('\t')?;
            if !matches!(instruction, Instruction::Label(_)) {
                f.write_char('\t')?;
            }
            writeln!(f, "{instruction:?}")?;
        }
        f.write_char('}')
    }
}

pub enum Instruction {
    SignExtend {
        src: Value,
        dst: Value,
    },

    ZeroExtend {
        src: Value,
        dst: Value,
    },

    Truncate {
        src: Value,
        dst: Value,
    },
    Return(Value),
    Unary {
        op: UnOp,
        source: Value,
        dst: Ident,
    },
    Binary {
        operator: TackyBinary,
        source_1: Value,
        source_2: Value,
        dst: Value,
    },
    Copy {
        src: Value,
        dst: Value,
    },
    Jump {
        target: Ident,
    },
    JumpIfZero {
        condition: Value,
        target: Ident,
    },
    JumpIfNotZero {
        condition: Value,
        target: Ident,
    },
    Label(Ident),
    FunCall {
        name: Ident,
        args: Box<[Value]>,
        dst: Value,
    },
}

impl fmt::Debug for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use fmt::Write;
        match self {
            Self::SignExtend { src, dst } => write!(f, "{dst:?} = sx({src:?})"),
            Self::ZeroExtend { src, dst } => write!(f, "{dst:?} = zx({src:?})"),
            Self::Truncate { src, dst } => write!(f, "{dst:?} = trunc({src:?})"),
            Self::Return(val) => write!(f, "ret {val:?}"),
            Self::Unary { op, source, dst } => write!(f, "{dst:?} = {op:?}{source:?}"),
            Self::Binary {
                operator,
                source_1,
                source_2,
                dst,
            } => write!(f, "{dst:?} = {source_1:?} {operator:?} {source_2:?}"),
            Self::Copy { src, dst } => write!(f, "{dst:?} = {src:?}"),
            Self::Jump { target } => write!(f, "jump({target})"),
            Self::JumpIfZero { condition, target } => write!(f, "jz({condition:?}, {target})"),
            Self::JumpIfNotZero { condition, target } => write!(f, "jnz({condition:?}, {target})"),
            Self::Label(label) => write!(f, "{label:?}:"),
            Self::FunCall { name, args, dst } => {
                write!(f, "{dst:?} = {name}(")?;
                f.debug_list().entries(args).finish()?;
                f.write_char(')')
            }
        }
    }
}

#[derive(Clone)]
pub enum Value {
    Constant(Constant),
    Var(Ident),
}

impl Value {
    pub const fn constant(&self) -> Option<&Constant> {
        if let Self::Constant(s) = self {
            Some(s)
        } else {
            None
        }
    }
    pub const fn var(&self) -> Option<&Ident> {
        if let Self::Var(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Constant(c) => <Constant as fmt::Debug>::fmt(c, f),
            Self::Var(v) => <String as fmt::Display>::fmt(v, f),
        }
    }
}

#[derive(Eq, PartialEq, Copy, Clone)]
pub enum TackyBinary {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    BitAnd,
    BitOr,
    Xor,
    LeftShift,
    RightShift,
    EqualTo,
    NotEqual,
    LessThan,
    GreaterThan,
    Leq,
    Geq,
}

impl fmt::Debug for TackyBinary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str = match self {
            Self::Add => "+",
            Self::Subtract => "-",
            Self::Multiply => "*",
            Self::Divide => "/",
            Self::Remainder => "%",
            Self::BitAnd => "&",
            Self::BitOr => "|",
            Self::Xor => "^",
            Self::LeftShift => "<<",
            Self::RightShift => ">>",
            Self::EqualTo => "==",
            Self::NotEqual => "!=",
            Self::LessThan => "<",
            Self::GreaterThan => ">",
            Self::Leq => "<=",
            Self::Geq => ">=",
        };
        f.write_str(str)
    }
}

use std::num::Wrapping;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Shl, Shr, Sub};

impl TackyBinary {
    // most evil function i've ever written
    pub fn apply<T>(&self, lhs: T, rhs: T) -> T
    where
        Wrapping<T>: Add<Output = Wrapping<T>>
            + Sub<Output = Wrapping<T>>
            + Mul<Output = Wrapping<T>>
            + Div<Output = Wrapping<T>>
            + Rem<Output = Wrapping<T>>
            + BitAnd<Output = Wrapping<T>>
            + BitOr<Output = Wrapping<T>>
            + BitXor<Output = Wrapping<T>>
            + Shl<usize, Output = Wrapping<T>>
            + Shr<usize, Output = Wrapping<T>>,
        T: From<u8> + Ord + TryInto<usize>,
    {
        let (lhs, rhs) = (Wrapping(lhs), Wrapping(rhs));
        let truee = || <u8 as Into<T>>::into(1);
        let falsee = || <u8 as Into<T>>::into(0);
        let zero = || Wrapping(falsee());
        match self {
            TackyBinary::Add => (lhs + rhs).0,
            TackyBinary::Subtract => (lhs - rhs).0,
            TackyBinary::Multiply => (lhs * rhs).0,
            TackyBinary::Divide => {
                if rhs != zero() {
                    (lhs / rhs).0
                } else {
                    zero().0
                }
            }
            TackyBinary::Remainder => (lhs % rhs).0,
            TackyBinary::BitAnd => (lhs & rhs).0,
            TackyBinary::BitOr => (lhs | rhs).0,
            TackyBinary::Xor => (lhs ^ rhs).0,
            // UNDEFINED BEHAVIOUR HERE: IF THEY TRY TO BITSHIFT BY A NEGATIVE NUMBER WE GIVE THEM
            // ZERO
            TackyBinary::LeftShift => (lhs << rhs.0.try_into().unwrap_or(0)).0,
            TackyBinary::RightShift => (lhs >> rhs.0.try_into().unwrap_or(0)).0,
            TackyBinary::LessThan => {
                if lhs < rhs {
                    truee()
                } else {
                    falsee()
                }
            }
            TackyBinary::GreaterThan => {
                if lhs > rhs {
                    truee()
                } else {
                    falsee()
                }
            }
            TackyBinary::Leq => {
                if lhs <= rhs {
                    truee()
                } else {
                    falsee()
                }
            }
            TackyBinary::Geq => {
                if lhs >= rhs {
                    truee()
                } else {
                    falsee()
                }
            }
            TackyBinary::EqualTo => {
                if lhs == rhs {
                    truee()
                } else {
                    falsee()
                }
            }
            TackyBinary::NotEqual => {
                if lhs != rhs {
                    truee()
                } else {
                    falsee()
                }
            }
        }
    }
}
