use symtab::Key;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnType {
    pub ret: Option<VarType>,
    pub params: Box<[VarType]>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum VarType {
    Int,
    UInt,
    Long,
    ULong,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Ord, PartialOrd)]
pub enum Constant {
    Int(i32),
    UInt(u32),
    Long(i64),
    ULong(u64),
}

impl Constant {
    pub const fn ty(&self) -> VarType {
        match self {
            Self::Int(_) => VarType::Int,
            Self::UInt(_) => VarType::UInt,
            Self::Long(_) => VarType::Long,
            Self::ULong(_) => VarType::ULong,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum StaticInit {
    Int(i32),
    Long(i64),
}

#[derive(Debug, Copy, Clone)]
pub enum InitialVal {
    Tentative,
    Init(StaticInit),
}

impl InitialVal {
    pub const fn get_static(&self, ty: VarType) -> StaticInit {
        if let Self::Init(s) = self {
            *s
        } else {
            match ty {
                VarType::Int => StaticInit::Int(0),
                VarType::Long => StaticInit::Long(0),
                _ => todo!(),
            }
        }
    }
}

impl From<&Option<StaticInit>> for InitialVal {
    fn from(val: &Option<StaticInit>) -> Self {
        match val {
            Some(s) => Self::from(s),
            None => Self::Tentative,
        }
    }
}

impl From<&StaticInit> for InitialVal {
    fn from(v: &StaticInit) -> Self {
        Self::Init(*v)
    }
}

impl From<Constant> for StaticInit {
    fn from(c: Constant) -> Self {
        match c {
            Constant::Int(i) => Self::Int(i),
            Constant::UInt(ui) => Self::Int(ui as i32),
            Constant::Long(l) => Self::Long(l),
            Constant::ULong(ul) => Self::Int(ul as i32),
        }
    }
}

use std::fmt::{Display, Formatter, Result};

impl Display for Constant {
    fn fmt(&self, f: &mut Formatter) -> Result {
        use Constant::{Int, Long, UInt, ULong};
        match self {
            Int(c) => write!(f, "{c}"),
            UInt(c) => write!(f, "{c}"),
            Long(c) => write!(f, "{c}"),
            ULong(c) => write!(f, "{c}"),
        }
    }
}

impl Display for StaticInit {
    fn fmt(&self, f: &mut Formatter) -> Result {
        use StaticInit::{Int, Long};
        match self {
            Int(c) => write!(f, "{c}"),
            Long(c) => write!(f, "{c}"),
        }
    }
}

pub type ParamList<'a> = Box<[Param<'a>]>;

#[derive(Debug, Clone)]
pub struct Param<'a> {
    pub typ: VarType,
    pub name: Key<'a>,
}

impl VarType {
    pub const fn common_type(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (Self::Long, Self::Long) | (Self::Long, Self::Int) | (Self::Int, Self::Long) => {
                Some(Self::Long)
            }
            (Self::Int, Self::Int) => Some(Self::Int),
            _ => todo!(),
        }
    }

    pub const fn alignment(&self) -> u32 {
        match self {
            Self::Int | Self::UInt => 4,
            Self::Long | Self::ULong => 8,
        }
    }
}
