#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnType {
    pub ret: Option<VarType>,
    pub params: Box<[VarType]>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum VarType {
    Int,
    Long,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Ord, PartialOrd)]
pub enum Constant {
    Int(i32),
    UInt(u32),
    Long(i64),
    ULong(u64),
}

#[derive(Debug, Copy, Clone)]
pub enum StaticInit {
    Int(i32),
    Long(i64),
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
