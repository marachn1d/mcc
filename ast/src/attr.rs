use super::{
    c_vals::{FnType, InitialVal, VarType},
    Key,
};
use std::collections::HashMap;

use std::collections::hash_map::Entry;

// TODO: do like a transform/collect to turn it into a hashmap of either TackyVar or TackyTmp
pub type SymbolTable<'a> = HashMap<Key<'a>, Attr>;
#[derive(Debug)]
pub enum Attr {
    Static {
        typ: VarType,
        init: Option<InitialVal>,
        global: bool,
    },
    Automatic(VarType),
    Fn {
        defined: bool,
        global: bool,
        typ: FnType,
    },
}

pub enum ExpectedType {
    Var,
    Fn,
}

impl Attr {
    pub const fn global(&self) -> bool {
        match self {
            Self::Static { global, .. } | Self::Fn { global, .. } => *global,
            Self::Automatic(_) => false,
        }
    }

    pub const fn var_type(&self) -> Result<&VarType, ExpectedType> {
        match self {
            Self::Static { typ, .. } | Self::Automatic(typ) => Ok(typ),
            Self::Fn { .. } => Err(ExpectedType::Var),
        }
    }

    pub const fn fn_type(&self) -> Result<&FnType, ExpectedType> {
        match self {
            Self::Fn { typ, .. } => Ok(typ),
            Self::Static { .. } | Self::Automatic(_) => Err(ExpectedType::Fn),
        }
    }
}
