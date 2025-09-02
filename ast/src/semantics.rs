pub mod labeled;
pub mod typed;
use crate::parse::StaticInit;
use crate::Constant;
use crate::Ident;
use crate::{parse::FnType, VarType};
use std::collections::HashMap;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct LabelId(pub usize);

#[derive(Debug, Clone)]
pub struct StatementLabels {
    pub start: Ident,
    pub r#break: Ident,
    pub r#continue: Ident,
    pub end: Ident,
}
impl LabelId {
    pub fn labels(&self) -> StatementLabels {
        StatementLabels {
            start: Ident::from(format!("s{}s", self.0)),
            r#break: self.r#break(),
            r#continue: self.r#continue(),
            end: Ident::from(format!("s{}e", self.0)),
        }
    }

    pub fn r#break(&self) -> Ident {
        Ident::from(format!("s{}b", self.0))
    }

    pub fn case(&self, case: &typed::Case) -> Ident {
        Ident::from(format!("sc{}{case}", self.0))
    }
    pub fn default(&self) -> Ident {
        Ident::from(format!("sc{}d", self.0))
    }
    pub fn r#continue(&self) -> Ident {
        Ident::from(format!("s{}c", self.0))
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Label {
    Named(Ident),
    Default(LabelId),
    Case { c: Constant, id: LabelId },
}

pub type SymbolTable = HashMap<Ident, Attr>;
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

#[derive(Debug)]
pub enum Expected {
    VarType,
    FnType,
}

impl Attr {
    pub const fn global(&self) -> bool {
        match self {
            Self::Static { global, .. } | Self::Fn { global, .. } => *global,
            Self::Automatic(_) => false,
        }
    }

    pub const fn var_type(&self) -> Result<&VarType, Expected> {
        match self {
            Self::Static { typ, .. } | Self::Automatic(typ) => Ok(typ),
            Self::Fn { .. } => Err(Expected::VarType),
        }
    }

    pub const fn fn_type(&self) -> Result<&FnType, Expected> {
        match self {
            Self::Fn { typ, .. } => Ok(typ),
            Self::Static { .. } | Self::Automatic(_) => Err(Expected::FnType),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum InitialVal {
    Tentative,
    Initial(StaticInit),
}

impl InitialVal {
    pub const fn get_static(&self, ty: VarType) -> StaticInit {
        match (self, ty) {
            (InitialVal::Initial(s), _) => *s,
            (InitialVal::Tentative, VarType::Int) => StaticInit::Int(0),
            (InitialVal::Tentative, VarType::Long) => StaticInit::Long(0),
        }
    }

    pub fn as_long(&self) -> i64 {
        match self {
            Self::Initial(StaticInit::Long(i)) => *i,
            Self::Initial(StaticInit::Int(i)) => (*i).into(),
            Self::Tentative => 0,
        }
    }

    pub const fn as_int(&self) -> i32 {
        match self {
            Self::Initial(StaticInit::Int(i)) => *i,
            Self::Initial(StaticInit::Long(i)) => *i as i32,
            Self::Tentative => 0,
        }
    }
}
