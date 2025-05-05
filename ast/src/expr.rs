use crate::prelude::*;

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Assignment {
        dst: Box<Self>,
        src: Box<Self>,
    },
    Bin(Binary<'a>),
    Cast {
        target: VarType,
        exp: Box<Self>,
    },

    //factors
    IncDec {
        op: IncDec,
        exp: Box<Self>,
    },

    Var(Key<'a>),
    Const(Constant),
    Unary(Unary<'a>),
    Nested(Box<Self>),

    Conditional {
        condition: Box<Self>,
        r#true: Box<Self>,
        r#false: Box<Self>,
    },
    FunctionCall {
        name: Key<'a>,
        args: Box<[Self]>,
    },
}

#[derive(Debug, Clone)]
pub struct Unary<'a> {
    pub exp: Box<Expr<'a>>,
    pub op: UnOp,
}

#[derive(Debug, Clone)]
pub struct Binary<'a> {
    pub operator: Bop,
    pub left: Box<Expr<'a>>,
    pub right: Box<Expr<'a>>,
}

impl Expr<'_> {
    pub fn pre_inc(e: Self) -> Self {
        Self::IncDec {
            op: PRE_INC,
            exp: Box::new(e),
        }
    }

    pub fn pre_dec(e: Self) -> Self {
        Self::IncDec {
            op: PRE_DEC,
            exp: Box::new(e),
        }
    }

    pub fn post_inc(e: Self) -> Self {
        Self::IncDec {
            op: POST_INC,
            exp: Box::new(e),
        }
    }

    pub fn post_dec(e: Self) -> Self {
        Self::IncDec {
            op: POST_DEC,
            exp: Box::new(e),
        }
    }

    pub const fn lvalue(&self) -> bool {
        match self {
            Self::Var(_) => true,
            Self::Nested(e) => e.lvalue(),
            _ => false,
        }
    }

    pub const fn number(&self) -> Option<Constant> {
        match self {
            Self::Nested(e) => e.number(),
            Self::Const(c) => Some(*c),
            _ => None,
        }
    }

    pub const fn static_init(&self) -> Option<StaticInit> {
        match self {
            Self::Nested(e) => e.static_init(),
            Self::Const(Constant::Long(c)) => Some(StaticInit::Long(*c)),
            Self::Const(Constant::Int(c)) => Some(StaticInit::Int(*c)),
            _ => None,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum UnOp {
    Complement,
    Negate,
    Not,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Bop {
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
    LogAnd,
    LogOr,
    EqualTo,
    NotEqual,
    LessThan,
    GreaterThan,
    Leq,
    Geq,

    Equals,

    PlusEquals,
    MinusEquals,
    TimesEqual,
    DivEqual,
    RemEqual,
    BitAndEqual,
    BitOrEqual,
    BitXorEqual,
    LeftShiftEqual,
    RightShiftEqual,

    Ternary,
}

impl Bop {
    pub const fn precedence(&self) -> u8 {
        match self {
            Self::Equals
            | Self::PlusEquals
            | Self::MinusEquals
            | Self::TimesEqual
            | Self::DivEqual
            | Self::RemEqual
            | Self::BitAndEqual
            | Self::BitOrEqual
            | Self::BitXorEqual
            | Self::LeftShiftEqual
            | Self::RightShiftEqual => 1,
            Self::Ternary => 3,
            Self::LogOr => 5,
            Self::LogAnd => 10,
            Self::BitOr => 15,
            Self::Xor => 20,
            Self::BitAnd => 25,
            Self::EqualTo | Self::NotEqual => 30,
            Self::LessThan | Self::GreaterThan | Self::Leq | Self::Geq => 35,
            Self::LeftShift | Self::RightShift => 40,
            Self::Add | Self::Subtract => 45,
            Self::Multiply | Self::Divide | Self::Remainder => 50,
        }
    }

    pub const fn assignment_operator(&self) -> bool {
        matches!(self, Self::Equals) || self.compound()
    }

    pub const fn compound(&self) -> bool {
        matches!(
            self,
            Self::PlusEquals
                | Self::MinusEquals
                | Self::TimesEqual
                | Self::DivEqual
                | Self::RemEqual
                | Self::BitAndEqual
                | Self::BitOrEqual
                | Self::BitXorEqual
                | Self::LeftShiftEqual
                | Self::RightShiftEqual
        )
    }

    pub const fn relational(&self) -> bool {
        matches!(
            self,
            Self::EqualTo
                | Self::NotEqual
                | Self::LessThan
                | Self::GreaterThan
                | Self::Leq
                | Self::Geq
        )
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct IncDec {
    pub inc: IncOp,
    pub fix: Fix,
}

impl std::fmt::Display for IncDec {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str(match *self {
            PRE_INC => "PrefixInc",
            PRE_DEC => "PrefixDec",
            POST_INC => "PostfixInc",
            POST_DEC => "PostfixDec",
        })
    }
}

impl std::fmt::Debug for IncDec {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str(match *self {
            PRE_INC => "PrefixInc",
            PRE_DEC => "PrefixDec",
            POST_INC => "PostfixInc",
            POST_DEC => "PostfixDec",
        })
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum IncOp {
    Inc,
    Dec,
}
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Fix {
    Pre,
    Post,
}
pub const PRE_INC: IncDec = IncDec {
    inc: IncOp::Inc,
    fix: Fix::Pre,
};

pub const PRE_DEC: IncDec = IncDec {
    inc: IncOp::Dec,
    fix: Fix::Pre,
};

pub const POST_INC: IncDec = IncDec {
    inc: IncOp::Inc,
    fix: Fix::Post,
};

pub const POST_DEC: IncDec = IncDec {
    inc: IncOp::Dec,
    fix: Fix::Post,
};
