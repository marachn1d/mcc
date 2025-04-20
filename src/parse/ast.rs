use crate::lex::Identifier;
pub use inc_dec::*;
use std::fmt::{self, Display, Formatter};
pub type Arr<T> = Box<[T]>;

#[derive(Debug)]
pub struct Program(pub Box<[Dec]>);

#[derive(Debug)]
pub enum Dec {
    Fn(FnDec),
    Var(VarDec),
}

#[derive(Debug)]
pub struct FnDec {
    pub name: Identifier,
    pub params: ParamList,
    pub body: Option<Block>,
    pub sc: Option<StorageClass>,
    pub typ: FnType,
}

impl From<FnDec> for Dec {
    fn from(dec: FnDec) -> Self {
        Dec::Fn(dec)
    }
}

impl From<VarDec> for Dec {
    fn from(dec: VarDec) -> Self {
        Dec::Var(dec)
    }
}
#[derive(Debug, Copy, Clone)]
pub enum StaticInit {
    Int(i32),
    UInt(u32),
    Long(i64),
    ULong(u64),
}

impl From<Constant> for StaticInit {
    fn from(c: Constant) -> Self {
        match c {
            Constant::Int(i) => Self::Int(i),
            Constant::UInt(i) => Self::UInt(i),
            Constant::ULong(i) => Self::ULong(i),
            Constant::Long(i) => Self::Long(i),
        }
    }
}

pub type Block = Arr<BlockItem>;

pub type ParamList = Arr<Param>;

#[derive(Debug, Clone)]
pub struct Param {
    pub typ: VarType,
    pub name: Identifier,
}

#[derive(Debug)]
pub struct VarDec {
    pub name: Identifier,
    pub init: Option<Expr>,
    pub sc: Option<StorageClass>,
    pub typ: VarType,
}

#[derive(Debug, Copy, Clone)]
pub enum StorageClass {
    Static,
    Extern,
}

#[derive(Debug, Clone)]
pub struct FnType {
    pub ret: Option<VarType>,
    pub params: Box<[VarType]>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum VarType {
    Int,
    Long,
    UInt,
    ULong,
}

impl VarType {
    pub fn common_ty(&self, other: &Self) -> Option<Self> {
        Some(match self.cmp(other) {
            Ordering::Greater | Ordering::Equal => *self,
            Ordering::Less => *other,
        })
    }

    pub const fn sign(&self) -> Sign {
        match self {
            Self::Int | Self::Long => Sign::Signed,
            Self::UInt | Self::ULong => Sign::Unsigned,
        }
    }

    pub const fn size(&self) -> TySize {
        match self {
            Self::Int | Self::UInt => TySize::Int,
            Self::Long | Self::ULong => TySize::Long,
        }
    }

    pub fn signed(&self) -> bool {
        self.sign() == Sign::Signed
    }

    pub fn unsigned(&self) -> bool {
        self.sign() == Sign::Unsigned
    }
}

use std::cmp::{Eq, Ordering, PartialEq};
impl Ord for VarType {
    // C Arithmetic Conversion Rules: (Section 6.3.1.8 Paragraph 1 of Cstd)
    // 1. if both have same type no conversion is needed
    // 2. if both are signed or unsigned, compare by rank
    // 3. If Unsigned can represent signed, convert to unsigned
    // 4. If Signed can represent Unsigned, convert to signed
    // 5. otherwise, (if they're the same size) convert to unsigned greater ranked type

    // In other words, if they're the same size, then convert to unsigned, if they're
    // different, convert to the bigger one

    fn cmp(&self, other: &Self) -> Ordering {
        let size = self.size().cmp(&other.size());
        let sign = self.sign().cmp(&other.sign());
        if size == Ordering::Equal {
            sign
        } else {
            size
        }
    }
}

impl PartialOrd for VarType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum Sign {
    Unsigned = 2,
    Signed = 1,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum TySize {
    Long = 2,
    Int = 1,
}

#[derive(Debug)]
pub enum BlockItem {
    S(Stmnt),
    D(Dec),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Assignment {
        dst: Box<Self>,
        src: Box<Self>,
    },
    Bin(Binary),
    Cast {
        target: VarType,
        exp: Box<Self>,
    },

    //factors
    IncDec {
        op: inc_dec::IncDec,
        exp: Box<Self>,
    },

    Var(Identifier),
    Const(Constant),
    Unary(Unary),
    Nested(Box<Self>),

    Conditional {
        condition: Box<Self>,
        r#true: Box<Self>,
        r#false: Box<Self>,
    },
    FunctionCall {
        name: Identifier,
        args: Box<[Self]>,
    },
}

impl Expr {
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
            Self::Const(Constant::UInt(c)) => Some(StaticInit::UInt(*c)),
            Self::Const(Constant::ULong(c)) => Some(StaticInit::ULong(*c)),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub exp: Box<Expr>,
    pub op: UnOp,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum UnOp {
    Complement,
    Negate,
    Not,
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub operator: Bop,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
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

#[derive(Debug)]
pub enum Stmnt {
    Ret(Expr),
    Exp(Expr),
    If {
        condition: Expr,
        then: Box<Self>,
        r#else: Option<Box<Self>>,
    },
    Break,
    Continue,
    While {
        condition: Expr,
        body: Box<Self>,
    },
    DoWhile {
        body: Box<Self>,
        condition: Expr,
    },
    For {
        init: Option<ForInit>,
        condition: Option<Expr>,
        post: Option<Expr>,
        body: Box<Self>,
    },
    Compound(Block),
    Label {
        label: Label,
        body: Box<Self>,
    },
    Goto(Identifier),
    Null,
    Switch {
        val: Expr,
        body: Box<Self>,
    },
}

#[derive(Debug)]
pub enum ForInit {
    D(VarDec),
    E(Expr),
}

#[derive(Debug, Clone)]
pub enum Label {
    Named(Identifier),
    Case(Constant),
    Default,
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Program(\n{:?}\n)", self.0)
    }
}

impl Display for Stmnt {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Stmnt::Ret(ret) => write!(f, "Return(\n{:?}\n)", ret),
            Stmnt::Exp(e) => write!(f, "(\n{:?}\n)", e),
            Stmnt::Null => write!(f, "(\nnull\n)"),

            Stmnt::If {
                condition,
                then,
                r#else,
            } => write!(f, "if(\n{condition:?}\n){then}{else:?}"),
            Stmnt::Label { label, body } => write!(f, "LABEL\n{label}:{body}\n"),

            Stmnt::Goto(name) => write!(f, "goto\n{name}:\n"),

            Stmnt::Compound(block) => writeln!(f, "{{{block:?}}}"),
            _ => todo!(),
        }
    }
}

impl Display for Label {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Label::Default => write!(f, "default"),
            Label::Named(name) => write!(f, "{name}"),
            Label::Case(case) => write!(f, "case {case:?}"),
        }
    }
}

pub mod inc_dec {

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

    pub fn int(&self) -> i32 {
        match self {
            Self::Int(i) => *i,
            Self::Long(l) => *l as i32,
            Self::ULong(ul) => *ul as i32,
            Self::UInt(ui) => *ui as i32,
        }
    }

    pub fn long(&self) -> i64 {
        match self {
            Self::Int(i) => *i as i64,
            Self::Long(l) => *l,
            Self::ULong(ul) => *ul as i64,
            Self::UInt(ui) => *ui as i64,
        }
    }
}

impl From<i32> for Constant {
    fn from(i: i32) -> Self {
        Self::Int(i)
    }
}

impl From<i64> for Constant {
    fn from(i: i64) -> Self {
        Self::Long(i)
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Int(i) => i.fmt(f),
            Self::Long(l) => l.fmt(f),
            Self::UInt(i) => i.fmt(f),
            Self::ULong(l) => l.fmt(f),
        }
    }
}

#[cfg(test)]
mod test {
    use super::Sign::{self, Signed, Unsigned};
    use super::TySize;
    use super::VarType::{self, Int, Long, UInt, ULong};
    const SIGNS: [Sign; 2] = [Signed, Unsigned];
    const SIZES: [TySize; 2] = [TySize::Int, TySize::Long];
    const TYPES: [VarType; 4] = [Int, Long, UInt, ULong];

    #[test]
    fn type_conv() {
        for ty in TYPES {
            assert_eq!(ty, ty)
        }
        for sz in SIZES {
            assert_eq!(sz, sz)
        }
        for sn in SIGNS {
            assert_eq!(sn, sn)
        }
        assert!(Signed < Unsigned);
        assert!(Unsigned > Signed);
        assert!(TySize::Int < TySize::Long);
        assert!(TySize::Long > TySize::Int);

        assert!(UInt > Int);
        assert!(Long > Int);
        assert!(Long > UInt);
        assert!(ULong > Long);
        assert!(ULong > UInt);
        assert!(ULong > Int);

        assert!(Int < UInt);
        assert!(Int < Long);
        assert!(Int < ULong);

        assert!(UInt < Long);
        assert!(UInt < ULong);

        assert!(Long < ULong);
    }
}
