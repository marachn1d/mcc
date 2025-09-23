use crate::stmnt_path::LookupError;
use crate::Arr;
use crate::StatementPath;
use crate::{Constant, Ident, VarType};
pub use inc_dec::*;
use std::fmt::{self, Debug, Display, Formatter};

#[derive(Debug)]
pub struct Program(pub Box<[Dec]>);

#[derive(Debug, Clone)]
pub enum Dec {
    Fn(FnDec),
    Var(VarDec),
}

#[derive(Debug, Clone)]
pub struct FnDec {
    pub name: Ident,
    pub params: ParamList,
    pub body: Option<Block>,
    pub sc: Option<StorageClass>,
    pub typ: FnType,
}

#[derive(Debug, Clone)]
pub struct VarDec {
    pub name: Ident,
    pub init: Option<Expr>,
    pub sc: Option<StorageClass>,
    pub typ: VarType,
}

pub type Block = Arr<BlockItem>;
pub type ParamList = Arr<Param>;

#[derive(Debug, Copy, Clone, Hash)]
pub enum StorageClass {
    Static,
    Extern,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnType {
    pub ret: Option<VarType>,
    pub params: Box<[VarType]>,
}

#[derive(Debug, Clone)]
pub enum BlockItem {
    S(Stmnt),
    D(Dec),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Param {
    pub typ: VarType,
    pub name: Ident,
}

#[derive(Debug, Clone)]
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
    Goto(Ident),
    Null,
    Switch {
        val: Expr,
        body: Box<Self>,
    },
}

impl std::ops::Index<&StatementPath> for Stmnt {
    type Output = Self;
    fn index(&self, index: &StatementPath) -> &Self::Output {
        self.lookup(index).expect("invalid index")
    }
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
    Long(i64),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

    Var(Ident),
    Const(Constant),
    Unary(Unary),
    Nested(Box<Self>),

    Conditional {
        condition: Box<Self>,
        r#true: Box<Self>,
        r#false: Box<Self>,
    },
    FunctionCall {
        name: Ident,
        args: Box<[Self]>,
    },
}

impl std::fmt::Display for StaticInit {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Int(0) => f.write_str(".zero 4"),
            Self::Long(0) => f.write_str(".zero 8"),
            Self::Int(i) => write!(f, ".long {i}"),
            Self::Long(i) => write!(f, ".quad {i}"),
        }
    }
}

impl From<Constant> for StaticInit {
    fn from(c: Constant) -> Self {
        match c {
            Constant::Int(i) => Self::Int(i),

            Constant::Long(i) => Self::Long(i),
        }
    }
}

impl BlockItem {
    pub const fn as_inner(&self) -> (Option<&Stmnt>, Option<&Dec>) {
        match self {
            Self::S(s) => (Some(s), None),
            Self::D(d) => (None, Some(d)),
        }
    }

    pub const fn as_inner_mut(&mut self) -> (Option<&mut Stmnt>, Option<&mut Dec>) {
        match self {
            Self::S(s) => (Some(s), None),
            Self::D(d) => (None, Some(d)),
        }
    }

    pub fn into_inner(self) -> (Option<Stmnt>, Option<Dec>) {
        match self {
            Self::S(s) => (Some(s), None),
            Self::D(d) => (None, Some(d)),
        }
    }

    pub const fn as_stmnt(&self) -> Option<&Stmnt> {
        self.as_inner().0
    }

    pub const fn as_stmnt_mut(&mut self) -> Option<&mut Stmnt> {
        self.as_inner_mut().0
    }

    pub fn into_stmnt(self) -> Option<Stmnt> {
        self.into_inner().0
    }

    pub fn into_dec(self) -> Option<Dec> {
        self.into_inner().1
    }

    pub const fn as_dec(&self) -> Option<&Dec> {
        self.as_inner().1
    }
}

#[derive(Debug)]
pub struct InvalidLVal(Expr);

impl Expr {
    pub fn const_eval(&self) -> Option<Constant> {
        match self {
            Expr::Const(c) => Some(*c),
            Expr::IncDec { op, exp } => {
                let c = exp.const_eval()?;
                Some(c.with_incdec(*op))
            }
            Expr::Unary(Unary { exp, op }) => {
                let c = exp.const_eval()?;
                Some(c.with_unop(*op))
            }
            /*
            Expr::Bin(Binary {
                operator,
                left,
                right,
            }) => todo!(),
            */
            _ => None,
        }
    }

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

    pub fn check_lvalue(&self) -> Result<(), InvalidLVal> {
        match self {
            Self::Var(_) => Ok(()),
            Self::Nested(inner) => inner.check_lvalue(),
            _ => Err(InvalidLVal(self.clone())),
        }
    }

    pub const fn as_lvalue(&self) -> Option<&Ident> {
        match self {
            Self::Var(l) => Some(l),
            Self::Nested(inner) => inner.as_lvalue(),
            _ => None,
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

// use later
fn _debug_depth(f: &mut Formatter, depth: Option<usize>, t: impl Debug) -> fmt::Result {
    if let Some(depth) = depth {
        write!(f, "|")?;
        for _ in 0..depth {
            write!(f, "-")?;
        }
    }
    write!(f, "{:?}", t)
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Unary {
    pub exp: Box<Expr>,
    pub op: UnOp,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum UnOp {
    Complement,
    Negate,
    Not,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Binary {
    pub operator: Bop,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
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

    Ternary,
}

impl Bop {
    pub const fn precedence(&self) -> u8 {
        match self {
            Self::Equals => 1,
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

    pub const fn bitwise(&self) -> bool {
        matches!(self, Self::BitAnd | Self::BitOr) || self.bitshift()
    }

    pub const fn bitshift(&self) -> bool {
        matches!(self, Self::LeftShift | Self::RightShift)
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

use std::collections::VecDeque;

struct Cases<'a>(VecDeque<&'a Stmnt>);

pub enum CaseRef<'a> {
    Case(&'a Expr),
    Default,
}

pub enum CaseRefMut<'a> {
    Case(&'a Expr),
    Default,
}

impl<'a> Iterator for Cases<'a> {
    type Item = (CaseRef<'a>, &'a Stmnt);
    fn next(&mut self) -> Option<Self::Item> {
        fn extract_case<'a>(s: &'a Stmnt) -> Option<(CaseRef<'a>, &'a Stmnt)> {
            match s {
                Stmnt::Label {
                    label: Label::Case(c),
                    body,
                } => Some((CaseRef::Case(c), body)),
                Stmnt::Label {
                    label: Label::Default,
                    body,
                } => Some((CaseRef::Default, body)),
                _ => None,
            }
        }

        let next = self.0.pop_front()?;
        if let Some(case) = extract_case(next) {
            self.0.push_back(case.1);
            Some(case)
        } else {
            match next {
                Stmnt::Label { body, .. }
                | Stmnt::While { body, .. }
                | Stmnt::DoWhile { body, .. }
                | Stmnt::For { body, .. } => {
                    self.0.push_back(body);
                }
                Stmnt::If { then, r#else, .. } => {
                    self.0.push_back(then);
                    if let Some(e) = r#else {
                        self.0.push_back(e);
                    }
                }

                Stmnt::Compound(block_items) => {
                    self.0
                        .extend(block_items.iter().filter_map(BlockItem::as_stmnt));
                }

                _ => (),
            };
            self.next()
        }
    }
}

impl<'a> CasesMut<'a> {
    fn next(&mut self) -> Option<(CaseRefMut<'a>, NonNull<Stmnt>)> {
        let mut next = self.stack.pop_front()?;
        let next = unsafe { next.as_mut() };
        match next {
            Stmnt::Label {
                label: Label::Case(c),
                body,
            } => Some((CaseRefMut::Case(c), NonNull::from_mut(body))),
            Stmnt::Label {
                label: Label::Default,
                body,
            } => Some((CaseRefMut::Default, NonNull::from_mut(body))),
            Stmnt::Label { body, .. }
            | Stmnt::While { body, .. }
            | Stmnt::DoWhile { body, .. }
            | Stmnt::For { body, .. } => {
                self.stack.push_back(NonNull::from_mut(body));
                self.next()
            }
            Stmnt::If { then, r#else, .. } => {
                self.stack.push_back(NonNull::from_mut(then));
                if let Some(e) = r#else {
                    self.stack.push_back(NonNull::from_mut(e));
                }
                self.next()
            }

            Stmnt::Compound(block_items) => {
                self.stack.extend(
                    block_items
                        .iter_mut()
                        .filter_map(BlockItem::as_stmnt_mut)
                        .map(NonNull::from_mut),
                );
                self.next()
            }

            _ => self.next(),
        }
    }
}

use std::marker::PhantomData;
use std::ptr::NonNull;
struct CasesMut<'a> {
    stack: VecDeque<NonNull<Stmnt>>,
    _marker: PhantomData<&'a Stmnt>,
}

impl Stmnt {
    pub fn lookup(&self, path: &StatementPath) -> Result<&Self, LookupError<Self>> {
        let mut cur = self;
        //let mut history = vec![];
        //let mut final_idx = 0;
        for idx in path.into_iter().copied() {
            let err = || LookupError {
                last: cur.clone(),
                invalid_idx: idx,
                path: path.clone(),
                top_level: self.clone(),
            };

            match self {
                Stmnt::If {
                    then: zero,
                    r#else: Some(one),
                    ..
                } => match idx {
                    0 => cur = zero,
                    1 => cur = one,
                    _ => return Err(err()),
                },

                Stmnt::While { body, .. }
                | Stmnt::For { body, .. }
                | Stmnt::If {
                    then: body,
                    r#else: None,
                    ..
                }
                | Stmnt::DoWhile { body, .. }
                | Stmnt::Label { body, .. }
                | Stmnt::Switch { body, .. } => {
                    if idx == 0 {
                        cur = body;
                    } else {
                        return Err(err());
                    }
                }

                Stmnt::Compound(block_items) => match block_items.get(idx as usize) {
                    Some(BlockItem::S(s)) => cur = s,
                    None | Some(BlockItem::D(_)) => return Err(err()),
                },
                Stmnt::Ret(_)
                | Stmnt::Exp(_)
                | Stmnt::Break
                | Stmnt::Continue
                | Stmnt::Goto(_)
                | Stmnt::Null => return Err(err()),
            }
        }
        Ok(cur)
        /*
        let last: Self = *cur.clone();
        let top_level: Self = self.clone();
        Err(LookupError {
            last,
            invalid_idx: final_idx,
            path: path.clone(),
            top_level,
        })
            */
    }

    pub const fn secondary(&self) -> bool {
        !self.primary()
    }

    pub const fn primary(&self) -> bool {
        matches!(
            self,
            Self::Compound(_)
                | Self::If { .. }
                | Self::Switch { .. }
                | Self::While { .. }
                | Self::DoWhile { .. }
                | Self::For { .. }
        )
    }

    fn cases<'a>(&'a self) -> Cases<'a> {
        Cases(VecDeque::from_iter([self]))
    }

    fn cases_mut<'a>(&'a mut self) -> CasesMut<'a> {
        CasesMut {
            stack: VecDeque::from_iter([NonNull::from_mut(self)]),
            _marker: PhantomData,
        }
    }
}

#[derive(Debug)]
pub struct SwitchCase {
    pub case: Option<Case>,
    pub body: Stmnt,
}

impl SwitchCase {
    pub const fn new(case: Expr, body: Stmnt) -> Self {
        Self {
            case: Some(Case::Case(case)),
            body,
        }
    }

    pub const fn default(body: Stmnt) -> Self {
        Self {
            case: Some(Case::Default),
            body,
        }
    }

    pub const fn case(e: Expr, body: Stmnt) -> Self {
        Self {
            case: Some(Case::Case(e)),
            body,
        }
    }

    pub const fn constant(c: &Constant, body: Stmnt) -> Self {
        Self {
            case: Some(Case::Case(Expr::Const(*c))),
            body,
        }
    }

    pub const fn block(body: Stmnt) -> Self {
        Self { case: None, body }
    }

    pub const fn full_block(block: Block) -> Self {
        Self {
            case: None,
            body: Stmnt::Compound(block),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Case {
    Default,
    Case(Expr),
}

impl From<Constant> for Case {
    fn from(value: Constant) -> Self {
        Self::Case(Expr::Const(value))
    }
}

#[derive(Debug, Clone)]
pub enum ForInit {
    D(VarDec),
    E(Expr),
}

#[derive(Debug, Clone)]
pub enum Label {
    Named(Ident),
    Case(Expr),
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

    #[derive(Copy, Clone, Eq, PartialEq, Hash)]
    pub struct IncDec {
        pub inc: IncOp,
        pub fix: Fix,
    }

    impl IncDec {
        pub const fn pre(&self) -> bool {
            matches!(self.fix, Fix::Pre)
        }

        pub const fn post(&self) -> bool {
            !self.pre()
        }

        pub const fn inc(&self) -> bool {
            matches!(self.inc, IncOp::Inc)
        }

        pub const fn dec(&self) -> bool {
            !self.inc()
        }
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

    #[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
    pub enum IncOp {
        Inc,
        Dec,
    }
    #[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
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

impl PartialEq for StorageClass {
    fn eq(&self, other: &Self) -> bool {
        matches!(
            (self, other),
            (Self::Static, Self::Static) | (Self::Extern, Self::Extern)
        )
    }
}

impl Eq for StorageClass {}

#[derive(Debug)]
pub struct Function {
    pub name: Ident,
    pub body: Block,
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Function(\nname={},\nbody={:?}\n)", self.name, self.body)
    }
}

fn fmt_depth(f: &mut Formatter, depth: usize) -> fmt::Result {
    write!(f, "|-")?;
    for _ in 0..depth {
        write!(f, "-")?;
    }

    Ok(())
}

impl Unary {
    fn debug(&self, f: &mut Formatter, depth: usize) -> fmt::Result {
        fmt_depth(f, depth)?;
        write!(f, " {:?}{:?}", self.op, self.exp)
    }
}
