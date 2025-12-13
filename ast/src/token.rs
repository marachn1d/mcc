use crate::VarType;
use crate::parse::StaticInit;
use crate::var_type::Sign;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Token<Ident> {
    Int,
    Void,
    Return,
    If,
    Else,
    Goto,
    Do,
    While,
    For,
    Break,
    Continue,
    Switch,
    Default,
    Case,
    Static,
    Extern,
    Long,
    Const(Constant),
    Ident(Ident),
    OpenParen,
    CloseParen,
    OpenBrace,
    Semicolon,
    CloseBrace,
    Tilde,
    Decrement,
    Minus,
    Plus,

    PlusEqual,
    MinusEqual,
    TimesEqual,
    DivEqual,
    PercentEqual,
    BitAndEqual,
    BitOrEqual,
    BitXorEqual,

    Asterisk,
    Slash,
    Percent,
    Ampersand,
    Bar,
    Caret,
    Increment,
    LeftShift,
    LeftShiftEqual,
    RightShift,
    RightShiftEqual,
    Not,
    LogicalAnd,
    LogicalOr,
    EqualTo,
    NotEqual,
    LessThan,
    GreaterThan,
    Leq,
    Geq,
    Equals,
    Comma,

    QuestionMark,
    Colon,

    Unsigned,
    Signed,
}

impl<Ident> Token<Ident> {
    pub const fn type_specifier(&self) -> bool {
        use Token::{Int, Long, Signed, Unsigned};
        matches!(self, Int | Long | Signed | Unsigned)
    }
    pub const fn specifier(&self) -> bool {
        use Token::{Extern, Static};
        self.type_specifier() || matches!(self, Extern | Static)
    }

    pub fn constant(&self) -> bool {
        matches!(self, Self::Const(_))
    }

    pub fn identifier(&self) -> bool {
        matches!(self, Self::Ident(_))
    }
}

use std::fmt::{self, Display, Formatter};

impl<T: fmt::Debug + Display> Display for Token<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Self::Ident(s) = self {
            <T as Display>::fmt(s, f)
        } else {
            <Self as fmt::Debug>::fmt(self, f)
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Ord, PartialOrd, Hash)]
pub enum Constant {
    Int(Int),
    Long(Long),
}

impl std::fmt::Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.is_negative() {
            f.write_str("n")?
        }

        match self.abs() {
            Self::Int(i) => i.fmt(f),
            Self::Long(l) => l.fmt(f),
        }
    }
}

#[allow(dead_code)]
fn word_character<T: AsRef<char>>(byte: &T) -> bool {
    // assuming that performance "loss" is worth dedup
    word_start(byte) || byte.as_ref().is_ascii_digit()
}

fn word_start<T: AsRef<char>>(byte: &T) -> bool {
    byte.as_ref().is_ascii_alphanumeric() || *byte.as_ref() == '_'
}

pub struct DebugToken<Ident> {
    pub token: Token<Ident>,
    pub line: usize,
    pub char: usize,
}

impl<T> From<Constant> for Token<T> {
    fn from(c: Constant) -> Self {
        Self::Const(c)
    }
}

impl From<i32> for Constant {
    fn from(c: i32) -> Self {
        Self::new_int(c)
    }
}

impl From<i64> for Constant {
    fn from(c: i64) -> Self {
        Self::new_long(c)
    }
}

impl From<StaticInit> for Constant {
    fn from(si: StaticInit) -> Self {
        si.map(&mut Self::Int, &mut Self::Long)
    }
}

impl Constant {
    pub const fn new_int(int: i32) -> Self {
        Self::Int(Int::I(int))
    }

    pub const fn new_long(long: i64) -> Self {
        Self::Long(Long::I(long))
    }

    pub const fn new_uint(int: u32) -> Self {
        Self::Int(Int::U(int))
    }

    pub const fn new_ulong(long: u64) -> Self {
        Self::Long(Long::U(long))
    }

    pub fn map<T, F: Fn(Int) -> T, G: Fn(Long) -> T>(self, f: F, g: G) -> T {
        match self {
            Constant::Int(i) => f(i),
            Constant::Long(l) => g(l),
        }
    }

    pub fn convert_to(&self, ty: &VarType) -> Self {
        self.static_init().convert_to(ty).into()
    }

    pub fn map_deep<T, F: Fn(i32) -> T, G: Fn(i64) -> T, H: Fn(u32) -> T, J: Fn(u64) -> T>(
        self,
        int: F,
        long: G,
        uint: H,
        ulong: J,
    ) -> T {
        self.map(|i| i.map(&int, &uint), |l| l.map(&long, &ulong))
    }

    pub fn map_int<T, F: Fn(i32) -> T, G: Fn(i64) -> T>(self, f: F, g: G) -> T {
        match self {
            Constant::Int(i) => f(i.signed()),
            Constant::Long(l) => g(l.signed()),
        }
    }

    pub fn map_uint<T, F: Fn(u32) -> T, G: Fn(u64) -> T>(self, f: F, g: G) -> T {
        self.map(|i| f(i.unsigned()), |l| g(l.unsigned()))
    }

    pub fn edit<F: Fn(Int) -> Int, G: Fn(Long) -> Long>(self, f: F, g: G) -> Self {
        match self {
            Constant::Int(i) => Self::Int(f(i)),
            Constant::Long(l) => Self::Long(g(l)),
        }
    }

    const fn copied(&self) -> Self {
        *self
    }

    pub fn int(&self) -> Int {
        self.copied().map(|i| i, |l| l.int())
    }

    pub fn long(&self) -> Long {
        self.copied().map(|i| i.long(), |l| l)
    }

    pub fn ty(&self) -> crate::VarType {
        self.map(|i| i.ty(), |l| l.ty())
    }

    pub fn abs(&self) -> Self {
        self.copied().edit(|i| i.abs(), |l| l.abs())
    }

    pub fn is_positive(&self) -> bool {
        self.map(|i| i.is_positive(), |l| l.is_positive())
    }

    pub fn is_negative(&self) -> bool {
        !self.is_positive()
    }

    pub fn with_unop(&self, op: crate::parse::UnOp) -> Self {
        use crate::parse::UnOp;
        match op {
            UnOp::Negate => self.edit(|i| -i, |l| -l),
            UnOp::Not => self.edit(|i| !i, |l| !l),
            UnOp::Complement => self.edit(|i| i.complement(), |l| l.complement()),
        }
    }

    pub fn with_incdec(&self, op: crate::parse::IncDec) -> Self {
        use crate::parse::inc_dec::{POST_DEC, POST_INC, PRE_DEC, PRE_INC};
        let c = self.copied();
        match op {
            POST_INC | POST_DEC => c,
            PRE_INC => c.edit(|i| i + 1, |l| l - 1i64),
            PRE_DEC => c.edit(|i| i - 1, |l| l - 1i64),
        }
    }

    /*
    pub fn static_init(&self) -> StaticInit {
        self.copied().map(StaticInit::Int, StaticInit::Long)
    }
    */

    pub fn eq(&self, int: i32, long: i64) -> bool {
        self.copied()
            .map(|i| i.eq_signed(int), |l| l.eq_signed(long))
    }

    pub fn is_zero(&self) -> bool {
        self.eq(0, 0)
    }

    pub fn static_init(&self) -> StaticInit {
        self.map(StaticInit::Int, StaticInit::Long)
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Ord, PartialOrd, Hash)]
pub enum Int {
    U(u32),
    I(i32),
}

impl Int {
    pub const fn ty(&self) -> VarType {
        match self {
            Self::U(_) => VarType::uint(),

            Self::I(_) => VarType::int(),
        }
    }

    pub fn map<T, F: Fn(i32) -> T, G: Fn(u32) -> T>(self, f: F, g: G) -> T {
        match self {
            Self::I(i) => f(i),
            Self::U(u) => g(u),
        }
    }

    pub fn edit<F: Fn(i32) -> i32, G: Fn(u32) -> u32>(self, f: F, g: G) -> Self {
        self.map(|i| Self::I(f(i)), |u| Self::U(g(u)))
    }

    const fn copied(&self) -> Self {
        *self
    }

    pub fn signed(&self) -> i32 {
        self.copied().map(|i| i, |u| u.cast_signed())
    }

    pub fn unsigned(&self) -> u32 {
        self.copied().map(|i| i.cast_unsigned(), |u| u)
    }

    pub fn long(&self) -> Long {
        self.map(|i| Long::I(i as i64), |u| Long::U(u as u64))
    }

    pub fn abs(&self) -> Self {
        self.edit(|i| i.abs(), |u| u)
    }

    pub fn is_positive(&self) -> bool {
        self.copied().map(|i| i.is_positive(), |_| true)
    }

    pub fn combine<F: Fn(i32, i32) -> i32, G: Fn(u32, u32) -> u32>(
        self,
        other: Self,
        f: F,
        g: G,
    ) -> Self {
        match (self, other) {
            (Self::I(l), Self::I(r)) => Self::I(f(l, r)),
            (l, r) => Self::U(g(l.unsigned(), r.unsigned())),
        }
    }

    pub fn to_signed(&self) -> Self {
        Self::I(self.signed())
    }

    pub fn to_unsigned(&self) -> Self {
        Self::U(self.unsigned())
    }

    pub const fn is_signed(&self) -> bool {
        self.ty().signed()
    }

    pub fn eq_signed(&self, other: i32) -> bool {
        self.signed() == other
    }

    pub fn eq_unsigned(&self, other: u32) -> bool {
        self.unsigned() == other
    }

    pub fn eq_signed_strict(&self, other: i32) -> bool {
        self.map(|i| i == other, |_| false)
    }

    pub fn eq_unsigned_strict(&self, other: u32) -> bool {
        self.map(|_| false, |u| u == other)
    }

    pub fn complement(&self) -> Self {
        self.edit(|i| !i, |u| !u)
    }

    pub const fn zero(s: Sign) -> Self {
        match s {
            Sign::Unsigned => Self::U(0),
            Sign::Signed => Self::I(0),
        }
    }

    pub fn is_zero(&self) -> bool {
        self.map(|i| i == 0, |u| u == 0)
    }

    pub fn fmt_label(&self) -> String {
        match self {
            Int::U(u) => format!("{u}"),
            Int::I(i) if *i < 0 => format!("n{}", i.abs()),
            Int::I(i) => format!("{i}"),
        }
    }

    pub fn fmt_num(&self) -> String {
        match self {
            Int::U(u) => format!("{u}"),
            Int::I(i) => format!("{i}"),
        }
    }
}

use std::ops::{Add, Neg, Not, Sub};

#[derive(Debug, PartialEq, Eq, Copy, Clone, Ord, PartialOrd, Hash)]
pub enum Long {
    I(i64),
    U(u64),
}

impl Long {
    pub fn map<T, F: Fn(i64) -> T, G: Fn(u64) -> T>(self, f: F, g: G) -> T {
        match self {
            Self::I(i) => f(i),
            Self::U(u) => g(u),
        }
    }

    pub fn combine<F: Fn(i64, i64) -> i64, G: Fn(u64, u64) -> u64>(
        self,
        other: Self,
        f: F,
        g: G,
    ) -> Self {
        match (self, other) {
            (Self::I(l), Self::I(r)) => Self::I(f(l, r)),
            (l, r) => Self::U(g(l.unsigned(), r.unsigned())),
        }
    }

    pub fn edit<F: Fn(i64) -> i64, G: Fn(u64) -> u64>(self, f: F, g: G) -> Self {
        self.map(|i| Self::I(f(i)), |u| Self::U(g(u)))
    }

    const fn copied(&self) -> Self {
        *self
    }

    pub fn signed(&self) -> i64 {
        self.copied().map(|i| i, |u| u.cast_signed())
    }

    pub fn unsigned(&self) -> u64 {
        self.copied().map(|i| i.cast_unsigned(), |u| u)
    }

    pub fn int(&self) -> Int {
        self.map(|i| Int::I(i as i32), |u| Int::U(u as u32))
    }

    pub const fn is_signed(&self) -> bool {
        self.ty().signed()
    }

    pub fn to_signed(&self) -> Self {
        Self::I(self.signed())
    }

    pub fn to_unsigned(&self) -> Self {
        Self::U(self.unsigned())
    }

    pub fn abs(&self) -> Self {
        self.edit(|i| i.abs(), |u| u)
    }

    pub fn complement(&self) -> Self {
        self.edit(|i| !i, |u| !u)
    }

    pub fn is_positive(&self) -> bool {
        self.copied().map(|i| i.is_positive(), |_| true)
    }

    pub fn eq_signed(&self, other: i64) -> bool {
        self.signed() == other
    }

    pub fn eq_unsigned(&self, other: u64) -> bool {
        self.unsigned() == other
    }

    pub fn eq_signed_strict(&self, other: i64) -> bool {
        self.map(|i| i == other, |_| false)
    }

    pub fn eq_unsigned_strict(&self, other: u64) -> bool {
        self.map(|_| false, |l| l == other)
    }

    pub const fn zero(s: Sign) -> Self {
        match s {
            Sign::Unsigned => Self::U(0),
            Sign::Signed => Self::I(0),
        }
    }

    pub fn is_zero(&self) -> bool {
        self.map(|i| i == 0, |u| u == 0)
    }

    pub fn fmt_label(&self) -> String {
        match self {
            Self::U(u) => format!("{u}"),
            Self::I(i) if *i < 0 => format!("n{}", i.abs()),
            Self::I(i) => format!("{i}"),
        }
    }

    pub fn fmt_num(&self) -> String {
        match self {
            Self::U(u) => format!("{u}"),
            Self::I(i) => format!("{i}"),
        }
    }

    pub const fn ty(&self) -> VarType {
        match self {
            Self::U(_) => VarType::ulong(),
            Self::I(_) => VarType::long(),
        }
    }
}

/*
impl From<StaticInit> for Constant {
    fn from(value: StaticInit) -> Self {
        match value {
            StaticInit::Int(i) => Self::Int(i),
            StaticInit::Long(l) => Self::Long(l),
        }
    }
}
*/

impl<Ident> DebugToken<Ident> {
    pub fn into_inner(self) -> (Token<Ident>, usize) {
        (self.token, self.line)
    }

    pub const fn line(&self) -> usize {
        self.line
    }
}

impl<Ident> std::ops::Deref for DebugToken<Ident> {
    type Target = Token<Ident>;

    fn deref(&self) -> &Self::Target {
        &self.token
    }
}
use std::ops::Deref;

use std::ops::DerefMut;
impl<Ident> AsRef<Token<Ident>> for DebugToken<Ident> {
    fn as_ref(&self) -> &Token<Ident> {
        self.deref()
    }
}

impl<I> AsMut<Token<I>> for DebugToken<I> {
    fn as_mut(&mut self) -> &mut Token<I> {
        self.deref_mut()
    }
}

impl<I> std::ops::DerefMut for DebugToken<I> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.token
    }
}

impl<I> From<DebugToken<I>> for Token<I> {
    fn from(debug: DebugToken<I>) -> Token<I> {
        debug.token
    }
}

impl From<crate::Ident> for Token<crate::Ident> {
    fn from(i: crate::Ident) -> Self {
        Self::Ident(i)
    }
}

impl Add for Long {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        self.combine(rhs, i64::wrapping_add, u64::wrapping_add)
    }
}

impl Sub for Long {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        self.combine(rhs, i64::wrapping_sub, u64::wrapping_sub)
    }
}

impl Neg for Long {
    type Output = Self;

    fn neg(self) -> Self::Output {
        self.edit(i64::wrapping_neg, u64::wrapping_neg)
    }
}

impl Not for Long {
    type Output = Self;

    fn not(self) -> Self::Output {
        self.edit(
            |i| if i == 0 { 1 } else { 0 },
            |l| if l == 0 { 1 } else { 0 },
        )
    }
}

impl Add<i64> for Long {
    type Output = Self;

    fn add(self, rhs: i64) -> Self::Output {
        self + Self::I(rhs)
    }
}

impl Sub<i64> for Long {
    type Output = Self;

    fn sub(self, rhs: i64) -> Self::Output {
        self - Self::I(rhs)
    }
}

impl Add<u64> for Long {
    type Output = Self;

    fn add(self, rhs: u64) -> Self::Output {
        self + Self::U(rhs)
    }
}

impl Sub<u64> for Long {
    type Output = Self;

    fn sub(self, rhs: u64) -> Self::Output {
        self - Self::U(rhs)
    }
}

impl Add for Int {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        self.combine(rhs, i32::wrapping_add, u32::wrapping_add)
    }
}

impl Sub for Int {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        self.combine(rhs, i32::wrapping_sub, u32::wrapping_sub)
    }
}

impl Neg for Int {
    type Output = Self;

    fn neg(self) -> Self::Output {
        self.edit(i32::wrapping_neg, u32::wrapping_neg)
    }
}

impl Not for Int {
    type Output = Self;

    fn not(self) -> Self::Output {
        self.edit(
            |i| if i == 0 { 1 } else { 0 },
            |l| if l == 0 { 1 } else { 0 },
        )
    }
}

impl Add<i32> for Int {
    type Output = Self;

    fn add(self, rhs: i32) -> Self::Output {
        self + Self::I(rhs)
    }
}

impl Sub<i32> for Int {
    type Output = Self;

    fn sub(self, rhs: i32) -> Self::Output {
        self - Self::I(rhs)
    }
}

impl Add<u32> for Int {
    type Output = Self;

    fn add(self, rhs: u32) -> Self::Output {
        self + Self::U(rhs)
    }
}

impl Sub<u32> for Int {
    type Output = Self;

    fn sub(self, rhs: u32) -> Self::Output {
        self - Self::U(rhs)
    }
}

impl std::fmt::Display for Int {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Int::U(u) => u.fmt(f),
            Int::I(i) if *i < 0 => write!(f, "n{}", i.abs()),
            Int::I(i) => i.fmt(f),
        }
    }
}

impl std::fmt::Display for Long {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Long::U(u) => u.fmt(f),
            Long::I(i) if *i < 0 => write!(f, "n{}", i.abs()),
            Long::I(i) => i.fmt(f),
        }
    }
}
