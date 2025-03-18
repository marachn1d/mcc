use super::lex::Identifier;
use super::slice_iter::TokenIter;
use lex::Constant;

use super::Token;
use std::fmt::{self, Display, Formatter};

pub fn parse(tokens: Box<[Token]>) -> Result<Program, Error> {
    let mut tokens = TokenIter::new(tokens);
    let program = program(&mut tokens)?;
    if tokens.is_empty() {
        Ok(program)
    } else {
        Err(Error::ExtraStuff)
    }
}

fn program(tokens: &mut TokenIter) -> Result<Program, Error> {
    let mut functions = Vec::new();
    while !tokens.is_empty() {
        functions.push(declaration(tokens)?);
    }
    Ok(Program(functions.into()))
}

#[derive(Debug)]
pub struct Program(pub Box<[Declaration]>);

fn declaration(tokens: &mut TokenIter) -> Result<Declaration, Error> {
    let SpecifierList {
        storage_class,
        r#type,
    } = specifiers(tokens)?;

    let name = tokens.consume_identifier()?;

    match tokens.consume_any()? {
        Token::Equals => {
            let exp = expression(tokens, None)?;
            tokens.consume(Token::Semicolon)?;
            Ok(Declaration::Var {
                name,
                init: Some(exp),
                storage_class,
                r#type,
            })
        }
        Token::OpenParen => {
            let params = param_list(tokens)?;

            let fn_type = FnType {
                ret: Some(r#type.into()),
                params: params.iter().map(|p| p.r#type).collect(),
            };

            let body = if tokens.next_if(|x| x == &Token::Semicolon).is_some() {
                None
            } else {
                Some(block(tokens)?)
            };

            Ok(Declaration::Function {
                name,
                params,
                body,
                r#type: fn_type,
                storage_class,
            })
        }
        Token::Semicolon => Ok(Declaration::Var {
            name,
            init: None,
            storage_class,
            r#type,
        }),
        _ => Err(Error::Catchall("expected initializer or semicolon")),
    }
}

// type specifier

struct SpecifierList {
    storage_class: Option<StorageClass>,
    r#type: VarType,
}

#[derive(Debug, Clone)]
pub struct SpeclistFsm {
    storage_class: Option<StorageClass>,
    int: bool,
    r#type: Option<VarType>,
}

impl SpeclistFsm {
    const fn new() -> Self {
        Self {
            storage_class: None,
            r#type: None,
            int: false,
        }
    }

    fn done(self) -> Result<SpecifierList, Error> {
        if let Some(r#type) = self.r#type {
            Ok(SpecifierList {
                storage_class: self.storage_class,
                r#type,
            })
        } else {
            Err(Error::Catchall("invalid specifier list"))
        }
    }

    fn type_specifier(self) -> Result<VarType, Error> {
        match self {
            Self {
                storage_class: Some(_),
                ..
            } => Err(Error::NoStorageClass),
            Self {
                storage_class: None,
                r#type: None,
                int: _,
            } => Err(Error::InvalidSpecifiers),
            Self {
                storage_class: None,
                r#type: Some(typ),
                int: _,
            } => Ok(typ),
        }
    }

    const fn r#extern(&mut self) -> Result<(), Error> {
        match self.storage_class {
            Some(StorageClass::Static) => Err(Error::ConflictingLinkage),
            Some(StorageClass::Extern) => Err(Error::InvalidSpecifiers),
            None => {
                self.storage_class = Some(StorageClass::Extern);
                Ok(())
            }
        }
    }

    const fn r#static(&mut self) -> Result<(), Error> {
        match self.storage_class {
            Some(StorageClass::Static) => Err(Error::InvalidSpecifiers),
            Some(StorageClass::Extern) => Err(Error::ConflictingLinkage),
            None => {
                self.storage_class = Some(StorageClass::Static);
                Ok(())
            }
        }
    }

    fn long(&mut self) -> Result<(), Error> {
        match self.r#type {
            Some(VarType::Int) => {
                self.r#type = Some(VarType::Long);
                self.int = true;
                Ok(())
            }
            Some(VarType::Long) => self.invalid_type(),
            None => {
                self.r#type = Some(VarType::Long);
                Ok(())
            }
        }
    }

    fn int(&mut self) -> Result<(), Error> {
        match self.r#type {
            None => {
                self.r#type = Some(VarType::Int);
                self.int = true;
                Ok(())
            }
            Some(VarType::Long) if !self.int => Ok(()),
            Some(VarType::Int | VarType::Long) => self.invalid_type(),
        }
    }

    fn invalid_type<T>(&self) -> Result<T, Error> {
        Err(Error::InvalidType(self.clone()))
    }
}

fn get_specifiers(tokens: &mut TokenIter) -> Result<SpeclistFsm, Error> {
    let mut builder = SpeclistFsm::new();

    tokens.take_until(|token| match token {
        Token::Int => builder.int().map(|_| true),
        Token::Long => builder.long().map(|_| true),
        Token::Static => builder.r#static().map(|_| true),
        Token::Extern => builder.r#extern().map(|_| true),
        _ => Ok(false),
    })?;
    Ok(builder)
}

fn specifiers(tokens: &mut TokenIter) -> Result<SpecifierList, Error> {
    let builder = get_specifiers(tokens)?;
    builder.done()
}

fn type_specifier(tokens: &mut TokenIter) -> Result<VarType, Error> {
    let builder = get_specifiers(tokens)?;
    builder.type_specifier()
}

fn param_list(tokens: &mut TokenIter) -> Result<ParamList, Error> {
    if tokens.consume(Token::Void).is_ok() {
        tokens.consume(Token::CloseParen)?;
        return Ok(Box::new([]));
    }

    let mut params = Vec::new();

    let mut p = param(tokens)?;

    while !p.last {
        params.push(p);
        p = param(tokens)?;
    }

    params.push(p);
    Ok(params.into())
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub r#type: VarType,
    pub name: Identifier,
    pub last: bool,
}

fn param(tokens: &mut TokenIter) -> Result<Parameter, Error> {
    if let Ok(r#type) = type_specifier(tokens) {
        let name = tokens.consume_identifier()?;
        let last = match tokens.peek_any()? {
            Token::Comma => {
                tokens.next();
                Ok(false)
            }
            Token::CloseParen => {
                tokens.next();
                Ok(true)
            }
            _ => Err(Error::Catchall("expected ',' or ')'.")),
        }?;
        Ok(Parameter { r#type, name, last })
    } else {
        Err(Error::Catchall("expected ',' or ')'."))
    }
}

#[derive(Debug)]
pub struct Block(pub Box<[BlockItem]>);

fn block(tokens: &mut TokenIter) -> Result<Block, Error> {
    tokens.consume(Token::OpenBrace)?;
    let mut body = Vec::new();
    while let Some(item) = block_item(tokens)? {
        body.push(item);
    }
    tokens.consume(Token::CloseBrace)?;
    Ok(Block(body.into()))
}

fn block_item(tokens: &mut TokenIter) -> Result<Option<BlockItem>, Error> {
    match tokens.peek_any()? {
        Token::Int | Token::Static | Token::Extern | Token::Long => {
            Ok(Some(BlockItem::D(declaration(tokens)?)))
        }
        Token::CloseBrace => Ok(None),
        _ => Ok(Some(BlockItem::S(statement(tokens)?))),
    }
}

fn var_declaration(
    tokens: &mut TokenIter,
    storage_class: Option<StorageClass>,
) -> Result<VariableDeclaration, Error> {
    let r#type = type_specifier(tokens)?;
    let name = tokens.consume_identifier()?;
    let init = match tokens.consume_any()? {
        Token::Equals => {
            let exp = expression(tokens, None)?;

            tokens.consume(Token::Semicolon)?;
            Ok(Some(exp))
        }
        Token::Semicolon => Ok(None),
        _ => Err(Error::Catchall("expected initializer or semicolon")),
    }?;
    Ok(VariableDeclaration {
        name,
        init,
        r#type,
        storage_class,
    })
}

pub type ParamList = Box<[Parameter]>;

pub enum Param {
    Void,
    Int(Identifier),
}

#[derive(Debug)]
pub struct VariableDeclaration {
    pub name: Identifier,
    pub init: Option<Expression>,
    pub storage_class: Option<StorageClass>,
    pub r#type: VarType,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum VarType {
    Int,
    Long,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Var(VarType),
    Fn(FnType),
}

impl VarType {
    pub const fn common_type(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (Self::Long, Self::Long) | (Self::Long, Self::Int) | (Self::Int, Self::Long) => {
                Some(Self::Long)
            }
            (Self::Int, Self::Int) => Some(Self::Int),
        }
    }

    pub const fn alignment(&self) -> u32 {
        match self {
            Self::Int => 4,
            Self::Long => 8,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnType {
    pub ret: Option<VarType>,
    pub params: Box<[VarType]>,
}

#[derive(Debug, Copy, Clone)]
pub enum StorageClass {
    Static,
    Extern,
}

impl PartialEq for StorageClass {
    fn eq(&self, other: &Self) -> bool {
        matches!(
            (self, other),
            (Self::Static, Self::Static) | (Self::Extern, Self::Extern)
        )
    }
}

impl From<VariableDeclaration> for Declaration {
    fn from(dec: VariableDeclaration) -> Self {
        Self::Var {
            name: dec.name,
            init: dec.init,
            r#type: dec.r#type,
            storage_class: dec.storage_class,
        }
    }
}

#[derive(Debug)]
pub struct Function {
    pub name: Identifier,
    pub body: Block,
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Function(\nname={},\nbody={:?}\n)", self.name, self.body)
    }
}

fn statement(tokens: &mut TokenIter) -> Result<Statement, Error> {
    Ok(match tokens.peek_any()? {
        Token::Return => {
            tokens.next();
            let expression = expression(tokens, None)?;
            tokens.consume(Token::Semicolon)?;
            Statement::Ret(expression)
        }
        Token::Semicolon => {
            tokens.next();
            Statement::Null
        }
        Token::If => {
            tokens.next();
            tokens.consume(Token::OpenParen)?;
            let condition = expression(tokens, None)?;
            tokens.consume(Token::CloseParen)?;
            let stmnt = statement(tokens)?;
            let r#else = if tokens.consume(Token::Else).is_ok() {
                Some(Box::new(statement(tokens)?))
            } else {
                None
            };
            Statement::If {
                condition,
                then: Box::new(stmnt),
                r#else,
            }
        }
        Token::Switch => {
            tokens.next();
            tokens.consume(Token::OpenParen)?;
            let Expression::Const(c) = expression(tokens, None)? else {
                todo!()
            };
            tokens.consume(Token::CloseParen)?;
            let body = Box::new(statement(tokens)?);
            Statement::Switch {
                val: ConstExpr::Literal(c),
                body,
            }
        }
        Token::Goto => {
            tokens.next();
            let identifier = tokens.consume_identifier()?;
            tokens.consume(Token::Semicolon)?;
            Statement::Goto(identifier)
        }
        // LABEL
        Token::Identifier(_) if tokens.peek_peek().is_some_and(|x| x == &Token::Colon) => {
            let name = tokens.consume_identifier()?;
            let label = Label::Named(name.clone());
            tokens.next();
            let body = statement(tokens)?.into();

            Statement::Label { label, body }
        }

        Token::Default => {
            tokens.next();
            tokens.consume(Token::Colon)?;
            let body = statement(tokens)?.into();
            Statement::Label {
                label: Label::Default,
                body,
            }
        }
        Token::OpenBrace => {
            let block = block(tokens)?;
            Statement::Compound(block)
        }
        Token::Break => {
            tokens.next();
            tokens.consume(Token::Semicolon)?;

            Statement::Break
        }
        Token::Case => {
            tokens.next();
            let Token::Constant(con) = tokens.consume_any()? else {
                return Err(Error::Catchall("expected constant"));
            };

            tokens.consume(Token::Colon)?;
            let label = Label::Case(ConstExpr::Literal(con));
            let body = statement(tokens)?.into();
            Ok(Statement::Label { label, body })?
        }
        Token::Continue => {
            tokens.next();
            tokens.consume(Token::Semicolon)?;
            Statement::Continue
        }
        Token::While => {
            tokens.next();
            tokens.consume(Token::OpenParen)?;
            let condition = expression(tokens, None)?;

            tokens.consume(Token::CloseParen)?;
            Statement::While {
                condition,
                body: Box::new(statement(tokens)?),
            }
        }
        Token::Do => {
            tokens.next();
            let body = Box::new(statement(tokens)?);
            tokens.consume_arr([Token::While, Token::OpenParen])?;
            let condition = expression(tokens, None)?;
            tokens.consume_arr([Token::CloseParen, Token::Semicolon])?;
            Statement::DoWhile { body, condition }
        }
        Token::For => {
            tokens.next();
            tokens.consume(Token::OpenParen)?;
            let init = for_init(tokens)?;

            let condition = optional_expr(tokens, Token::Semicolon)?;

            let post = optional_expr(tokens, Token::CloseParen)?;

            let body = Box::new(statement(tokens)?);

            Statement::For {
                init,
                condition,
                post,
                body,
            }
        }
        _ => {
            let e = expression(tokens, None)?;

            tokens.consume(Token::Semicolon)?;

            Statement::Exp(e)
        }
    })
}

fn optional_expr(tokens: &mut TokenIter, delim: Token) -> Result<Option<Expression>, Error> {
    if tokens.consume(delim.clone()).is_ok() {
        Ok(None)
    } else {
        let expression = expression(tokens, None)?;
        tokens.consume(delim)?;
        Ok(Some(expression))
    }
}

// for debugging
#[allow(dead_code)]
fn print_return<T: fmt::Debug>(mut f: impl FnMut() -> T) -> T {
    let val = f();
    eprintln!("{:?}", val);
    val
}

fn for_init(tokens: &mut TokenIter) -> Result<Option<ForInit>, Error> {
    if let Ok(declaration) = var_declaration(tokens, None) {
        Ok(Some(ForInit::D(declaration)))
    } else {
        let res = optional_expr(tokens, Token::Semicolon)?.map(ForInit::E);
        Ok(res)
    }
}

#[derive(Debug)]
pub enum BlockItem {
    S(Statement),
    D(Declaration),
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct DebugStatement {
    statement: Statement,
    line: usize,
}

#[derive(Debug)]
pub enum Statement {
    Ret(Expression),
    Exp(Expression),
    If {
        condition: Expression,
        then: Box<Statement>,
        r#else: Option<Box<Statement>>,
    },
    Break,
    Continue,
    While {
        condition: Expression,
        body: Box<Self>,
    },
    DoWhile {
        body: Box<Self>,
        condition: Expression,
    },
    For {
        init: Option<ForInit>,
        condition: Option<Expression>,
        post: Option<Expression>,
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
        val: ConstExpr,
        body: Box<Self>,
    },
}

#[derive(Debug)]
pub enum ForInit {
    D(VariableDeclaration),
    E(Expression),
}

#[derive(Debug, Clone)]
pub enum Label {
    Named(Identifier),
    Case(ConstExpr),
    Default,
}

#[derive(Debug)]
pub enum Declaration {
    Function {
        name: Identifier,
        params: ParamList,
        body: Option<Block>,
        storage_class: Option<StorageClass>,
        r#type: FnType,
    },
    Var {
        name: Identifier,
        init: Option<Expression>,
        storage_class: Option<StorageClass>,
        r#type: VarType,
    },
}
/*
pub struct Declaration {
    pub name: Rc<Identifier>,
    pub init: Option<Expression>,
}
*/

fn expression(tokens: &mut TokenIter, min_precedence: Option<u8>) -> Result<Expression, Error> {
    let precedence = min_precedence.unwrap_or(0);

    let mut left = factor(tokens)?.into();

    while let Some(operator) = binary_operator(tokens, precedence) {
        match operator {
            BinaryOperator::Equals => {
                let right = expression(tokens, Some(operator.precedence()))?;
                left = Expression::Assignment((left, right).into());
            }
            BinaryOperator::Ternary => {
                let middle = expression(tokens, None)?;
                tokens.consume(Token::Colon)?;
                let right = expression(tokens, Some(operator.precedence()))?;
                left = Expression::Conditional {
                    condition: left.into(),
                    r#true: middle.into(),
                    r#false: right.into(),
                }
            }
            operator if operator.compound() => {
                let right = expression(tokens, Some(operator.precedence()))?;
                left = Expression::Binary(Binary {
                    left: Box::new(left),
                    right: Box::new(right),
                    operator,
                });
            }
            operator => {
                let right = expression(tokens, Some(operator.precedence() + 1))?;
                left = Expression::Binary(Binary {
                    left: Box::new(left),
                    right: Box::new(right),
                    operator,
                });
            }
        }
    }
    Ok(left)
}

fn binary_operator(tokens: &mut TokenIter, min_precedence: u8) -> Option<BinaryOperator> {
    let token = match tokens.peek()? {
        Token::Plus => Some(BinaryOperator::Add),
        Token::Minus => Some(BinaryOperator::Subtract),
        Token::Asterisk => Some(BinaryOperator::Multiply),
        Token::Slash => Some(BinaryOperator::Divide),
        Token::Percent => Some(BinaryOperator::Remainder),

        Token::LeftShift => Some(BinaryOperator::LeftShift),
        Token::RightShift => Some(BinaryOperator::RightShift),
        Token::Ampersand => Some(BinaryOperator::BitAnd),
        Token::Bar => Some(BinaryOperator::BitOr),
        Token::Caret => Some(BinaryOperator::Xor),

        Token::LogicalAnd => Some(BinaryOperator::LogAnd),
        Token::LogicalOr => Some(BinaryOperator::LogOr),

        Token::EqualTo => Some(BinaryOperator::EqualTo),
        Token::NotEqual => Some(BinaryOperator::NotEqual),
        Token::LessThan => Some(BinaryOperator::LessThan),
        Token::GreaterThan => Some(BinaryOperator::GreaterThan),
        Token::Leq => Some(BinaryOperator::Leq),
        Token::Geq => Some(BinaryOperator::Geq),

        Token::Equals => Some(BinaryOperator::Equals),
        Token::PlusEqual => Some(BinaryOperator::PlusEquals),
        Token::MinusEqual => Some(BinaryOperator::MinusEquals),
        Token::TimesEqual => Some(BinaryOperator::TimesEqual),
        Token::DivEqual => Some(BinaryOperator::DivEqual),
        Token::PercentEqual => Some(BinaryOperator::RemEqual),

        Token::BitAndEqual => Some(BinaryOperator::BitAndEqual),

        Token::BitOrEqual => Some(BinaryOperator::BitOrEqual),

        Token::BitXorEqual => Some(BinaryOperator::BitXorEqual),
        Token::LeftShiftEqual => Some(BinaryOperator::LeftShiftEqual),
        Token::RightShiftEqual => Some(BinaryOperator::RightShiftEqual),

        Token::QuestionMark => Some(BinaryOperator::Ternary),

        _ => None,
    }?;
    if token.precedence() >= min_precedence {
        tokens.next();
        Some(token)
    } else {
        None
    }
}
use super::lex;
fn factor(tokens: &mut TokenIter) -> Result<Factor, Error> {
    let factor = match tokens.consume_any()? {
        Token::Increment => {
            let exp = Box::new(factor(tokens)?.into());
            Ok(Factor::PrefixIncrement(exp))
        }
        Token::Decrement => {
            let exp = Box::new(factor(tokens)?.into());
            Ok(Factor::PrefixDecrement(exp))
        }
        Token::Constant(c) => Ok(Factor::Const(c)),

        t @ (Token::Minus | Token::Tilde | Token::Not) => {
            let operator = if t == Token::Minus {
                UnaryOperator::Negate
            } else if t == Token::Tilde {
                UnaryOperator::Complement
            } else {
                UnaryOperator::Not
            };
            let exp = Box::new(factor(tokens)?.into());
            Ok(Factor::Unary(Unary { exp, op: operator }))
        }
        Token::OpenParen => {
            if let Some(target) = tokens.consume_type() {
                tokens.consume(Token::CloseParen)?;
                let exp = factor(tokens)?.into();
                Ok(Factor::Cast { target, exp })
            } else {
                let exp = Box::new(expression(tokens, None)?);
                tokens.consume(Token::CloseParen)?;
                Ok(Factor::Nested(exp))
            }
        }
        Token::Identifier(ident) => {
            if tokens.peek() == Some(&Token::OpenParen) {
                tokens.next();
                let args = argument_list(tokens)?;
                Ok(Factor::FunctionCall { name: ident, args })
            } else {
                Ok(Factor::Var(ident))
            }
        }

        e => {
            eprintln!("errored in {e:?}");
            Err(Error::ExpectedExpression)
        }
    }?;
    match tokens.next_if(|x| x == &Token::Increment || x == &Token::Decrement) {
        Some(Token::Increment) => Ok(Factor::PostfixIncrement(Box::new(factor.into()))),
        Some(Token::Decrement) => Ok(Factor::PostfixDecrement(Box::new(factor.into()))),
        _ => Ok(factor),
    }
}

fn argument_list(tokens: &mut TokenIter) -> Result<Box<[Expression]>, Error> {
    if tokens.consume(Token::CloseParen).is_ok() {
        return Ok(Box::new([]));
    }

    let mut list = Vec::new();
    loop {
        list.push(expression(tokens, None)?);
        match tokens.consume_any()? {
            Token::Comma => {}
            Token::CloseParen => {
                break;
            }
            _ => return Err(Error::Catchall("idk")),
        }
    }

    Ok(list.into())
}

#[derive(Debug, Clone)]
pub enum Expression {
    Assignment(Box<(Self, Self)>),
    Binary(Binary),
    Cast {
        target: VarType,
        exp: Box<Self>,
    },

    //factors
    PostfixIncrement(Box<Self>),
    PostfixDecrement(Box<Self>),
    PrefixIncrement(Box<Self>),
    PrefixDecrement(Box<Self>),

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

#[derive(Debug, Copy, Clone)]
pub enum ConstExpr {
    Literal(Constant),
}

impl ConstExpr {
    pub const fn long(&self) -> i64 {
        match self {
            Self::Literal(Constant::Long(l)) => *l,
            Self::Literal(Constant::Integer(i)) => *i as i64,
        }
    }

    pub const fn int(&self) -> i32 {
        match self {
            Self::Literal(Constant::Integer(i)) => *i,
            Self::Literal(Constant::Long(l)) => *l as i32,
        }
    }
}

impl Expression {
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
            Self::Const(Constant::Integer(c)) => Some(StaticInit::Int(*c)),
            _ => None,
        }
    }
}

use crate::semantics::typecheck::StaticInit;

impl TryFrom<Expression> for Factor {
    type Error = ();

    fn try_from(expression: Expression) -> Result<Factor, ()> {
        match expression {
            Expression::Var(v) => Ok(Self::Var(v)),
            Expression::Const(i) => Ok(Self::Const(i)),
            Expression::Unary(u) => Ok(Self::Unary(u)),
            Expression::PrefixIncrement(p) => Ok(Self::PrefixIncrement(p)),
            Expression::PrefixDecrement(p) => Ok(Self::PrefixDecrement(p)),
            Expression::PostfixIncrement(p) => Ok(Self::PostfixIncrement(p)),
            Expression::PostfixDecrement(p) => Ok(Self::PostfixDecrement(p)),

            Expression::Nested(e) => Ok(Self::Nested(e)),

            _ => Err(()),
        }
    }
}

impl From<Factor> for Expression {
    fn from(factor: Factor) -> Self {
        match factor {
            Factor::Cast { target, exp } => {
                let factor: Factor = *exp;
                let exp: Expression = factor.into();
                Self::Cast {
                    target,
                    exp: Box::new(exp),
                }
            }
            Factor::Var(v) => Self::Var(v),
            Factor::Const(c) => Self::Const(c),
            Factor::Unary(u) => Self::Unary(u),
            Factor::PrefixIncrement(p) => Self::PrefixIncrement(p),
            Factor::PrefixDecrement(p) => Self::PrefixDecrement(p),
            Factor::PostfixIncrement(p) => Self::PostfixIncrement(p),
            Factor::PostfixDecrement(p) => Self::PostfixDecrement(p),

            Factor::Nested(e) => Self::Nested(e),

            Factor::FunctionCall { name, args } => Self::FunctionCall { name, args },
        }
    }
}

#[derive(Debug, Clone)]
pub enum Factor {
    Cast {
        target: VarType,
        exp: Box<Self>,
    },
    Var(Identifier),
    Const(lex::Constant),
    Unary(Unary),
    PrefixIncrement(Box<Expression>),
    PostfixIncrement(Box<Expression>),
    PrefixDecrement(Box<Expression>),
    PostfixDecrement(Box<Expression>),
    Nested(Box<Expression>),
    FunctionCall {
        name: Identifier,
        args: Box<[Expression]>,
    },
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub exp: Box<Expression>,
    pub op: UnaryOperator,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum UnaryOperator {
    Complement,
    Negate,
    Not,
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub operator: BinaryOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum BinaryOperator {
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

impl BinaryOperator {
    const fn precedence(&self) -> u8 {
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

impl Display for Program {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Program(\n{:?}\n)", self.0)
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Statement::Ret(ret) => write!(f, "Return(\n{:?}\n)", ret),
            Statement::Exp(e) => write!(f, "(\n{:?}\n)", e),
            Statement::Null => write!(f, "(\nnull\n)"),

            Statement::If {
                condition,
                then,
                r#else,
            } => write!(f, "if(\n{condition:?}\n){then}{else:?}"),
            Statement::Label { label, body } => write!(f, "LABEL\n{label}:{body}\n"),

            Statement::Goto(name) => write!(f, "goto\n{name}:\n"),

            Statement::Compound(Block(block)) => writeln!(f, "{{{block:?}}}"),
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

impl From<Unary> for Factor {
    fn from(u: Unary) -> Self {
        Self::Unary(u)
    }
}

#[derive(Debug)]
pub struct DebugError {
    pub e: Error,
    pub line: usize,
}

const fn expected_eof<T>() -> Result<T, Error> {
    Err(Error::UnexpectedEof)
}

#[derive(Debug)]
pub enum Error {
    UnexpectedEof,
    Expected(Token),
    ExpectedIdentifier,
    ExpectedConstant,
    ExpectedExpression,
    ExpectedAnyKeyword,
    Catchall(&'static str),
    ExtraStuff,
    DoubleDef,
    NoType,
    ConflictingLinkage,
    InvalidSpecifiers,
    InvalidType(SpeclistFsm),
    NoStorageClass,
}
