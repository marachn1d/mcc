use super::lex::Identifier;
use super::lex::Keyword;
use super::slice_iter::TokenIter;
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
    let program = function(tokens).map(Program)?;
    if tokens.is_empty() {
        Ok(program)
    } else {
        Err(Error::ExtraStuff)
    }
}

#[derive(Debug)]
pub struct Program(pub Function);
// working with an iterator would be better but sdince the type isn't Copy it's kinda weird idk
fn function(tokens: &mut TokenIter) -> Result<Function, Error> {
    consume(tokens, Keyword::Int)?;
    let name = consume_identifier(tokens, "function")?;
    tokens
        .consume_array([
            Token::OpenParen,
            Keyword::Void.into(),
            Token::CloseParen,
            Token::OpenBrace,
        ])
        .map_err(|_| {
            Error::Catchall("Expected Function Definition after Function identifier".into())
        })?;
    let body = block_items(tokens)?;
    consume(tokens, Token::CloseBrace)?;
    Ok(Function { name, body })
}

fn block_items(tokens: &mut TokenIter) -> Result<Box<[BlockItem]>, Error> {
    let mut items = Vec::new();
    while let Some(block) = block_item(tokens)? {
        items.push(block);
    }
    Ok(items.into())
}

fn block_item(tokens: &mut TokenIter) -> Result<Option<BlockItem>, Error> {
    match tokens.peek_any()? {
        Token::Keyword(Keyword::Int) => Ok(Some(BlockItem::D(declaration(tokens)?))),
        Token::CloseBrace => Ok(None),
        _ => Ok(Some(BlockItem::S(statement(tokens)?))),
    }
}

fn declaration(tokens: &mut TokenIter) -> Result<Declaration, Error> {
    consume(tokens, Keyword::Int)?;
    let name = tokens.consume_identifier()?;

    let init = match tokens.consume_any()? {
        Token::Equals => {
            let exp = expression(tokens, None)?;
            tokens.consume(Token::Semicolon)?;
            Ok(Some(exp))
        }
        Token::Semicolon => Ok(None),
        _ => Err(Error::Catchall("expected initializer or semicolon".into())),
    }?;
    Ok(Declaration {
        name: name.into(),
        init,
    })
}

#[derive(Debug)]
pub struct Function {
    pub name: Identifier,
    pub body: Box<[BlockItem]>,
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Function(\nname={},\nbody={:?}\n)", self.name, self.body)
    }
}

fn statement(tokens: &mut TokenIter) -> Result<Statement, Error> {
    Ok(match tokens.peek_any()? {
        Token::Keyword(Keyword::Return) => {
            tokens.next();
            let expression = expression(tokens, None)?;
            consume(tokens, Token::Semicolon)?;
            Statement::Ret(expression)
        }
        Token::Semicolon => {
            tokens.next();
            Statement::Null
        }
        _ => {
            let e = expression(tokens, None)?;
            tokens.consume(Token::Semicolon)?;
            Statement::Exp(e)
        }
    })
}

#[derive(Debug)]
pub enum BlockItem {
    S(Statement),
    D(Declaration),
}

#[derive(Debug)]
pub enum Statement {
    Ret(Expression),
    Exp(Expression),
    Null,
}

use std::rc::Rc;
#[derive(Debug)]
pub struct Declaration {
    pub name: Rc<Identifier>,
    pub init: Option<Expression>,
}

fn expression(tokens: &mut TokenIter, min_precedence: Option<u8>) -> Result<Expression, Error> {
    let precedence = min_precedence.unwrap_or(0);
    let mut left = Expression::Factor(factor(tokens)?);

    // here's the way we do it: factor parses prefix operators, then this function binary_operator
    // now needs to handle postfix operators OR binary operators
    while let Some(operator) = expression_operator(tokens, precedence) {
        match operator {
            ExpressionOperator::Increment => {
                left = Expression::Factor(Factor::Increment {
                    op: Box::new(left),
                    fix: Fixness::Postfix,
                })
            }
            ExpressionOperator::Decrement => {
                left = Expression::Factor(Factor::Decrement {
                    op: Box::new(left),
                    fix: Fixness::Postfix,
                })
            }
            ExpressionOperator::Binary(BinaryOperator::Equals) => {
                let right = expression(tokens, Some(BinaryOperator::Equals.precedence()))?;
                left = Expression::Assignment((left, right).into())
            }
            ExpressionOperator::Binary(operator) => {
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

enum ExpressionOperator {
    Binary(BinaryOperator),
    Increment,
    Decrement,
}

fn expression_operator(tokens: &mut TokenIter, min_precedence: u8) -> Option<ExpressionOperator> {
    let next = tokens.peek()?;
    if next == &Token::Increment {
        tokens.next();
        return Some(ExpressionOperator::Increment);
    } else if next == &Token::Decrement {
        tokens.next();
        return Some(ExpressionOperator::Decrement);
    }
    Some(ExpressionOperator::Binary(binary_operator(
        tokens,
        min_precedence,
    )?))
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
    match tokens.consume_any()? {
        Token::Increment => {
            let exp = Box::new(expression(tokens, None)?);
            Ok(Factor::Increment {
                op: exp,
                fix: Fixness::Prefix,
            })
        }
        Token::Decrement => {
            let exp = Box::new(expression(tokens, None)?);
            Ok(Factor::Decrement {
                op: exp,
                fix: Fixness::Prefix,
            })
        }
        Token::Constant(lex::Constant::Integer(c)) => Ok(Factor::Int(c)),

        t @ (Token::Minus | Token::Tilde | Token::Not) => {
            let operator = if t == Token::Minus {
                UnaryOperator::Negate
            } else if t == Token::Tilde {
                UnaryOperator::Complement
            } else {
                UnaryOperator::Not
            };
            let factor = Box::new(factor(tokens)?);
            Ok(Factor::Unary(Unary {
                exp: factor,
                op: operator,
            }))
        }
        Token::OpenParen => {
            let exp = Box::new(expression(tokens, None)?);
            consume(tokens, Token::CloseParen)?;
            Ok(Factor::Nested(exp))
        }
        Token::Identifier(ident) => Ok(Factor::Var(ident.into())),
        _ => Err(Error::ExpectedExpression),
    }
}

fn unop(token: &Token) -> Option<UnaryOperator> {
    match token {
        Token::Minus => Some(UnaryOperator::Negate),
        Token::Tilde => Some(UnaryOperator::Complement),
        Token::Not => Some(UnaryOperator::Not),
        _ => None,
    }
}
#[derive(Debug)]
pub enum Expression {
    Assignment(Box<(Expression, Expression)>),
    Factor(Factor),
    Binary(Binary),
}

#[derive(Debug)]
pub enum Factor {
    Var(Rc<Identifier>),
    Int(u64),
    Unary(Unary),
    Increment { op: Box<Expression>, fix: Fixness },
    Decrement { op: Box<Expression>, fix: Fixness },
    Nested(Box<Expression>),
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Fixness {
    Prefix,
    Postfix,
}

#[derive(Debug)]
pub struct Unary {
    pub exp: Box<Factor>,
    pub op: UnaryOperator,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum UnaryOperator {
    Complement,
    Negate,
    Not,
}

#[derive(Debug)]
pub struct Binary {
    pub operator: BinaryOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, Eq, PartialEq)]
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
}

fn consume<T: PartialEq<Token> + Into<Token>>(
    tokens: &mut TokenIter,
    type_: T,
) -> Result<(), Error> {
    if tokens.next_if(|x| &type_ == x).is_some() {
        Ok(())
    } else {
        Err(Error::Expected(type_.into()))
    }
}

fn consume_identifier(tokens: &mut TokenIter, for_node: &str) -> Result<Identifier, Error> {
    if let Some(Token::Identifier(_)) = tokens.peek() {
        let Some(Token::Identifier(ident)) = tokens.next() else {
            unreachable!()
        };
        Ok(ident)
    } else {
        Err(Error::ExpectedIdentifier)
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Program(\n{}\n)", self.0)
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Statement::Ret(ret) => write!(f, "Return(\n{:?}\n)", ret),
            Statement::Exp(e) => write!(f, "(\n{:?}\n)", e),
            Statement::Null => write!(f, "(\nnull\n)"),
        }
    }
}

impl From<Constant> for Factor {
    fn from(c: Constant) -> Self {
        Self::Int(c.0)
    }
}

impl From<Unary> for Factor {
    fn from(u: Unary) -> Self {
        Self::Unary(u)
    }
}

#[derive(Debug)]
pub struct Constant(pub u64);

impl Display for Constant {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Constant({})", self.0)
    }
}

#[derive(Debug)]
pub enum Error {
    UnexpectedEof,
    Expected(Token),
    ExpectedIdentifier,
    ExpectedConstant,
    ExpectedExpression,
    ExpectedAnyKeyword,
    ExpectedKeyword(Keyword),
    Catchall(String),
    ExtraStuff,
}
