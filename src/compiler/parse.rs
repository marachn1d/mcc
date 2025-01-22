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
    let body = statement(tokens)?;
    consume(tokens, Token::CloseBrace)?;
    Ok(Function { name, body })
}

#[derive(Debug)]
pub struct Function {
    pub name: Identifier,
    pub body: Statement,
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Function(\nname={},\nbody={}\n)", self.name, self.body)
    }
}

fn statement(tokens: &mut TokenIter) -> Result<Statement, Error> {
    consume(tokens, Keyword::Return)?;
    let expression = expression(tokens, None)?;
    consume(tokens, Token::Semicolon)?;
    Ok(Statement { ret: expression })
}

#[derive(Debug)]
pub struct Statement {
    pub ret: Expression,
}

fn expression(tokens: &mut TokenIter, min_precedence: Option<u8>) -> Result<Expression, Error> {
    let precedence = min_precedence.unwrap_or(0);
    let mut left = Expression::Factor(factor(tokens)?);
    while let Some(operator) = binary_operator(tokens, precedence) {
        //tokens.next();
        //*tokens = &tokens[1..];
        let right = expression(tokens, Some(operator.precedence() + 1))?;
        left = Expression::Binary(Binary {
            left: Box::new(left),
            right: Box::new(right),
            operator,
        });
    }
    Ok(left)
}

fn binary_operator(tokens: &mut TokenIter, min_precedence: u8) -> Option<BinaryOperator> {
    let token = match tokens.peek() {
        Some(Token::Plus) => Some(BinaryOperator::Add),
        Some(Token::Minus) => Some(BinaryOperator::Subtract),
        Some(Token::Asterisk) => Some(BinaryOperator::Multiply),
        Some(Token::Slash) => Some(BinaryOperator::Divide),
        Some(Token::Percent) => Some(BinaryOperator::Remainder),

        Some(Token::LeftShift) => Some(BinaryOperator::LeftShift),
        Some(Token::RightShift) => Some(BinaryOperator::RightShift),
        Some(Token::Ampersand) => Some(BinaryOperator::BitAnd),
        Some(Token::Bar) => Some(BinaryOperator::BitOr),
        Some(Token::Caret) => Some(BinaryOperator::Xor),

        Some(Token::LogicalAnd) => Some(BinaryOperator::LogAnd),
        Some(Token::LogicalOr) => Some(BinaryOperator::LogOr),

        Some(Token::EqualTo) => Some(BinaryOperator::Equal),
        Some(Token::NotEqual) => Some(BinaryOperator::NotEqual),
        Some(Token::LessThan) => Some(BinaryOperator::LessThan),
        Some(Token::GreaterThan) => Some(BinaryOperator::GreaterThan),
        Some(Token::Leq) => Some(BinaryOperator::Leq),
        Some(Token::Geq) => Some(BinaryOperator::Geq),

        _ => None,
    }?;
    if token.precedence() >= min_precedence {
        tokens.next();
        Some(token)
    } else {
        None
    }
}

fn factor(tokens: &mut TokenIter) -> Result<Factor, Error> {
    match tokens.next() {
        Some(Token::Constant(crate::compiler::lex::Constant::Integer(c))) => Ok(Factor::Int(c)),
        Some(t @ (Token::Minus | Token::Tilde | Token::Not)) => {
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
        Some(Token::OpenParen) => {
            let exp = Box::new(expression(tokens, None)?);
            consume(tokens, Token::CloseParen)?;
            Ok(Factor::Nested(exp))
        }
        None => Err(Error::UnexpectedEof),
        _ => Err(Error::ExpectedExpression),
    }
}

#[derive(Debug)]
pub enum Expression {
    Factor(Factor),
    Binary(Binary),
}

#[derive(Debug)]
pub enum Factor {
    Int(u64),
    Unary(Unary),
    Nested(Box<Expression>),
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
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    Leq,
    Geq,
}

impl BinaryOperator {
    const fn precedence(&self) -> u8 {
        match self {
            Self::LogOr => 5,
            Self::LogAnd => 10,
            Self::BitOr => 15,
            Self::Xor => 20,
            Self::BitAnd => 25,
            Self::Equal | Self::NotEqual => 30,
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
        Err(Error::ExpectedIdentifier {
            for_node: for_node.into(),
        })
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Program(\n{}\n)", self.0)
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Return(\n{:?}\n)", self.ret)
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
    ExpectedIdentifier { for_node: String },
    ExpectedConstant { constant: String, for_node: String },
    ExpectedExpression,
    Catchall(String),
    ExtraStuff,
}
