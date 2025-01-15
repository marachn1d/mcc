use super::lex::Constant as LexConstant;
use super::lex::Identifier;
use super::lex::Keyword;
use super::Token;
use std::fmt::{self, Display, Formatter};

pub fn parse(mut tokens: &[Token]) -> Result<Program, Error> {
    let program = program(&mut tokens)?;
    if tokens.is_empty() {
        Ok(program)
    } else {
        Err(Error::ExtraStuff)
    }
}

fn program(tokens: &mut &[Token]) -> Result<Program, Error> {
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
fn function(tokens: &mut &[Token]) -> Result<Function, Error> {
    consume(tokens, Keyword::Int, "function")?;
    let name = consume_identifier(tokens, "function")?;
    consume(tokens, Token::OpenParen, "function")?;
    consume(tokens, Keyword::Void, "function")?;
    consume(tokens, Token::CloseParen, "function")?;
    consume(tokens, Token::OpenBrace, "function")?;
    let body = statement(tokens)?;
    consume(tokens, Token::CloseBrace, "function")?;
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

fn statement(tokens: &mut &[Token]) -> Result<Statement, Error> {
    consume(tokens, Keyword::Return, "statement")?;
    let expression = expression(tokens, None)?;
    consume(tokens, Token::Semicolon, "statement")?;
    Ok(Statement { ret: expression })
}

#[derive(Debug)]
pub struct Statement {
    pub ret: Expression,
}

fn expression(tokens: &mut &[Token], precedence: Option<u8>) -> Result<Expression, Error> {
    let precedence = precedence.unwrap_or(0);
    let mut left = Expression::Factor(factor(tokens)?);
    while let Some(operator) = binary_operator(tokens, precedence) {
        *tokens = &tokens[1..];
        let right = expression(tokens, Some(precedence + 1))?;
        left = Expression::Binary(Binary {
            left: Box::new(left),
            right: Box::new(right),
            operator,
        })
    }
    Ok(left)
}

fn binary_operator(tokens: &mut &[Token], min_precedence: u8) -> Option<BinaryOperator> {
    let token = match tokens.first() {
        Some(Token::Plus) => Some(BinaryOperator::Add),
        Some(Token::Minus) => Some(BinaryOperator::Subtract),
        Some(Token::Asterisk) => Some(BinaryOperator::Multiply),
        Some(Token::Slash) => Some(BinaryOperator::Divide),
        Some(Token::Percent) => Some(BinaryOperator::Remainder),
        _ => None,
    }?;
    if token.precedence() >= min_precedence {
        *tokens = &tokens[1..];
        Some(token)
    } else {
        None
    }
}

fn factor(tokens: &mut &[Token]) -> Result<Factor, Error> {
    match tokens.first() {
        Some(Token::Constant(crate::compiler::lex::Constant::Integer(c))) => {
            *tokens = &tokens[1..];
            Ok(Factor::Int(*c))
        }
        Some(t @ (Token::Minus | Token::Tilde)) => {
            let operator = if t == &Token::Minus {
                UnaryOperator::Negate
            } else {
                UnaryOperator::Complement
            };
            *tokens = &tokens[1..];
            let factor = Box::new(factor(tokens)?);
            Ok(Factor::Unary(Unary {
                exp: factor,
                op: operator,
            }))
        }
        Some(Token::OpenParen) => {
            *tokens = &tokens[1..];
            let exp = Box::new(expression(tokens)?);
            consume(tokens, Token::CloseParen, "factor")?;
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

fn unary(tokens: &mut &[Token], operator: UnaryOperator) -> Result<Factor, Error> {
    let factor = Box::new(factor(tokens)?);
    let unary = Unary {
        exp: factor,
        op: operator,
    };
    Ok(Factor::Unary(unary))
}

#[derive(Debug)]
pub struct Unary {
    pub exp: Box<Factor>,
    pub op: UnaryOperator,
}

#[derive(Debug)]
pub enum UnaryOperator {
    Complement,
    Negate,
}

#[derive(Debug)]
pub struct Binary {
    pub operator: BinaryOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
}

impl BinaryOperator {
    const fn precedence(&self) -> u8 {
        match self {
            Self::Add => 45,
            Self::Subtract => 45,
            Self::Multiply => 50,
            Self::Divide => 50,
            Self::Remainder => 50,
        }
    }
}

fn constant(constant: LexConstant) -> Factor {
    let LexConstant::Integer(inner) = constant;
    Factor::Int(inner)
}

fn consume<T: PartialEq<Token> + Into<Token>>(
    tokens: &mut &[Token],
    type_: T,
    for_node: &str,
) -> Result<(), Error> {
    if tokens.first().is_some_and(|x| &type_ == x) {
        *tokens = &tokens[1..];
        Ok(())
    } else {
        Err(Error::Expected {
            token: type_.into(),
            for_node: for_node.into(),
        })
    }
}

fn consume_identifier(tokens: &mut &[Token], for_node: &str) -> Result<Identifier, Error> {
    if let Some(Token::Identifier(ident)) = tokens.first() {
        *tokens = &tokens[1..];
        let ident: Identifier = ident.clone();
        Ok(ident.clone())
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
    Expected { token: Token, for_node: String },
    ExpectedIdentifier { for_node: String },
    ExpectedConstant { constant: String, for_node: String },
    ExpectedExpression,
    ExtraStuff,
}
