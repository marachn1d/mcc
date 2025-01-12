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
fn statement(tokens: &mut &[Token]) -> Result<Statement, Error> {
    consume(tokens, Keyword::Return, "statement")?;
    let expression = expression(tokens)?;
    consume(tokens, Token::Semicolon, "statement")?;
    Ok(Statement { ret: expression })
}

fn expression(tokens: &mut &[Token]) -> Result<Expression, Error> {
    let Some(token) = tokens.first() else {
        return Err(Error::UnexpectedEof);
    };
    *tokens = &tokens[1..];
    match token {
        Token::Constant(c) => Ok(constant(c.clone())),
        Token::Minus => unary(tokens, UnaryOperator::Negate),
        Token::Tilde => unary(tokens, UnaryOperator::Complement),
        Token::OpenParen => {
            let inner = Box::new(expression(tokens)?);
            consume(tokens, Token::CloseParen, "expression")?;
            Ok(Expression::Nested(inner))
        }
        _ => panic!("error"),
    }
}

fn unary(tokens: &mut &[Token], operator: UnaryOperator) -> Result<Expression, Error> {
    let expression = Box::new(expression(tokens)?);
    let unary = Unary {
        exp: expression,
        op: operator,
    };
    Ok(Expression::Unary(unary))
}

fn constant(constant: LexConstant) -> Expression {
    let LexConstant::Integer(inner) = constant;
    Expression::Constant(Constant(inner))
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

fn consume_constant(tokens: &mut &[Token], for_node: &str) -> Result<LexConstant, Error> {
    if let Some(Token::Constant(constant)) = tokens.first() {
        *tokens = &tokens[1..];
        Ok(constant.clone())
    } else {
        Err(Error::ExpectedIdentifier {
            for_node: for_node.into(),
        })
    }
}

#[derive(Debug)]
pub struct Program(pub Function);

impl Display for Program {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Program(\n{}\n)", self.0)
    }
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

#[derive(Debug)]
pub struct Statement {
    pub ret: Expression,
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Return(\n{:?}\n)", self.ret)
    }
}

#[derive(Debug)]
pub enum Expression {
    Constant(Constant),
    Unary(Unary),
    Nested(Box<Expression>),
}

impl From<Constant> for Expression {
    fn from(c: Constant) -> Self {
        Self::Constant(c)
    }
}

impl From<Unary> for Expression {
    fn from(u: Unary) -> Self {
        Self::Unary(u)
    }
}

#[derive(Debug)]
pub struct Unary {
    pub exp: Box<Expression>,
    pub op: UnaryOperator,
}

#[derive(Debug)]
pub enum UnaryOperator {
    Complement,
    Negate,
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
