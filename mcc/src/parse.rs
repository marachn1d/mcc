mod specifier_list;

pub use ast::parse::{
    Binary, Block, BlockItem, Bop, Dec, Expr, FnDec, FnType, ForInit, Label, Param, ParamList,
    Program, Stmnt, StorageClass, UnOp, Unary, VarDec,
};
pub use ast::{Arr, VarType};

pub use ast::parse::inc_dec::{self, *};

use util::TokenIter;

use super::Token;

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

fn declaration(tokens: &mut TokenIter) -> Result<Dec, Error> {
    let spec_list @ SpecifierList { sc, typ } = specifiers(tokens)?;

    let name = tokens.consume_identifier()?;

    match tokens.consume_any()? {
        Token::Equals => top_level_vardec(tokens, spec_list, name).map(Dec::from),
        Token::OpenParen => top_level_fndec(tokens, spec_list, name).map(Dec::from),
        // Variable Declaration no Initializer
        Token::Semicolon => Ok(Dec::Var(VarDec {
            name,
            init: None,
            sc,
            typ,
        })),
        _ => Err(Error::Catchall("expected initializer or semicolon")),
    }
}

fn top_level_fndec(
    tokens: &mut TokenIter,
    SpecifierList { sc, typ }: SpecifierList,
    name: String,
) -> Result<FnDec, Error> {
    let params = param_list(tokens)?;

    let fn_type = FnType {
        ret: Some(typ),
        params: params.iter().map(|p| p.typ).collect(),
    };

    let body = if tokens.next_if(|x| x == &Token::Semicolon).is_some() {
        None
    } else {
        Some(block(tokens)?)
    };

    Ok(FnDec {
        name,
        params,
        body,
        typ: fn_type,
        sc,
    })
}

fn top_level_vardec(
    iter: &mut TokenIter,
    SpecifierList { sc, typ }: SpecifierList,
    name: String,
) -> Result<VarDec, Error> {
    let exp = expression(iter, None)?;
    iter.consume(Token::Semicolon)?;
    Ok(VarDec {
        name,
        init: Some(exp),
        sc,
        typ,
    })
}

// type specifier

pub struct SpecifierList {
    sc: Option<StorageClass>,
    typ: VarType,
}

fn specifiers(tokens: &mut TokenIter) -> Result<SpecifierList, Error> {
    specifier_list::get_specifiers(tokens).and_then(specifier_list::SpeclistFsm::done)
}

fn type_specifier(tokens: &mut TokenIter) -> Result<VarType, Error> {
    specifier_list::get_specifiers(tokens).and_then(specifier_list::SpeclistFsm::type_specifier)
}

fn param_list(tokens: &mut TokenIter) -> Result<ParamList, Error> {
    if tokens.consume(Token::Void).is_ok() {
        tokens.consume(Token::CloseParen)?;
        return Ok(Box::new([]));
    }

    let mut params = Vec::new();

    let (mut p, mut last) = param(tokens)?;

    while !last {
        params.push(p);
        (p, last) = param(tokens)?;
    }

    params.push(p);
    Ok(params.into())
}

fn param(tokens: &mut TokenIter) -> Result<(Param, bool), Error> {
    let typ = type_specifier(tokens)?;

    let name = tokens.consume_identifier()?;
    let last = match tokens.consume_any()? {
        Token::Comma => Ok(false),
        Token::CloseParen => Ok(true),
        other => Err(Error::Catchall(
            format!("expected ',' or ')', got {other}.").leak(),
        )),
    }?;
    Ok((Param { typ, name }, last))
}

fn block(tokens: &mut TokenIter) -> Result<Block, Error> {
    tokens.consume(Token::OpenBrace)?;
    let mut body = Vec::new();
    while let Some(item) = block_item(tokens)? {
        body.push(item);
    }
    tokens.consume(Token::CloseBrace)?;
    Ok(body.into())
}

fn block_item(tokens: &mut TokenIter) -> Result<Option<BlockItem>, Error> {
    match tokens.peek_any()? {
        t if t.specifier() => Ok(Some(BlockItem::D(declaration(tokens)?))),
        Token::CloseBrace => Ok(None),
        _ => Ok(Some(BlockItem::S(statement(tokens)?))),
    }
}

fn var_declaration(tokens: &mut TokenIter, sc: Option<StorageClass>) -> Result<VarDec, Error> {
    let typ = type_specifier(tokens)?;
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
    Ok(VarDec {
        name,
        init,
        typ,
        sc,
    })
}

fn statement(tokens: &mut TokenIter) -> Result<Stmnt, Error> {
    Ok(match tokens.peek_any()? {
        Token::Return => {
            tokens.next();
            let expression = expression(tokens, None)?;
            tokens.consume(Token::Semicolon)?;
            Stmnt::Ret(expression)
        }
        Token::Semicolon => {
            tokens.next();
            Stmnt::Null
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
            Stmnt::If {
                condition,
                then: Box::new(stmnt),
                r#else,
            }
        }
        Token::Switch => {
            tokens.next();
            tokens.consume(Token::OpenParen)?;
            let expr = expression(tokens, None)?;
            tokens.consume(Token::CloseParen)?;
            let body = Box::new(statement(tokens)?);
            Stmnt::Switch { val: expr, body }
        }
        Token::Goto => {
            tokens.next();
            let identifier = tokens.consume_identifier()?;
            tokens.consume(Token::Semicolon)?;
            Stmnt::Goto(identifier)
        }
        // LABEL
        Token::Ident(_) if tokens.peek_peek().is_some_and(|x| x == &Token::Colon) => {
            let name = tokens.consume_identifier()?;
            let label = Label::Named(name.clone());
            tokens.next();
            let body = statement(tokens)?.into();

            Stmnt::Label { label, body }
        }

        Token::Default => {
            tokens.next();
            tokens.consume(Token::Colon)?;
            let body = statement(tokens)?.into();
            Stmnt::Label {
                label: Label::Default,
                body,
            }
        }
        Token::OpenBrace => {
            let block = block(tokens)?;
            Stmnt::Compound(block)
        }
        Token::Break => {
            tokens.next();
            tokens.consume(Token::Semicolon)?;

            Stmnt::Break
        }
        Token::Case => {
            tokens.next();
            let Token::Const(con) = tokens.consume_any()? else {
                return Err(Error::Catchall("expected constant"));
            };

            tokens.consume(Token::Colon)?;
            let label = Label::Case(con);
            let body = statement(tokens)?.into();
            Stmnt::Label { label, body }
        }
        Token::Continue => {
            tokens.next();
            tokens.consume(Token::Semicolon)?;
            Stmnt::Continue
        }
        Token::While => {
            tokens.next();
            tokens.consume(Token::OpenParen)?;
            let condition = expression(tokens, None)?;

            tokens.consume(Token::CloseParen)?;
            Stmnt::While {
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
            Stmnt::DoWhile { body, condition }
        }
        Token::For => {
            tokens.next();
            tokens.consume(Token::OpenParen)?;
            let init = for_init(tokens)?;

            let condition = optional_expr(tokens, Token::Semicolon)?;

            let post = optional_expr(tokens, Token::CloseParen)?;

            let body = Box::new(statement(tokens)?);

            Stmnt::For {
                init,
                condition,
                post,
                body,
            }
        }
        _ => {
            let e = expression(tokens, None)?;

            tokens.consume(Token::Semicolon)?;

            Stmnt::Exp(e)
        }
    })
}

fn optional_expr(tokens: &mut TokenIter, delim: Token) -> Result<Option<Expr>, Error> {
    if tokens.consume(delim.clone()).is_ok() {
        Ok(None)
    } else {
        let expression = expression(tokens, None)?;
        tokens.consume(delim)?;
        Ok(Some(expression))
    }
}

fn for_init(tokens: &mut TokenIter) -> Result<Option<ForInit>, Error> {
    if let Ok(declaration) = var_declaration(tokens, None) {
        Ok(Some(ForInit::D(declaration)))
    } else {
        let res = optional_expr(tokens, Token::Semicolon)?.map(ForInit::E);
        Ok(res)
    }
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct DebugStmnt {
    statement: Stmnt,
    line: usize,
}

fn expression(tokens: &mut TokenIter, min_precedence: Option<u8>) -> Result<Expr, Error> {
    let precedence = min_precedence.unwrap_or(0);

    let mut left = factor(tokens)?;

    while let Some(operator) = binary_operator(tokens, precedence) {
        match operator {
            Bop::Equals => {
                let right = Box::from(expression(tokens, Some(operator.precedence()))?);
                left = Expr::Assignment {
                    dst: left.into(),
                    src: right,
                }
            }
            Bop::Ternary => {
                let middle = expression(tokens, None)?;
                tokens.consume(Token::Colon)?;
                let right = expression(tokens, Some(operator.precedence()))?;
                left = Expr::Conditional {
                    condition: left.into(),
                    r#true: middle.into(),
                    r#false: right.into(),
                }
            }
            operator if operator.compound() => {
                let right = expression(tokens, Some(operator.precedence()))?;
                left = Expr::Bin(Binary {
                    left: Box::new(left),
                    right: Box::new(right),
                    operator,
                });
            }
            operator => {
                let right = expression(tokens, Some(operator.precedence() + 1))?;
                left = Expr::Bin(Binary {
                    left: Box::new(left),
                    right: Box::new(right),
                    operator,
                });
            }
        }
    }
    Ok(left)
}

fn binary_operator(tokens: &mut TokenIter, min_precedence: u8) -> Option<Bop> {
    let token = match tokens.peek()? {
        Token::Plus => Some(Bop::Add),
        Token::Minus => Some(Bop::Subtract),
        Token::Asterisk => Some(Bop::Multiply),
        Token::Slash => Some(Bop::Divide),
        Token::Percent => Some(Bop::Remainder),

        Token::LeftShift => Some(Bop::LeftShift),
        Token::RightShift => Some(Bop::RightShift),
        Token::Ampersand => Some(Bop::BitAnd),
        Token::Bar => Some(Bop::BitOr),
        Token::Caret => Some(Bop::Xor),

        Token::LogicalAnd => Some(Bop::LogAnd),
        Token::LogicalOr => Some(Bop::LogOr),

        Token::EqualTo => Some(Bop::EqualTo),
        Token::NotEqual => Some(Bop::NotEqual),
        Token::LessThan => Some(Bop::LessThan),
        Token::GreaterThan => Some(Bop::GreaterThan),
        Token::Leq => Some(Bop::Leq),
        Token::Geq => Some(Bop::Geq),

        Token::Equals => Some(Bop::Equals),
        Token::PlusEqual => Some(Bop::PlusEquals),
        Token::MinusEqual => Some(Bop::MinusEquals),
        Token::TimesEqual => Some(Bop::TimesEqual),
        Token::DivEqual => Some(Bop::DivEqual),
        Token::PercentEqual => Some(Bop::RemEqual),

        Token::BitAndEqual => Some(Bop::BitAndEqual),

        Token::BitOrEqual => Some(Bop::BitOrEqual),

        Token::BitXorEqual => Some(Bop::BitXorEqual),
        Token::LeftShiftEqual => Some(Bop::LeftShiftEqual),
        Token::RightShiftEqual => Some(Bop::RightShiftEqual),

        Token::QuestionMark => Some(Bop::Ternary),

        _ => None,
    }?;
    if token.precedence() >= min_precedence {
        tokens.next();
        Some(token)
    } else {
        None
    }
}
fn factor(tokens: &mut TokenIter) -> Result<Expr, Error> {
    match tokens.consume_any()? {
        Token::Increment => factor(tokens).map(Expr::pre_inc),
        Token::Decrement => factor(tokens).map(Expr::pre_dec),
        Token::Const(c) => Ok(Expr::Const(c)),

        t @ (Token::Minus | Token::Tilde | Token::Not) => {
            let operator = if t == Token::Minus {
                UnOp::Negate
            } else if t == Token::Tilde {
                UnOp::Complement
            } else {
                UnOp::Not
            };
            let exp = Box::new(factor(tokens)?);
            Ok(Expr::Unary(Unary { exp, op: operator }))
        }
        Token::OpenParen => {
            if tokens.peek_any()?.type_specifier() {
                let target = type_specifier(tokens)?;
                tokens.consume(Token::CloseParen)?;
                let exp = factor(tokens)?.into();
                Ok(Expr::Cast { target, exp })
            } else {
                let exp = Box::new(expression(tokens, None)?);
                tokens.consume(Token::CloseParen)?;
                Ok(Expr::Nested(exp))
            }
        }
        Token::Ident(ident) => {
            if tokens.peek() == Some(&Token::OpenParen) {
                tokens.next();
                let args = argument_list(tokens)?;
                Ok(Expr::FunctionCall { name: ident, args })
            } else {
                Ok(Expr::Var(ident))
            }
        }

        t => Err(Error::ExpectedExpr(t)),
    }
    .map(
        |factor| match tokens.next_if(|x| x == &Token::Increment || x == &Token::Decrement) {
            Some(Token::Increment) => Expr::post_inc(factor),
            Some(Token::Decrement) => Expr::post_dec(factor),
            _ => factor,
        },
    )
}

fn argument_list(tokens: &mut TokenIter) -> Result<Box<[Expr]>, Error> {
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

#[derive(Debug)]
pub struct DebugError {
    pub e: Error,
    pub line: usize,
}

#[derive(Debug)]
pub enum Error {
    Expected(util::Expected),
    UnexpectedEof,
    ExpectedExpr(Token),
    ExpectedKeyword,
    Catchall(&'static str),
    ExtraStuff,
    DoubleDef,
    NoType,
    ConflictingLinkage,
    InvalidSpecifiers(specifier_list::SpeclistFsm),
    InvalidType(specifier_list::TyBuilder, Token),
    NoStorageClass(specifier_list::SpeclistFsm),
}

impl From<util::Expected> for Error {
    fn from(value: util::Expected) -> Self {
        Self::Expected(value)
    }
}
