use super::lex::Identifier;
use super::parse;
use super::parse::BlockItem as AstBlock;
use super::parse::Expression as AstExpression;
use super::parse::Function as AstFunction;
use super::parse::Program as AstProgram;
use super::parse::Statement as AstStatement;
use parse::Binary as AstBinary;
use parse::Declaration as AstDeclaration;
use parse::Factor as AstFactor;
use parse::Unary as AstUnary;

use std::sync::atomic::{AtomicU32, Ordering};

static COUNTER: AtomicU32 = AtomicU32::new(0);

use std::collections::HashMap;
use std::rc::Rc;
type VarMap = HashMap<Rc<Identifier>, Rc<Identifier>>;

pub fn resolve(AstProgram(function): AstProgram) -> Result<AstProgram, Error> {
    let mut map: VarMap = VarMap::new();
    Ok(AstProgram(resolve_function(function, &mut map)?))
}

fn resolve_function(function: AstFunction, map: &mut VarMap) -> Result<AstFunction, Error> {
    let mut blocks = Vec::new();
    for block in function.body {
        blocks.push(resolve_block(block, map)?);
    }
    Ok(AstFunction {
        name: function.name,
        body: blocks.into(),
    })
}

fn resolve_block(block: AstBlock, map: &mut VarMap) -> Result<AstBlock, Error> {
    Ok(match block {
        AstBlock::S(statement) => AstBlock::S(resolve_statement(statement, map)?),
        AstBlock::D(statement) => AstBlock::D(resolve_declaration(statement, map)?),
    })
}

use std::collections::hash_map::Entry;
fn resolve_declaration(dec: AstDeclaration, map: &mut VarMap) -> Result<AstDeclaration, Error> {
    if let Entry::Vacant(e) = map.entry(dec.name.clone()) {
        let unique: Rc<Identifier> = new_var(&dec.name.0).into();
        e.insert(unique.clone());
        let init = if let Some(init) = dec.init {
            Some(resolve_expression(init, map)?)
        } else {
            None
        };
        Ok(AstDeclaration { name: unique, init })
    } else {
        Err(Error::DuplicateDeclaration)
    }
}

fn resolve_statement(statement: AstStatement, map: &mut VarMap) -> Result<AstStatement, Error> {
    match statement {
        AstStatement::Ret(exp) => Ok(AstStatement::Ret(resolve_expression(exp, map)?)),
        AstStatement::Null => Ok(AstStatement::Null),
        AstStatement::Exp(exp) => Ok(AstStatement::Exp(resolve_expression(exp, map)?)),
    }
}

fn resolve_expression(exp: AstExpression, map: &mut VarMap) -> Result<AstExpression, Error> {
    match exp {
        AstExpression::Assignment(a) => {
            let (left, right) = *a;
            if left.lvalue() {
                let assignment = (
                    resolve_expression(left, map)?,
                    resolve_expression(right, map)?,
                );
                Ok(AstExpression::Assignment(assignment.into()))
            } else {
                Err(Error::InvalidLval)
            }
        }
        AstExpression::Binary(AstBinary {
            left,
            right,
            operator,
        }) if operator.compound() => {
            if !left.lvalue() {
                return Err(Error::InvalidLval);
            }
            let left = resolve_expression(*left, map)?.into();
            let right = resolve_expression(*right, map)?.into();
            Ok(AstExpression::Binary(AstBinary {
                left,
                right,
                operator,
            }))
        }

        AstExpression::Binary(AstBinary {
            left,
            right,
            operator,
        }) => {
            let left = resolve_expression(*left, map)?.into();
            let right = resolve_expression(*right, map)?.into();
            Ok(AstExpression::Binary(AstBinary {
                left,
                right,
                operator,
            }))
        }
        expression @ (AstExpression::Var(_)
        | AstExpression::Int(_)
        | AstExpression::Unary(_)
        | AstExpression::PrefixIncrement(_)
        | AstExpression::PrefixDecrement(_)
        | AstExpression::PostfixIncrement(_)
        | AstExpression::PostfixDecrement(_)
        | AstExpression::Nested(_)) => {
            let factor = AstFactor::try_from(expression).unwrap();
            Ok(resolve_factor(factor, map)?.into())
        }
    }
}

fn resolve_factor(factor: AstFactor, map: &mut VarMap) -> Result<AstFactor, Error> {
    match factor {
        AstFactor::PrefixIncrement(inner) => {
            let inner = Box::new(resolve_expression(*inner, map)?);

            if inner.lvalue() {
                Ok(AstFactor::PrefixIncrement(inner))
            } else {
                Err(Error::InvalidLval)
            }
        }
        AstFactor::PrefixDecrement(inner) => {
            let inner = Box::new(resolve_expression(*inner, map)?);

            if inner.lvalue() {
                Ok(AstFactor::PrefixDecrement(inner))
            } else {
                Err(Error::InvalidLval)
            }
        }
        AstFactor::PostfixIncrement(inner) => {
            let inner = Box::new(resolve_expression(*inner, map)?);

            if inner.lvalue() {
                Ok(AstFactor::PostfixIncrement(inner))
            } else {
                Err(Error::InvalidLval)
            }
        }
        AstFactor::PostfixDecrement(inner) => {
            let inner = Box::new(resolve_expression(*inner, map)?);

            if inner.lvalue() {
                Ok(AstFactor::PostfixDecrement(inner))
            } else {
                Err(Error::InvalidLval)
            }
        }
        AstFactor::Var(v) => {
            if let Some(key) = map.get(&v) {
                Ok(AstFactor::Var(key.clone()))
            } else {
                Err(Error::UndeclaredVar)
            }
        }
        AstFactor::Nested(exp) => Ok(AstFactor::Nested(resolve_expression(*exp, map)?.into())),
        a @ AstFactor::Int(_) => Ok(a),
        AstFactor::Unary(AstUnary { exp, op }) => {
            let factor = resolve_factor(*exp, map)?.into();
            Ok(AstFactor::Unary(AstUnary { exp: factor, op }))
        }
    }
}

fn new_var(name: &[u8]) -> Identifier {
    let name = unsafe { std::str::from_utf8_unchecked(name) };
    Identifier(
        format!(
            "t{name}.{number}",
            number = COUNTER.fetch_add(1, Ordering::SeqCst)
        )
        .into_bytes()
        .into(),
    )
}

#[derive(Debug)]
pub enum Error {
    DuplicateDeclaration,
    InvalidLval,
    UndeclaredVar,
}
