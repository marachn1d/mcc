use crate::lex::Identifier;
use crate::parse;
use crate::parse::BlockItem as AstBlockItem;

use crate::parse::Block as AstBlock;
use crate::parse::Expression as AstExpression;
use crate::parse::Function as AstFunction;
use crate::parse::Program as AstProgram;
use crate::parse::Statement as AstStatement;
use parse::Binary as AstBinary;
use parse::Declaration as AstDeclaration;
use parse::Factor as AstFactor;
use parse::Label as AstLabel;
use parse::Unary as AstUnary;

use std::sync::atomic::{AtomicU32, Ordering};

static COUNTER: AtomicU32 = AtomicU32::new(0);

use std::collections::{HashMap, HashSet};
use std::rc::Rc;
type VarMap = HashMap<Rc<Identifier>, Var>;

#[derive(Clone)]
struct Var {
    name: Rc<Identifier>,
    from_current_block: bool,
}

impl Var {
    fn new(name: &Rc<Identifier>) -> Self {
        Self {
            name: name.clone(),
            from_current_block: true,
        }
    }
}

pub fn resolve(
    AstProgram(function): AstProgram,
) -> Result<(AstProgram, HashSet<Rc<Identifier>>), Error> {
    let mut map: VarMap = VarMap::new();
    let program = AstProgram(resolve_function(function, &mut map)?);
    let vars: HashSet<Rc<Identifier>> = map.into_values().map(|x| x.name).collect();
    Ok((program, vars))
}

fn resolve_function(
    AstFunction { name, body }: AstFunction,
    map: &mut VarMap,
) -> Result<AstFunction, Error> {
    Ok(AstFunction {
        name,
        body: resolve_block(body, map)?,
    })
}

fn resolve_block(block: AstBlock, map: &mut VarMap) -> Result<AstBlock, Error> {
    let mut block_vec = Vec::with_capacity(block.0.len());
    for item in block.0 {
        block_vec.push(resolve_block_item(item, map)?);
    }
    Ok(AstBlock(block_vec.into_boxed_slice()))
}

fn resolve_block_item(block: AstBlockItem, map: &mut VarMap) -> Result<AstBlockItem, Error> {
    Ok(match block {
        AstBlockItem::S(statement) => AstBlockItem::S(resolve_statement(statement, map)?),
        AstBlockItem::D(statement) => AstBlockItem::D(resolve_declaration(statement, map)?),
    })
}

use std::collections::hash_map::Entry;
fn resolve_declaration(dec: AstDeclaration, map: &mut VarMap) -> Result<AstDeclaration, Error> {
    let entry = map.entry(dec.name.clone());
    match entry {
        Entry::Occupied(mut e) if !e.get().from_current_block => {
            let unique: Rc<Identifier> = new_var(&dec.name.0).into();
            *e.get_mut() = Var::new(&unique);
            let init = if let Some(init) = dec.init {
                Some(resolve_expression(init, map)?)
            } else {
                None
            };
            Ok(AstDeclaration { name: unique, init })
        }
        Entry::Vacant(e) => {
            let unique: Rc<Identifier> = new_var(&dec.name.0).into();
            e.insert(Var::new(&unique));
            let init = if let Some(init) = dec.init {
                Some(resolve_expression(init, map)?)
            } else {
                None
            };
            Ok(AstDeclaration { name: unique, init })
        }
        _ => Err(Error::DuplicateDeclaration),
    }
}

fn resolve_statement(statement: AstStatement, map: &mut VarMap) -> Result<AstStatement, Error> {
    match statement {
        AstStatement::Ret(exp) => Ok(AstStatement::Ret(resolve_expression(exp, map)?)),
        AstStatement::Null => Ok(AstStatement::Null),
        AstStatement::Exp(exp) => Ok(AstStatement::Exp(resolve_expression(exp, map)?)),
        AstStatement::If {
            condition,
            then,
            r#else,
        } => {
            let condition = resolve_expression(condition, map)?;
            let then = Box::new(resolve_statement(*then, map)?);
            let r#else = match r#else {
                None => None,
                Some(r#else) => Some(Box::new(resolve_statement(*r#else, map)?)),
            };
            Ok(AstStatement::If {
                condition,
                then,
                r#else,
            })
        }
        AstStatement::Label(AstLabel::C17 { label, body }) => {
            let body = Box::new(resolve_statement(*body, map)?);
            Ok(AstStatement::Label(AstLabel::C17 { label, body }))
        }
        AstStatement::Compound(block) => {
            let mut new_scope = map.clone();
            for var in new_scope.values_mut() {
                var.from_current_block = false;
            }
            let block = resolve_block(block, &mut new_scope)?;
            Ok(AstStatement::Compound(block))
        }

        statement @ AstStatement::Label(AstLabel::C23(_)) => Ok(statement),
        statement @ AstStatement::Goto(_) => Ok(statement),
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

        AstExpression::Conditional {
            condition,
            r#true,
            r#false,
        } => {
            let condition = Box::new(resolve_expression(*condition, map)?);

            let r#true = Box::new(resolve_expression(*r#true, map)?);

            let r#false = Box::new(resolve_expression(*r#false, map)?);
            Ok(AstExpression::Conditional {
                condition,
                r#true,
                r#false,
            })
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
                Ok(AstFactor::Var(key.name.clone()))
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
