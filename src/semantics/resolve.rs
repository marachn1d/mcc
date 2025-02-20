use crate::lex::Identifier;
use crate::parse;
use crate::parse::BlockItem as AstBlockItem;

use crate::parse::Block as AstBlock;

use crate::parse::Expression as AstExpression;
use crate::parse::ForInit as AstForInit;
use crate::parse::Function as AstFunction;
use crate::parse::Program as AstProgram;
use crate::parse::Statement as AstStatement;
use parse::Binary as AstBinary;
use parse::Declaration as AstDeclaration;

use parse::Factor as AstFactor;
use parse::Unary as AstUnary;

use std::sync::atomic::{AtomicU32, Ordering};

static COUNTER: AtomicU32 = AtomicU32::new(0);

use std::collections::{HashMap, HashSet};
use std::rc::Rc;
type VarMap = HashMap<Rc<Identifier>, Var>;

#[derive(Clone, Debug)]
struct Var {
    name: Rc<Identifier>,
    from_current_block: bool,
    has_external_linkage: bool,
}

impl Var {
    fn new_var(name: &Rc<Identifier>) -> Self {
        Self {
            name: name.clone(),
            from_current_block: true,
            has_external_linkage: false,
        }
    }
    fn new_fn(name: &Rc<Identifier>) -> Self {
        Self {
            name: name.clone(),
            from_current_block: true,
            has_external_linkage: true,
        }
    }
}

pub fn resolve(AstProgram(functions): &mut AstProgram) -> Result<HashSet<Rc<Identifier>>, Error> {
    let mut map: VarMap = VarMap::new();
    for function in functions {
        resolve_function_dec(function, &mut map)?;
    }
    let vars: HashSet<Rc<Identifier>> = map.into_values().map(|x| x.name).collect();
    Ok(vars)
}

fn resolve_fn_dec(
    name: &mut Rc<Identifier>,
    body: &mut Option<AstBlock>,
    params: &mut parse::ParamList,
    map: &mut VarMap,
) -> Result<(), Error> {
    match map.entry(name.clone()) {
        Entry::Occupied(mut e) => {
            if e.get().from_current_block && !e.get().has_external_linkage {
                Err(Error::DuplicateDeclaration)
            } else {
                e.insert(Var::new_fn(name));
                let mut inner_map = new_scope(map);
                resolve_param(params, &mut inner_map)?;
                if let Some(body) = body {
                    resolve_block(body, &mut inner_map)?;
                }
                Ok(())
            }
        }
        Entry::Vacant(v) => {
            v.insert(Var::new_fn(name));
            let mut inner_map = new_scope(map);
            resolve_param(params, &mut inner_map)?;
            if let Some(body) = body {
                resolve_block(body, &mut inner_map)?;
            }
            Ok(())
        }
    }
}

fn resolve_function_dec(
    parse::FunctionDeclaration { name, body, params }: &mut parse::FunctionDeclaration,
    map: &mut VarMap,
) -> Result<(), Error> {
    resolve_fn_dec(name, body, params, map)
}

fn resolve_param(params: &mut parse::ParamList, map: &mut VarMap) -> Result<(), Error> {
    for param in params.iter() {
        if map
            .get(param)
            .is_some_and(|param| !param.has_external_linkage && param.from_current_block)
        {
            return Err(Error::DuplicateDeclaration);
        }

        let unique: Rc<Identifier> = new_var(&param.0).into();
        map.insert(param.clone(), Var::new_var(&unique));

        *param = unique;
    }
    Ok(())
}

fn resolve_block(block: &mut AstBlock, map: &mut VarMap) -> Result<(), Error> {
    for item in &mut block.0 {
        resolve_block_item(item, map)?;
    }
    Ok(())
}

fn resolve_block_item(block: &mut AstBlockItem, map: &mut VarMap) -> Result<(), Error> {
    match block {
        AstBlockItem::S(statement) => resolve_statement(statement, map)?,
        AstBlockItem::D(statement) => resolve_declaration(statement, map)?,
    };
    Ok(())
}

fn resolve_var_dec(
    name: &mut Rc<Identifier>,
    init: &mut Option<AstExpression>,
    map: &mut VarMap,
) -> Result<(), Error> {
    match map.entry(name.clone()) {
        Entry::Occupied(mut e) if !e.get().from_current_block => {
            let unique: Rc<Identifier> = new_var(&name.0).into();

            *e.get_mut() = Var::new_var(&unique);
            *name = unique;
            if let Some(init) = init {
                resolve_expression(init, map)?;
            }
            Ok(())
        }
        Entry::Vacant(e) => {
            let unique: Rc<Identifier> = new_var(&name.0).into();
            e.insert(Var::new_var(&unique));
            *name = unique;

            if let Some(init) = init {
                resolve_expression(init, map)?;
            }
            Ok(())
        }
        _ => Err(Error::DuplicateDeclaration),
    }
}

use std::collections::hash_map::Entry;
fn resolve_declaration(dec: &mut AstDeclaration, map: &mut VarMap) -> Result<(), Error> {
    match dec {
        AstDeclaration::Var { name, init } => resolve_var_dec(name, init, map),

        AstDeclaration::Function { body: Some(_), .. } => Err(Error::LocalFnDecBody),

        AstDeclaration::Function {
            body: None,
            name,
            params,
        } => resolve_fn_dec(name, &mut None, params, map),
    }
}

fn resolve_statement(statement: &mut AstStatement, map: &mut VarMap) -> Result<(), Error> {
    match statement {
        AstStatement::Ret(exp) => resolve_expression(exp, map),
        AstStatement::Null => Ok(()),
        AstStatement::Exp(exp) => resolve_expression(exp, map),
        AstStatement::If {
            condition,
            then,
            r#else,
        } => {
            resolve_expression(condition, map)?;
            resolve_statement(then, map)?;
            if let Some(r#else) = r#else {
                resolve_statement(r#else, map)?;
            }
            Ok(())
        }
        AstStatement::Label { body, .. } => resolve_statement(body, map),
        AstStatement::Compound(block) => {
            let mut new_scope = new_scope(map);
            resolve_block(block, &mut new_scope)
        }

        AstStatement::While {
            condition, body, ..
        } => {
            resolve_expression(condition, map)?;
            resolve_statement(body, map)
        }
        AstStatement::DoWhile {
            condition, body, ..
        } => {
            resolve_statement(body, map)?;
            resolve_expression(condition, map)
        }
        AstStatement::For {
            init,
            post,
            body,
            condition,
            ..
        } => {
            let mut new_map = new_scope(map);
            resolve_init(init, &mut new_map)?;
            if let Some(condition) = condition {
                resolve_expression(condition, &mut new_map)?;
            }
            if let Some(post) = post {
                resolve_expression(post, &mut new_map)?;
            }
            resolve_statement(body, &mut new_map)
        }

        AstStatement::Switch { val, body } => {
            resolve_expression(val, map)?;
            resolve_statement(body, map)
        }

        AstStatement::Goto(_) | AstStatement::Break | AstStatement::Continue => Ok(()),
    }
}

fn new_scope(map: &VarMap) -> VarMap {
    let mut new_scope = map.clone();
    for var in new_scope.values_mut() {
        var.from_current_block = false;
    }
    new_scope
}

fn resolve_init(init: &mut Option<AstForInit>, map: &mut VarMap) -> Result<(), Error> {
    match init {
        None => Ok(()),
        Some(AstForInit::D(dec)) => resolve_var_dec(&mut dec.name, &mut dec.init, map),
        Some(AstForInit::E(exp)) => resolve_expression(exp, map),
    }
}

fn resolve_expression(exp: &mut AstExpression, map: &mut VarMap) -> Result<(), Error> {
    match exp {
        AstExpression::Assignment(a) => {
            let (left, right): &mut (AstExpression, AstExpression) = &mut (*a);
            if left.lvalue() {
                resolve_expression(left, map)?;
                resolve_expression(right, map)
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
            resolve_expression(left, map)?;
            resolve_expression(right, map)
        }

        AstExpression::Conditional {
            condition,
            r#true,
            r#false,
        } => {
            resolve_expression(condition, map)?;
            resolve_expression(r#true, map)?;
            resolve_expression(r#false, map)
        }

        AstExpression::Binary(AstBinary { left, right, .. }) => {
            resolve_expression(left, map)?;
            resolve_expression(right, map)
        }
        AstExpression::PrefixIncrement(inner)
        | AstExpression::PrefixDecrement(inner)
        | AstExpression::PostfixIncrement(inner)
        | AstExpression::PostfixDecrement(inner) => {
            resolve_expression(inner, map)?;
            if inner.lvalue() {
                Ok(())
            } else {
                Err(Error::InvalidLval)
            }
        }

        AstExpression::Nested(inner) => resolve_expression(inner, map),
        AstExpression::Var(var) => match map.get(var) {
            Some(new_name) => {
                *var = new_name.name.clone();
                Ok(())
            }
            None => Err(Error::UndeclaredVar),
        },
        AstExpression::Int(_) => Ok(()),
        AstExpression::Unary(inner) => resolve_factor(&mut inner.exp, map),
        AstExpression::FunctionCall { name, args } => {
            if let Some(new_name) = map.get(name) {
                *name = new_name.name.clone();
                for arg in args {
                    resolve_expression(arg, map)?;
                }
                Ok(())
            } else {
                Err(Error::UndeclaredFn)
            }
        }
    }
}

fn resolve_factor(factor: &mut AstFactor, map: &mut VarMap) -> Result<(), Error> {
    match factor {
        AstFactor::PrefixIncrement(inner)
        | AstFactor::PrefixDecrement(inner)
        | AstFactor::PostfixIncrement(inner)
        | AstFactor::PostfixDecrement(inner) => {
            resolve_expression(inner, map)?;
            if inner.lvalue() {
                Ok(())
            } else {
                Err(Error::InvalidLval)
            }
        }
        AstFactor::Var(var) => match map.get(var) {
            Some(new_name) => {
                *var = new_name.name.clone();
                Ok(())
            }
            None => Err(Error::UndeclaredVar),
        },

        AstFactor::Nested(exp) => resolve_expression(exp, map),
        AstFactor::Int(_) => Ok(()),
        AstFactor::Unary(AstUnary { exp, op: _ }) => resolve_factor(exp, map),
        AstFactor::FunctionCall { name, args } => {
            if let Some(new_name) = map.get(name) {
                *name = new_name.name.clone();
                for arg in args {
                    resolve_expression(arg, map)?;
                }
                Ok(())
            } else {
                Err(Error::UndeclaredFn)
            }
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
    UndeclaredFn,
    LocalFnDecBody,
}
