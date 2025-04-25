use crate::types::ast::parse_prelude::*;

use std::sync::atomic::{AtomicU32, Ordering};

static COUNTER: AtomicU32 = AtomicU32::new(0);

use std::collections::HashMap;
type VarMap<'a> = HashMap<Key<'a>, Var<'a>>;

#[derive(Clone, Debug, PartialEq)]
struct Var<'a> {
    name: Key<'a>,
    from_current_block: bool,
    has_external_linkage: bool,
}

impl Var<'_> {
    fn new_var(name: &Identifier, sc: &Option<StorageClass>) -> Self {
        Self {
            name: name.clone(),
            from_current_block: true,
            has_external_linkage: sc.is_some_and(|x| x == StorageClass::Extern),
        }
    }
    fn new_fn(name: &Identifier, sc: &Option<StorageClass>) -> Self {
        Self {
            name: name.clone(),
            from_current_block: true,
            has_external_linkage: sc.is_some_and(|x| x == StorageClass::Extern),
        }
    }
}

pub fn resolve(Program(decs): &mut Program) -> Result<(), Error> {
    let mut map: VarMap = VarMap::new();
    for dec in decs {
        resolve_top_level_dec(dec, &mut map)?;
    }
    Ok(())
}

fn insert_fndec(
    map: &mut VarMap,
    name: &mut Identifier,
    storage_class: &mut Option<StorageClass>,
) -> Result<(), Error> {
    map.insert(name.clone(), Var::new_fn(name, storage_class));
    Ok(())

    /*
    if map.get(name).is_some_and(|var| {
        (Var::new_fn(name, storage_class) != *var)
            && (var.from_current_block && !var.has_external_linkage)
    }) {
        Err(Error::DuplicateDeclaration)
    } else {
        map.insert(name.clone(), Var::new_fn(name, storage_class));
        Ok(())
    }
        */
}

fn insert_local_var(
    map: &mut VarMap,
    name: &mut Identifier,
    storage_class: &mut Option<StorageClass>,
) -> Result<(), Error> {
    if let Some(prev_decl) = map.get(name) {
        if prev_decl.from_current_block
            && !(prev_decl.has_external_linkage && *storage_class == Some(StorageClass::Extern))
        {
            return Err(Error::ConflictingDec);
        }
    }

    if *storage_class == Some(StorageClass::Extern) {
        map.insert(
            name.clone(),
            Var {
                name: name.clone(),
                from_current_block: true,
                has_external_linkage: true,
            },
        );
    } else {
        let unique: Identifier = new_var(&name.0);
        map.insert(
            name.clone(),
            Var {
                name: unique.clone(),
                from_current_block: true,
                has_external_linkage: false,
            },
        );
        *name = unique.clone();
    };
    Ok(())
}

fn resolve_top_level_dec(dec: &mut Dec, map: &mut VarMap) -> Result<(), Error> {
    match dec {
        Dec::Var(VarDec { name, .. }) => {
            map.insert(
                name.clone(),
                Var {
                    name: name.clone(),
                    from_current_block: true,
                    has_external_linkage: true,
                },
            );
        }

        Dec::Fn(FnDec {
            name,
            params,
            sc,
            body,
            typ: _,
        }) => {
            map.insert(name.clone(), Var::new_fn(name, sc));
            let mut inner_map = new_scope(map);
            resolve_param(params, &mut inner_map)?;
            if let Some(body) = body {
                resolve_block(body, &mut inner_map)?;
            }
        }
    }
    Ok(())
}

fn resolve_fn_dec(
    name: &mut Key<'_>,
    body: &mut Option<Block>,
    params: &mut super::ParamList,
    storage_class: &mut Option<StorageClass>,
    map: &mut VarMap,
) -> Result<(), Error> {
    insert_fndec(map, name, storage_class)?;
    let mut inner_map = new_scope(map);
    resolve_param(params, &mut inner_map)?;
    if let Some(body) = body {
        resolve_block(body, &mut inner_map)?;
    }
    Ok(())
}

fn resolve_param(params: &mut super::ParamList, map: &mut VarMap) -> Result<(), Error> {
    for param in params.iter_mut().map(|x| &mut x.name) {
        if map
            .get(param)
            .is_some_and(|param| !param.has_external_linkage && param.from_current_block)
        {
            return Err(Error::DuplicateDeclaration);
        }

        let unique = new_var(&param.as_ref());
        map.insert(param.clone(), Var::new_var(&unique, &None));

        *param = unique;
    }
    Ok(())
}

fn resolve_block(block: &mut AstBlock, map: &mut VarMap) -> Result<(), Error> {
    for item in block {
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
    name: &mut Identifier,
    init: &mut Option<AstExpression>,
    sc: &mut Option<StorageClass>,
    map: &mut VarMap,
) -> Result<(), Error> {
    insert_local_var(map, name, sc)?;
    if let Some(init) = init {
        resolve_expression(init, map)?;
    }
    Ok(())
}

fn resolve_declaration(dec: &mut AstDeclaration, map: &mut VarMap) -> Result<(), Error> {
    match dec {
        AstDeclaration::Var(AstVDec {
            name,
            init,
            sc,
            typ: _,
        }) => resolve_var_dec(name, init, sc, map),

        AstDeclaration::Fn(AstFnDec { body: Some(_), .. }) => Err(Error::LocalFnDecBody),
        AstDeclaration::Fn(AstFnDec { sc, .. }) if *sc == Some(StorageClass::Static) => {
            Err(Error::StaticBlockScopeFn)
        }

        AstDeclaration::Fn(AstFnDec {
            body: None,
            name,
            params,
            sc,
            typ: _,
        }) => resolve_fn_dec(name, &mut None, params, sc, map),
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

fn new_scope<'a>(map: &VarMap<'a>) -> VarMap<'a> {
    let mut new_scope = map.clone();
    for var in new_scope.values_mut() {
        var.from_current_block = false;
    }
    new_scope
}

fn resolve_init(init: &mut Option<AstForInit>, map: &mut VarMap) -> Result<(), Error> {
    match init {
        None => Ok(()),
        Some(AstForInit::D(dec)) => resolve_var_dec(&mut dec.name, &mut dec.init, &mut dec.sc, map),
        Some(AstForInit::E(exp)) => resolve_expression(exp, map),
    }
}

fn resolve_expression(exp: &mut AstExpression, map: &mut VarMap) -> Result<(), Error> {
    match exp {
        AstExpression::Assignment { dst, src } => {
            if dst.lvalue() {
                resolve_expression(dst, map)?;
                resolve_expression(src, map)
            } else {
                Err(Error::InvalidLval)
            }
        }
        AstExpression::Bin(AstBinary {
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

        AstExpression::Bin(AstBinary { left, right, .. }) => {
            resolve_expression(left, map)?;
            resolve_expression(right, map)
        }
        AstExpression::IncDec { op: _, exp } => {
            resolve_expression(exp, map)?;
            if exp.lvalue() {
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
        AstExpression::Const(_) => Ok(()),
        AstExpression::Unary(inner) => resolve_expression(&mut inner.exp, map),
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
        AstExpression::Cast { target: _, exp } => resolve_expression(exp, map),
    }
}

fn new_var(name: &[u8]) -> Identifier {
    let name = unsafe { std::str::from_utf8_unchecked(name) };
    Identifier::from(format!(
        "t{name}.{number}",
        number = COUNTER.fetch_add(1, Ordering::SeqCst)
    ))
}

#[derive(Debug)]
pub enum Error {
    DuplicateDeclaration,
    InvalidLval,
    UndeclaredVar,
    UndeclaredFn,
    LocalFnDecBody,
    ConflictingDec,
    StaticBlockScopeFn,
}
