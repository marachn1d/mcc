use ast::parse::prelude::*;
use symtab::Store;
mod var_map;
use var_map::VarMap;

pub fn resolve<'a>(Program(decs): &mut Program<'a>, s: &'a Store) -> Result<(), Error> {
    let mut map: VarMap = VarMap::new(s);
    for dec in decs {
        resolve_top_level_dec(dec, &mut map)?;
    }
    Ok(())
}

fn resolve_top_level_dec<'a>(dec: &mut Dec<'a>, map: &mut VarMap<'a>) -> Result<(), Error> {
    match dec {
        Dec::Var(v) => map.add_var(v),

        Dec::Fn(f) => {
            map.add_fn(f)?;
            let mut inner = VarMap::new_scope(map);
            resolve_param(&mut f.params, &mut inner)?;
            if let Some(b) = &mut f.body {
                resolve_block(b, &mut inner)?;
            }
            Ok(())
        }
    }
}

fn resolve_param<'a>(params: &mut ParamList<'a>, map: &mut VarMap<'a>) -> Result<(), Error> {
    for Param { name, .. } in params.iter_mut() {
        if let Some(v) = map.map.get(name) {
            if !v.has_external_linkage && v.from_current_block {
                return Err(Error::DuplicateDeclaration);
            }
        };
        let unique = map.param_name(name);
        *name = unique.name;
        map.map.insert(*name, unique);
    }
    Ok(())
}

fn resolve_block<'a>(block: &mut [BlockItem<'a>], map: &mut VarMap<'a>) -> Result<(), Error> {
    for item in block {
        match item {
            BlockItem::S(s) => resolve_statement(s, map)?,
            BlockItem::D(d) => resolve_declaration(d, map)?,
        }
    }
    Ok(())
}

fn resolve_declaration<'a>(dec: &mut Dec<'a>, map: &mut VarMap<'a>) -> Result<(), Error> {
    match dec {
        Dec::Var(d) => resolve_var_dec(d, map),

        Dec::Fn(FnDec { body: Some(_), .. }) => Err(Error::LocalFnDecBody),
        Dec::Fn(FnDec { sc, .. }) if *sc == Some(StorageClass::Static) => {
            Err(Error::StaticBlockScopeFn)
        }

        Dec::Fn(f) => resolve_fn_dec(f, map),
    }
}

fn resolve_var_dec<'a>(dec: &mut VarDec<'a>, map: &mut VarMap<'a>) -> Result<(), Error> {
    map.add_var(dec)?;
    if let Some(init) = &mut dec.init {
        resolve_expression(init, map)?;
    }
    Ok(())
}

fn resolve_fn_dec<'a>(dec: &mut FnDec<'a>, map: &mut VarMap<'a>) -> Result<(), Error> {
    map.add_fn(dec)?;
    let mut inner_map = VarMap::new_scope(map);
    resolve_param(&mut dec.params, &mut inner_map)?;
    if let Some(body) = &mut dec.body {
        resolve_block(body, &mut inner_map)?;
    }
    Ok(())
}

fn resolve_statement<'a>(statement: &mut Stmnt<'a>, map: &mut VarMap<'a>) -> Result<(), Error> {
    match statement {
        Stmnt::Ret(exp) => resolve_expression(exp, map),
        Stmnt::Null => Ok(()),
        Stmnt::Exp(exp) => resolve_expression(exp, map),
        Stmnt::If {
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
        Stmnt::Case { body, .. } | Stmnt::Default(body) | Stmnt::NamedLabel { body, .. } => {
            resolve_statement(body, map)
        }
        Stmnt::Compound(block) => {
            let mut new_scope = VarMap::new_scope(map);
            resolve_block(block, &mut new_scope)
        }

        Stmnt::While {
            condition, body, ..
        } => {
            resolve_expression(condition, map)?;
            resolve_statement(body, map)
        }
        Stmnt::DoWhile {
            condition, body, ..
        } => {
            resolve_statement(body, map)?;
            resolve_expression(condition, map)
        }
        Stmnt::For {
            init,
            post,
            body,
            condition,
            ..
        } => {
            let mut new_map = VarMap::new_scope(map);
            resolve_init(init, &mut new_map)?;
            if let Some(condition) = condition {
                resolve_expression(condition, &mut new_map)?;
            }
            if let Some(post) = post {
                resolve_expression(post, &mut new_map)?;
            }
            resolve_statement(body, &mut new_map)
        }

        Stmnt::Switch { val, body } => {
            resolve_expression(val, map)?;
            resolve_statement(body, map)
        }

        Stmnt::Goto(_) | Stmnt::Break | Stmnt::Continue => Ok(()),
    }
}

fn resolve_init<'a>(init: &mut Option<ForInit<'a>>, map: &mut VarMap<'a>) -> Result<(), Error> {
    match init {
        None => Ok(()),
        Some(ForInit::D(dec)) => resolve_var_dec(dec, map),
        Some(ForInit::E(exp)) => resolve_expression(exp, map),
    }
}

fn resolve_expression<'a>(exp: &mut Expr<'a>, map: &mut VarMap<'a>) -> Result<(), Error> {
    match exp {
        Expr::Assignment { dst, src } => {
            if dst.lvalue() {
                resolve_expression(dst, map)?;
                resolve_expression(src, map)
            } else {
                Err(Error::InvalidLval)
            }
        }
        Expr::Bin(Binary {
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

        Expr::Conditional {
            condition,
            r#true,
            r#false,
        } => {
            resolve_expression(condition, map)?;
            resolve_expression(r#true, map)?;
            resolve_expression(r#false, map)
        }

        Expr::Bin(Binary { left, right, .. }) => {
            resolve_expression(left, map)?;
            resolve_expression(right, map)
        }
        Expr::IncDec { op: _, exp } => {
            resolve_expression(exp, map)?;
            if exp.lvalue() {
                Ok(())
            } else {
                Err(Error::InvalidLval)
            }
        }

        Expr::Nested(inner) => resolve_expression(inner, map),
        Expr::Var(var) => match map.map.get(var) {
            Some(new_name) => {
                *var = new_name.name;
                Ok(())
            }
            None => Err(Error::UndeclaredVar),
        },
        Expr::Const(_) => Ok(()),
        Expr::Unary(inner) => resolve_expression(&mut inner.exp, map),
        Expr::FunctionCall { name, args } => {
            if let Some(new_name) = map.map.get(name) {
                *name = new_name.name;
                for arg in args {
                    resolve_expression(arg, map)?;
                }
                Ok(())
            } else {
                Err(Error::UndeclaredFn)
            }
        }
        Expr::Cast { target: _, exp } => resolve_expression(exp, map),
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Duplicate Declaration")]
    DuplicateDeclaration,

    #[error("Invalid LVal")]
    InvalidLval,
    #[error("UndeclaredVar")]
    UndeclaredVar,
    #[error("UndeclaredFn")]
    UndeclaredFn,
    #[error("LocalFnDecBody")]
    LocalFnDecBody,
    #[error("Conflicting Declaration")]
    ConflictingDec,

    #[error("Static Function at Block Scope (not allowed)")]
    StaticBlockScopeFn,
}
