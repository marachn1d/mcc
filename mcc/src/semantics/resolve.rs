use ast::parse::{
    Binary, Block, BlockItem, Case, Dec, Expr, FnDec, ForInit, ParamList, Program, Stmnt,
    StorageClass, SwitchCase, VarDec,
};
use ast::Ident;

use util::{Var, VarMap};

pub fn resolve(Program(decs): &mut Program) -> Result<(), Error> {
    let mut map: VarMap = VarMap::new();
    for dec in decs {
        resolve_top_level_dec(dec, &mut map)?;
    }
    Ok(())
}

fn insert_fndec(
    map: &mut VarMap,
    name: &mut Ident,
    storage_class: &mut Option<StorageClass>,
) -> Result<(), Error> {
    map.insert(name.clone(), Var::new_fn(name, storage_class));
    Ok(())

    /*
    if map.get(name).is_some_and(|var| {
        (Var::new_fn(name, storage_class) != *var)
            && (var.from_current_block && !var.has_external_linkage)
    }) {
        Err(Error::DuplicateDec)
    } else {
        map.insert(name.clone(), Var::new_fn(name, storage_class));
        Ok(())
    }
        */
}

fn insert_local_var(
    map: &mut VarMap,
    name: &mut Ident,
    storage_class: &mut Option<StorageClass>,
) -> Result<(), Error> {
    if let Some(prev_decl) = map.get(name)
        && prev_decl.from_current_block
        && !(prev_decl.has_external_linkage && *storage_class == Some(StorageClass::Extern))
    {
        return Err(Error::ConflictingDec);
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
        let unique: Ident = map.new_var(name);
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
    name: &mut Ident,
    body: &mut Option<Block>,
    params: &mut ParamList,
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

fn resolve_param(params: &mut ParamList, map: &mut VarMap) -> Result<(), Error> {
    for param in params.iter_mut().map(|x| &mut x.name) {
        if map
            .get(param)
            .is_some_and(|param| !param.has_external_linkage && param.from_current_block)
        {
            return Err(Error::DuplicateDec);
        }

        let unique: Ident = map.new_var(&param);
        map.insert(param.clone(), Var::new_var(&unique, &None));

        *param = unique;
    }
    Ok(())
}

fn resolve_block(block: &mut Block, map: &mut VarMap) -> Result<(), Error> {
    for item in block {
        resolve_block_item(item, map)?;
    }
    Ok(())
}

fn resolve_block_item(block: &mut BlockItem, map: &mut VarMap) -> Result<(), Error> {
    match block {
        BlockItem::S(statement) => resolve_statement(statement, map)?,
        BlockItem::D(statement) => resolve_declaration(statement, map)?,
    };
    Ok(())
}

fn resolve_var_dec(
    name: &mut Ident,
    init: &mut Option<Expr>,
    sc: &mut Option<StorageClass>,
    map: &mut VarMap,
) -> Result<(), Error> {
    insert_local_var(map, name, sc)?;
    if let Some(init) = init {
        resolve_expression(init, map)?;
    }
    Ok(())
}

fn resolve_declaration(dec: &mut Dec, map: &mut VarMap) -> Result<(), Error> {
    match dec {
        Dec::Var(VarDec {
            name,
            init,
            sc,
            typ: _,
        }) => resolve_var_dec(name, init, sc, map),

        Dec::Fn(FnDec { body: Some(_), .. }) => Err(Error::LocalFnDecBody),
        Dec::Fn(FnDec { sc, .. }) if *sc == Some(StorageClass::Static) => {
            Err(Error::StaticBlockScopeFn)
        }

        Dec::Fn(FnDec {
            body: None,
            name,
            params,
            sc,
            typ: _,
        }) => resolve_fn_dec(name, &mut None, params, sc, map),
    }
}

fn resolve_statement(statement: &mut Stmnt, map: &mut VarMap) -> Result<(), Error> {
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
        Stmnt::Label { body, .. } => resolve_statement(body, map),
        Stmnt::Compound(block) => {
            let mut new_scope = new_scope(map);
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

        Stmnt::Switch { val, cases } => {
            resolve_expression(val, map)?;
            for SwitchCase { body, case } in cases {
                if let Some(Case::Case(c)) = case {
                    resolve_expression(c, map)?;
                }
                resolve_statement(body, map)?;
            }
            Ok(())
        }

        Stmnt::Goto(_) | Stmnt::Break | Stmnt::Continue => Ok(()),
    }
}

fn new_scope(map: &VarMap) -> VarMap {
    let mut new_scope = map.clone();
    for var in new_scope.values_mut() {
        var.from_current_block = false;
    }
    new_scope
}

fn resolve_init(init: &mut Option<ForInit>, map: &mut VarMap) -> Result<(), Error> {
    match init {
        None => Ok(()),
        Some(ForInit::D(dec)) => resolve_var_dec(&mut dec.name, &mut dec.init, &mut dec.sc, map),
        Some(ForInit::E(exp)) => resolve_expression(exp, map),
    }
}

fn resolve_expression(exp: &mut Expr, map: &mut VarMap) -> Result<(), Error> {
    match exp {
        Expr::Assignment { dst, src } => {
            if dst.as_lvalue().is_some() {
                resolve_expression(dst, map)?;
                resolve_expression(src, map)
            } else {
                Err(Error::InvalidLval)
            }
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
            if exp.as_lvalue().is_some() {
                Ok(())
            } else {
                Err(Error::InvalidLval)
            }
        }

        Expr::Nested(inner) => resolve_expression(inner, map),
        Expr::Var(var) => match map.get(var) {
            Some(new_name) => {
                *var = new_name.name.clone();
                Ok(())
            }
            None => Err(Error::UndeclaredVar),
        },
        Expr::Const(_) => Ok(()),
        Expr::Unary(inner) => resolve_expression(&mut inner.exp, map),
        Expr::FunctionCall { name, args } => {
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
        Expr::Cast { target: _, exp } => resolve_expression(exp, map),
    }
}

#[derive(Debug)]
pub enum Error {
    DuplicateDec,
    InvalidLval,
    UndeclaredVar,
    UndeclaredFn,
    LocalFnDecBody,
    ConflictingDec,
    StaticBlockScopeFn,
}
