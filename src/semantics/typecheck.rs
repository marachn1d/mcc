use super::Identifier;
use super::{
    BlockItem, Declaration, Expression, ForInit, ParamList, Program, Statement, StorageClass,
    VariableDeclaration,
};
use crate::parse::{Binary, Factor, Unary};
use std::collections::HashMap;

use std::collections::hash_map::Entry;

pub type SymbolTable = HashMap<Identifier, Attr>;

pub enum Attr {
    StaticInt {
        init: Option<InitialVal>,
        global: bool,
    },
    Automatic,
    Fn {
        len: usize,
        defined: bool,
        global: bool,
    },
}

pub enum InitialVal {
    Tentative,
    Initial(u64),
}

impl InitialVal {
    pub fn as_num(&self) -> u64 {
        match self {
            Self::Initial(i) => *i,
            Self::Tentative => 0,
        }
    }
}

// check FunctionDeclaration, VariableDeclaration,

pub fn typecheck(p: &mut Program) -> Result<SymbolTable, Error> {
    let mut table = SymbolTable::new();
    for dec in &mut p.0 {
        top_level_declaration(dec, &mut table)?;
    }

    Ok(table)
}

fn top_level_declaration(dec: &mut Declaration, table: &mut SymbolTable) -> Result<(), Error> {
    match dec {
        Declaration::Function {
            name,
            params,
            body,
            storage_class,
        } => function_declaration(name, params, body, storage_class, table),
        Declaration::Var {
            name,
            init,
            storage_class,
        } => top_level_var(name, init, storage_class, table),
    }
}

fn top_level_var(
    name: &mut Identifier,
    init: &mut Option<Expression>,
    storage_class: &mut Option<StorageClass>,
    table: &mut SymbolTable,
) -> Result<(), Error> {
    let mut initial_val = tlv_initial(init, storage_class)?;

    // if we're not static, then we're global, if we are static, then we're not global?
    let mut global = *storage_class != Some(StorageClass::Static);

    let table_entry = table.get(name);

    if let Some(Attr::StaticInt {
        init: old_init,
        global: old_global,
    }) = table_entry
    {
        check_linkage(&mut global, *old_global, storage_class)?;
        check_initializer(old_init, &mut initial_val)?;
    } else if table_entry.is_some() {
        return Err(Error::ConflictingType);
    };

    table.insert(
        name.clone(),
        Attr::StaticInt {
            init: initial_val,
            global,
        },
    );

    Ok(())
}

fn check_linkage(
    global: &mut bool,
    old_global: bool,
    sc: &Option<StorageClass>,
) -> Result<(), Error> {
    // if it's extern, then we go with it, otherwise they should be the same
    if *sc == Some(StorageClass::Extern) {
        *global = old_global;
        Ok(())
    } else if *global != old_global {
        Err(Error::ConflictingLinkage)
    } else {
        Ok(())
    }
}

fn check_initializer(old: &Option<InitialVal>, new: &mut Option<InitialVal>) -> Result<(), Error> {
    match (&new, old) {
        // if they're both declared something's wrong, even if it's the same definition
        (Some(InitialVal::Initial(_)), Some(InitialVal::Initial(_))) => {
            Err(Error::ConflictingDeclaration)
        }
        // if we have an initial pass then we take it
        (Some(InitialVal::Initial(c)), _) | (_, Some(InitialVal::Initial(c))) => {
            *new = Some(InitialVal::Initial(*c));
            Ok(())
        }
        // if we have a tentative we take it
        (Some(InitialVal::Tentative), _) | (_, Some(InitialVal::Tentative)) => {
            *new = Some(InitialVal::Tentative);
            Ok(())
        }
        // otherwise it's none, so we leave it as none
        (None, None) => Ok(()),
    }
}

fn tlv_initial(
    init: &Option<Expression>,
    sc: &Option<StorageClass>,
) -> Result<Option<InitialVal>, Error> {
    match (init, sc) {
        (Some(Expression::Int(c)), _) => Ok(Some(InitialVal::Initial(*c))),
        (None, Some(StorageClass::Extern)) => Ok(None),
        (None, Some(StorageClass::Static)) => Ok(Some(InitialVal::Tentative)),
        (None, None) => Ok(Some(InitialVal::Tentative)),
        (_, _) => {
            eprintln!("init {init:?} sc {sc:?}");
            Err(Error::NotConstInitialized)
        }
    }
}

fn declaration(dec: &mut Declaration, table: &mut SymbolTable) -> Result<(), Error> {
    match dec {
        Declaration::Function {
            name,
            params,
            body,
            storage_class,
        } => function_declaration(name, params, body, storage_class, table),
        Declaration::Var {
            name,
            init,
            storage_class,
        } => variable_declaration(name, init, storage_class, table),
    }
}

fn variable_declaration(
    name: &mut Identifier,
    init: &mut Option<Expression>,

    sc: &mut Option<StorageClass>,
    table: &mut SymbolTable,
) -> Result<(), Error> {
    match (sc, &init) {
        (Some(StorageClass::Extern), Some(_)) => Err(Error::DeclaredExtern),
        (Some(StorageClass::Extern), None) => match table.entry(name.clone()) {
            Entry::Occupied(e) => {
                if let Attr::Fn { .. } = e.get() {
                    Err(Error::FnAsVar)
                } else {
                    Ok(())
                }
            }
            Entry::Vacant(e) => {
                e.insert(Attr::StaticInt {
                    init: None,
                    global: true,
                });
                Ok(())
            }
        },
        (Some(StorageClass::Static), Some(Expression::Int(i))) => {
            table.insert(
                name.clone(),
                Attr::StaticInt {
                    init: Some(InitialVal::Initial(*i)),
                    global: false,
                },
            );
            Ok(())
        }
        (Some(StorageClass::Static), Some(_)) => Err(Error::NotConstInitialized),
        (Some(StorageClass::Static), None) => {
            table.insert(
                name.clone(),
                Attr::StaticInt {
                    init: Some(InitialVal::Initial(0)),
                    global: false,
                },
            );
            Ok(())
        }
        (None, _) => {
            table.insert(name.clone(), Attr::Automatic);
            if let Some(init) = init {
                typecheck_expression(init, table)?;
            }
            Ok(())
        }
    }
}

fn typecheck_expression(expression: &mut Expression, table: &mut SymbolTable) -> Result<(), Error> {
    match expression {
        Expression::FunctionCall { name, args } => typecheck_fn_call(name, args, table),
        Expression::Var(name) => typecheck_var(name, table),
        Expression::Assignment(assignment) => {
            typecheck_expression(&mut assignment.0, table)?;
            typecheck_expression(&mut assignment.1, table)
        }
        Expression::Binary(Binary { left, right, .. }) => {
            typecheck_expression(left, table)?;
            typecheck_expression(right, table)
        }
        Expression::PostfixIncrement(exp)
        | Expression::PostfixDecrement(exp)
        | Expression::PrefixIncrement(exp)
        | Expression::PrefixDecrement(exp)
        | Expression::Nested(exp) => typecheck_expression(exp, table),
        Expression::Int(_) => Ok(()),

        Expression::Unary(Unary { exp, .. }) => typecheck_factor(exp, table),

        Expression::Conditional {
            condition,
            r#true,
            r#false,
        } => {
            typecheck_expression(condition, table)?;
            typecheck_expression(r#true, table)?;
            typecheck_expression(r#false, table)
        }
    }
}

fn typecheck_fn_call(
    name: &mut Identifier,
    args: &mut [Expression],
    table: &mut SymbolTable,
) -> Result<(), Error> {
    match table.get(name) {
        Some(Attr::Fn { len, .. }) => {
            if *len == args.len() {
                for arg in args {
                    typecheck_expression(arg, table)?;
                }

                Ok(())
            } else {
                Err(Error::WrongArgs)
            }
        }
        Some(_) => Err(Error::VarAsFn),
        None => Err(Error::UndefinedFn),
    }
}

fn typecheck_var(name: &mut Identifier, table: &mut SymbolTable) -> Result<(), Error> {
    match table.get(name) {
        Some(Attr::Automatic | Attr::StaticInt { .. }) => Ok(()),
        Some(_) => Err(Error::FnAsVar),
        None => Err(Error::UndefinedVar),
    }
}

fn typecheck_factor(factor: &mut Factor, table: &mut SymbolTable) -> Result<(), Error> {
    match factor {
        Factor::Var(name) => typecheck_var(name, table),
        Factor::PostfixIncrement(exp)
        | Factor::PostfixDecrement(exp)
        | Factor::PrefixIncrement(exp)
        | Factor::PrefixDecrement(exp)
        | Factor::Nested(exp) => typecheck_expression(exp, table),
        Factor::Int(_) => Ok(()),
        Factor::Unary(Unary { exp, .. }) => typecheck_factor(exp, table),
        Factor::FunctionCall { name, args } => typecheck_fn_call(name, args, table),
    }
}

fn check_entry(
    entry: &Entry<Identifier, Attr>,
    params_len: usize,
    has_body: bool,
) -> Result<(), Error> {
    let Entry::Occupied(e) = entry else {
        return Ok(());
    };
    let Attr::Fn {
        len,
        defined,
        global: _,
    } = e.get()
    else {
        return Err(Error::ConflictingType);
    };

    if *len != params_len {
        return Err(Error::WrongParams);
    } else if *defined && has_body {
        return Err(Error::DuplicateDefinition);
    }

    Ok(())
}

fn function_declaration(
    name: &mut Identifier,
    params: &mut ParamList,
    body: &mut Option<Box<[BlockItem]>>,
    sc: &mut Option<StorageClass>,
    table: &mut SymbolTable,
) -> Result<(), Error> {
    let global = sc.is_some_and(|sc| sc != StorageClass::Static);

    let has_body = body.is_some();

    let entry = table.entry(name.clone());

    check_entry(&entry, params.len(), has_body)?;

    match entry {
        Entry::Occupied(mut e) => {
            let Attr::Fn {
                len: _,
                defined,
                global: was_global,
            } = e.get_mut()
            else {
                unreachable!()
            };
            if has_body {
                eprintln!("{name} is now defined");
                *defined = true;
            }

            if global && !(*was_global) {
                eprintln!("{name} is now global");
                *was_global = true;
            }
        }
        Entry::Vacant(e) => {
            e.insert(Attr::Fn {
                len: params.len(),
                defined: has_body,
                global,
            });
        }
    };

    for param in params.iter() {
        table.insert(param.clone(), Attr::Automatic);
    }

    if let Some(body) = body {
        for item in body {
            typecheck_blockitem(item, table)?;
        }
    }
    Ok(())
}

fn typecheck_blockitem(block_item: &mut BlockItem, table: &mut SymbolTable) -> Result<(), Error> {
    match block_item {
        BlockItem::D(dec) => declaration(dec, table),
        BlockItem::S(s) => typecheck_statement(s, table),
    }
}

fn typecheck_statement(stmt: &mut Statement, table: &mut SymbolTable) -> Result<(), Error> {
    match stmt {
        Statement::Compound(stmts) => {
            for item in stmts {
                typecheck_blockitem(item, table)?;
            }
            Ok(())
        }
        Statement::Exp(e) => typecheck_expression(e, table),
        Statement::If {
            condition,
            then,
            r#else,
        } => {
            typecheck_expression(condition, table)?;
            typecheck_statement(then, table)?;
            if let Some(r#else) = r#else {
                typecheck_statement(r#else, table)
            } else {
                Ok(())
            }
        }
        Statement::DoWhile {
            body,
            condition,
            label: _,
        } => {
            typecheck_statement(body, table)?;
            typecheck_expression(condition, table)
        }
        Statement::While {
            body,
            condition,
            label: _,
        } => {
            typecheck_expression(condition, table)?;
            typecheck_statement(body, table)
        }
        Statement::Label { body, name: _ } => typecheck_statement(body, table),
        Statement::For {
            init,
            condition,
            post,
            body,
            label: _,
        } => {
            if let Some(init) = init {
                match init {
                    ForInit::D(VariableDeclaration {
                        name,
                        init,
                        storage_class,
                    }) => variable_declaration(name, init, storage_class, table),

                    ForInit::E(e) => typecheck_expression(e, table),
                }?;
            }
            if let Some(condition) = condition {
                typecheck_expression(condition, table)?;
            };
            if let Some(post) = post {
                typecheck_expression(post, table)?;
            }
            typecheck_statement(body, table)
        }
        Statement::Ret(e) => typecheck_expression(e, table),
        Statement::Switch {
            val,
            body,
            label: _,
            cases: _,
            default: _,
        } => {
            typecheck_expression(val, table)?;
            typecheck_statement(body, table)
        }
        Statement::Null | Statement::Goto(_) | Statement::Break(_) | Statement::Continue(_) => {
            Ok(())
        }
    }
}

#[derive(Debug)]
pub enum Error {
    DuplicateDefinition,
    ConflictingDeclaration,

    UndefinedVar,

    UndefinedFn,

    WrongParams,

    FnAsVar,
    VarAsFn,
    WrongArgs,
    ConflictingType,
    StaticGlobal,

    NotConstInitialized,
    ConflictingLinkage,
    DeclaredExtern,
}
