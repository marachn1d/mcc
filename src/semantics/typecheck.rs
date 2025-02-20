use super::Identifier;
use super::{
    BlockItem, Declaration, Expression, ForInit, FunctionDeclaration, ParamList, Program,
    Statement, VariableDeclaration,
};
use crate::parse::{Binary, Factor, Unary};
use std::collections::HashMap;
use std::rc::Rc;

type SymbolTable = HashMap<Rc<Identifier>, Type>;

pub enum Type {
    Int,
    Fn { len: usize, defined: bool },
}

// check FunctionDeclaration, VariableDeclaration,

pub fn typecheck(p: &mut Program) -> Result<(), Error> {
    let mut table = SymbolTable::new();
    for FunctionDeclaration { name, params, body } in &mut p.0 {
        function_declaration(name, params, body, &mut table)?
    }

    Ok(())
}

fn declaration(dec: &mut Declaration, table: &mut SymbolTable) -> Result<(), Error> {
    match dec {
        Declaration::Function { name, params, body } => {
            function_declaration(name, params, body, table)
        }
        Declaration::Var { name, init } => variable_declaration(name, init, table),
    }
}

fn variable_declaration(
    name: &mut Rc<Identifier>,
    init: &mut Option<Expression>,
    table: &mut SymbolTable,
) -> Result<(), Error> {
    table.insert(name.clone(), Type::Int);
    if let Some(init) = init {
        typecheck_expression(init, table)?;
    }
    Ok(())
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

fn typecheck_var(name: &mut Rc<Identifier>, table: &mut SymbolTable) -> Result<(), Error> {
    match table.get(name) {
        Some(Type::Int) => Ok(()),
        Some(_) => Err(Error::FnAsVar),
        None => Err(Error::UndefinedVar),
    }
}

fn typecheck_fn_call(
    name: &mut Rc<Identifier>,
    args: &mut [Expression],
    table: &mut SymbolTable,
) -> Result<(), Error> {
    match table.get(name) {
        Some(Type::Fn { len, .. }) => {
            if *len == args.len() {
                for arg in args {
                    typecheck_expression(arg, table)?;
                }

                Ok(())
            } else {
                Err(Error::WrongArgs)
            }
        }
        Some(Type::Int) => Err(Error::VarAsFn),
        None => Err(Error::UndefinedFn),
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

fn function_declaration(
    name: &mut Rc<Identifier>,
    params: &mut ParamList,
    body: &mut Option<Box<[BlockItem]>>,
    table: &mut SymbolTable,
) -> Result<(), Error> {
    use std::collections::hash_map::Entry;

    let has_body = body.is_some();

    match table.entry(name.clone()) {
        Entry::Occupied(mut e) => match e.get_mut() {
            Type::Int => Err(Error::ConflictingType),
            Type::Fn { len, defined } => {
                if body.is_some() && *defined {
                    Err(Error::DuplicateDefinition)
                } else if *len != params.len() {
                    Err(Error::WrongParams)
                } else if *defined && has_body {
                    Err(Error::DuplicateDefinition)
                } else if has_body {
                    *defined = true;
                    Ok(())
                } else {
                    Ok(())
                }
            }
        },
        Entry::Vacant(e) => {
            e.insert(Type::Fn {
                len: params.len(),
                defined: has_body,
            });
            Ok(())
        }
    }?;

    for param in params.iter() {
        table.insert(param.clone(), Type::Int);
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
                    ForInit::D(VariableDeclaration { name, init }) => {
                        variable_declaration(name, init, table)
                    }

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
}
