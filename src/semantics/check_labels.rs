use crate::lex::Identifier;
use crate::parse::Block;
use crate::parse::BlockItem;
use crate::parse::FunctionDeclaration;

use crate::parse::Declaration;
use crate::parse::Label;
use crate::parse::Program;
use crate::parse::Statement;
use std::collections::HashSet;

pub fn check(program: &mut Program, vars: &HashSet<Identifier>) -> Result<(), Error> {
    for r#fn in &mut program.0 {
        check_dec(r#fn, vars)?
    }
    Ok(())
}

fn check_dec(dec: &mut Declaration, vars: &HashSet<Identifier>) -> Result<(), Error> {
    match dec {
        Declaration::Function {
            name,
            body: Some(body),
            ..
        } => check_body(&body.0, vars, name),

        Declaration::Var { .. } | Declaration::Function { .. } => Ok(()),
    }
}

fn check_function(
    FunctionDeclaration { body, name, .. }: &mut FunctionDeclaration,
    vars: &HashSet<Identifier>,
) -> Result<(), Error> {
    if let Some(body) = body {
        check_body(&body.0, vars, name)?;
    }
    Ok(())
}

fn check_body(
    block: &[BlockItem],
    vars: &HashSet<Identifier>,
    fn_name: &Identifier,
) -> Result<(), Error> {
    let mut labels = HashSet::new();
    for item in block.iter() {
        if let BlockItem::S(statement) = item {
            check_labels(statement, vars, &mut labels, fn_name)?;
        }
    }

    for item in block {
        if let BlockItem::S(statement) = item {
            check_gotos(statement, &labels)?;
        }
    }

    Ok(())
}

fn handle_label(
    label: &Label,
    body: &Statement,
    vars: &HashSet<Identifier>,
    labels: &mut HashSet<Identifier>,
    fn_name: &Identifier,
) -> Result<(), Error> {
    // we pass along everything else
    let Label::Named(label) = label else {
        return check_labels(body, vars, labels, fn_name);
    };
    // if the label is already defined but it's not main bc main can be a label
    if vars.contains(label) && label != fn_name {
        Err(Error::ClashedLabel)
    } else if labels.insert(label.clone()) {
        check_labels(body, vars, labels, fn_name)
    } else {
        Err(Error::RedefinedLabel)
    }
}

fn check_labels(
    statement: &Statement,
    vars: &HashSet<Identifier>,
    labels: &mut HashSet<Identifier>,
    fn_name: &Identifier,
) -> Result<(), Error> {
    match statement {
        Statement::Compound(Block(block)) => {
            for item in block {
                if let BlockItem::S(s) = item {
                    check_labels(s, vars, labels, fn_name)?;
                }
            }
            Ok(())
        }
        Statement::While { body, .. }
        | Statement::DoWhile { body, .. }
        | Statement::For { body, .. } => check_labels(body, vars, labels, fn_name),

        Statement::Label { label, body } => handle_label(label, body, vars, labels, fn_name),
        Statement::If {
            condition: _,
            then,
            r#else,
        } => {
            check_labels(then, vars, labels, fn_name)?;
            if let Some(r#else) = r#else {
                check_labels(r#else, vars, labels, fn_name)?;
            };
            Ok(())
        }
        Statement::Switch { val: _, body } => check_labels(body, vars, labels, fn_name),
        Statement::Ret(_)
        | Statement::Exp(_)
        | Statement::Null
        | Statement::Goto(_)
        | Statement::Continue
        | Statement::Break => Ok(()),
    }
}

fn check_gotos(statement: &Statement, labels: &HashSet<Identifier>) -> Result<(), Error> {
    match statement {
        Statement::Goto(goto) => {
            if labels.contains(goto) {
                Ok(())
            } else {
                Err(Error::UndefinedLabel)
            }
        }
        Statement::If {
            condition: _,
            then,
            r#else,
        } => {
            check_gotos(then, labels)?;
            if let Some(r#else) = r#else {
                check_gotos(r#else, labels)?;
            };
            Ok(())
        }
        Statement::Compound(Block(b)) => {
            for item in b {
                if let BlockItem::S(statement) = item {
                    check_gotos(statement, labels)?;
                }
            }
            Ok(())
        }
        Statement::While { body, .. }
        | Statement::DoWhile { body, .. }
        | Statement::Label { body, .. }
        | Statement::Switch { body, .. }
        | Statement::For { body, .. } => check_gotos(body, labels),

        Statement::Ret(_)
        | Statement::Exp(_)
        | Statement::Null
        | Statement::Break
        | Statement::Continue => Ok(()),
    }
}

#[derive(Debug)]
pub enum Error {
    RedefinedLabel,
    ClashedLabel,
    UndefinedLabel,
}
