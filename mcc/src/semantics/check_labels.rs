use ast::semantics::typed::{BlockItem, Dec, FnDec, Program, Stmnt};
use ast::semantics::Label;
use ast::Ident;

//use super::ast::type_prelude::*;
//

use ast::semantics::SymbolTable;
use std::collections::HashSet;

pub fn check(program: &Program, vars: &SymbolTable) -> Result<(), Error> {
    for r#fn in program {
        check_dec(r#fn, vars)?
    }
    Ok(())
}

fn check_dec(dec: &Dec, vars: &SymbolTable) -> Result<(), Error> {
    if let Dec::Fn(FnDec {
        name,
        body: Some(body),
        ..
    }) = dec
    {
        check_body(body, vars, name)
    } else {
        Ok(())
    }
}

fn check_body(block: &[BlockItem], vars: &SymbolTable, fn_name: &Ident) -> Result<(), Error> {
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

fn name_clashes(label: &Ident, vars: &SymbolTable, fn_name: &Ident) -> bool {
    use super::Attr;
    if label == fn_name {
        false
    } else {
        !matches!(vars.get(label), None | Some(Attr::Static { .. }))
    }
}

fn handle_label(
    label: &Label,
    body: &Stmnt,
    vars: &SymbolTable,
    labels: &mut HashSet<Ident>,
    fn_name: &Ident,
) -> Result<(), Error> {
    // we pass along everything else
    let Label::Named(label) = label else {
        return check_labels(body, vars, labels, fn_name);
    };
    // if the label is already defined but it's not main bc main can be a label

    if name_clashes(label, vars, fn_name) {
        Err(Error::ClashedLabel)
    } else if labels.insert(label.clone()) {
        check_labels(body, vars, labels, fn_name)
    } else {
        Err(Error::RedefinedLabel)
    }
}

fn check_labels(
    statement: &Stmnt,
    vars: &SymbolTable,
    labels: &mut HashSet<Ident>,
    fn_name: &Ident,
) -> Result<(), Error> {
    match statement {
        Stmnt::Compound(block) => {
            for item in block {
                if let BlockItem::S(s) = item {
                    check_labels(s, vars, labels, fn_name)?;
                }
            }
            Ok(())
        }
        Stmnt::While { body, .. } | Stmnt::DoWhile { body, .. } | Stmnt::For { body, .. } => {
            check_labels(body, vars, labels, fn_name)
        }

        Stmnt::Label { name, body } => handle_label(name, body, vars, labels, fn_name),
        Stmnt::If {
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
        Stmnt::Switch { body, .. } => check_labels(body, vars, labels, fn_name),
        Stmnt::Ret(_)
        | Stmnt::Exp(_)
        | Stmnt::Null
        | Stmnt::Goto(_)
        | Stmnt::Continue(_)
        | Stmnt::Break(_) => Ok(()),
    }
}

fn check_gotos(statement: &Stmnt, labels: &HashSet<Ident>) -> Result<(), Error> {
    match statement {
        Stmnt::Goto(goto) => {
            if labels.contains(goto) {
                Ok(())
            } else {
                Err(Error::UndefinedLabel)
            }
        }
        Stmnt::If {
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
        Stmnt::Compound(b) => {
            for item in b {
                if let BlockItem::S(statement) = item {
                    check_gotos(statement, labels)?;
                }
            }
            Ok(())
        }
        Stmnt::While { body, .. }
        | Stmnt::DoWhile { body, .. }
        | Stmnt::Label { body, .. }
        | Stmnt::Switch { body, .. }
        | Stmnt::For { body, .. } => check_gotos(body, labels),

        Stmnt::Ret(_) | Stmnt::Exp(_) | Stmnt::Null | Stmnt::Break(_) | Stmnt::Continue(_) => {
            Ok(())
        }
    }
}

#[derive(Debug)]
pub enum Error {
    RedefinedLabel,
    ClashedLabel,
    UndefinedLabel,
}
