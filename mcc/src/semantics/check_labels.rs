use ast::typed::*;
use ast::Key;

use crate::semantics::SymbolTable;
use std::collections::HashSet;

pub fn check(program: &Program, vars: &SymbolTable) -> Result<(), Error> {
    for f in program {
        if let Dec::Fn(FnDec {
            name,
            body: Some(body),
            ..
        }) = f
        {
            check_body(body, vars, name)?
        }
    }
    Ok(())
}

fn check_body(block: &[BlockItem], vars: &SymbolTable, fn_name: &Key) -> Result<(), Error> {
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

fn check_labels<'a>(
    statement: &Stmnt<'a>,
    vars: &SymbolTable<'a>,
    labels: &mut HashSet<Key<'a>>,
    fn_name: &Key<'a>,
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

        Stmnt::NamedLabel { l, body } => {
            if name_clashes(l, vars, fn_name) {
                Err(Error::ClashedLabel)
            } else if labels.insert(*l) {
                check_labels(body, vars, labels, fn_name)
            } else {
                Err(Error::RedefinedLabel)
            }
        }
        Stmnt::Default(stmnt) | Stmnt::Case { stmnt, .. } => {
            check_labels(&stmnt.body, vars, labels, fn_name)
        }

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

fn name_clashes(label: &Key, vars: &SymbolTable, fn_name: &Key) -> bool {
    use super::Attr;
    if label == fn_name {
        false
    } else {
        !matches!(vars.get(label), None | Some(Attr::Static { .. }))
    }
}

fn check_gotos(statement: &Stmnt, labels: &HashSet<Key>) -> Result<(), Error> {
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
        Stmnt::NamedLabel { body, .. }
        | Stmnt::While { body, .. }
        | Stmnt::DoWhile { body, .. }
        | Stmnt::Switch { body, .. }
        | Stmnt::For { body, .. } => check_gotos(body, labels),

        Stmnt::Case { stmnt, .. } | Stmnt::Default(stmnt) => check_gotos(&stmnt.body, labels),

        Stmnt::Ret(_) | Stmnt::Exp(_) | Stmnt::Null | Stmnt::Break(_) | Stmnt::Continue(_) => {
            Ok(())
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("label already defined")]
    RedefinedLabel,
    #[error("labels clash")]
    ClashedLabel,
    #[error("undefined label")]
    UndefinedLabel,
}
