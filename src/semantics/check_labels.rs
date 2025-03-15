use crate::lex::Identifier;
use crate::semantics::BlockItem;

use crate::semantics::Declaration;
use crate::semantics::Label;
use crate::semantics::Program;
use crate::semantics::Statement;
use crate::semantics::SymbolTable;
use std::collections::HashSet;

pub fn check<T>(program: &Program<T>, vars: &SymbolTable) -> Result<(), Error> {
    for r#fn in &program.0 {
        check_dec(r#fn, vars)?
    }
    Ok(())
}

fn check_dec<T>(dec: &Declaration<T>, vars: &SymbolTable) -> Result<(), Error> {
    match dec {
        Declaration::Function {
            name,
            body: Some(body),
            ..
        } => check_body(body, vars, name),

        Declaration::Var { .. } | Declaration::Function { .. } => Ok(()),
    }
}

fn check_body<T>(
    block: &[BlockItem<T>],
    vars: &SymbolTable,
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

fn name_clashes(label: &Identifier, vars: &SymbolTable, fn_name: &Identifier) -> bool {
    use super::Attr;
    if label == fn_name {
        false
    } else {
        !matches!(vars.get(label), None | Some(Attr::Static { .. }))
    }
}

fn handle_label<T>(
    label: &Label,
    body: &Statement<T>,
    vars: &SymbolTable,
    labels: &mut HashSet<Identifier>,
    fn_name: &Identifier,
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

fn check_labels<T>(
    statement: &Statement<T>,
    vars: &SymbolTable,
    labels: &mut HashSet<Identifier>,
    fn_name: &Identifier,
) -> Result<(), Error> {
    match statement {
        Statement::Compound(block) => {
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

        Statement::Label { name, body } => handle_label(name, body, vars, labels, fn_name),
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
        Statement::Switch { body, .. } => check_labels(body, vars, labels, fn_name),
        Statement::Ret(_)
        | Statement::Exp(_)
        | Statement::Null
        | Statement::Goto(_)
        | Statement::Continue(_)
        | Statement::Break(_) => Ok(()),
    }
}

fn check_gotos<T>(statement: &Statement<T>, labels: &HashSet<Identifier>) -> Result<(), Error> {
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
        Statement::Compound(b) => {
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
        | Statement::Break(_)
        | Statement::Continue(_) => Ok(()),
    }
}

#[derive(Debug)]
pub enum Error {
    RedefinedLabel,
    ClashedLabel,
    UndefinedLabel,
}
