use crate::lex::Identifier;
use crate::parse;
use parse::Program;
use std::collections::HashSet;
use std::rc::Rc;

use crate::parse::Label;

use parse::Statement;

use parse::BlockItem;

use parse::Function;

pub fn check(program: &mut Program, vars: &HashSet<Rc<Identifier>>) -> Result<(), Error> {
    check_function(&mut program.0, vars)
}

fn check_function(
    Function { name: _, body }: &mut Function,
    vars: &HashSet<Rc<Identifier>>,
) -> Result<(), Error> {
    check_body(body.0.as_ref(), vars)?;
    Ok(())
}

fn check_body(block: &[BlockItem], vars: &HashSet<Rc<Identifier>>) -> Result<(), Error> {
    let mut labels = HashSet::new();
    for item in block.iter() {
        if let BlockItem::S(statement) = item {
            check_labels(statement, vars, &mut labels)?;
        }
    }

    for item in block {
        if let BlockItem::S(statement) = item {
            check_gotos(statement, &labels)?;
        }
    }

    Ok(())
}

fn check_labels(
    statement: &Statement,
    vars: &HashSet<Rc<Identifier>>,
    labels: &mut HashSet<Rc<Identifier>>,
) -> Result<(), Error> {
    match statement {
        Statement::Compound(block) => {
            for item in &block.0 {
                if let BlockItem::S(s) = item {
                    check_labels(&s, vars, labels)?;
                }
            }
            Ok(())
        }
        Statement::While { body, .. }
        | Statement::DoWhile { body, .. }
        | Statement::For { body, .. }
        | Statement::Switch { body, .. } => check_labels(&body, vars, labels),

        Statement::Labeled {
            label: Label::Named(label),
            statement,
        } => {
            if vars.contains(label) {
                Err(Error::ClashedLabel)
            } else if labels.insert(label.clone()) {
                check_labels(statement, vars, labels)
            } else {
                Err(Error::RedefinedLabel)
            }
        }
        Statement::Labeled {
            label: _,
            statement,
        } => check_labels(statement, vars, labels),
        Statement::If {
            condition: _,
            then,
            r#else,
        } => {
            check_labels(then, vars, labels)?;
            if let Some(r#else) = r#else {
                check_labels(r#else, vars, labels)?;
            };
            Ok(())
        }
        Statement::Ret(_)
        | Statement::Exp(_)
        | Statement::Null
        | Statement::Goto(_)
        | Statement::Continue
        | Statement::Break => Ok(()),
    }
}

// this one checks switch statements for
fn check_switch() {}

fn check_gotos(statement: &Statement, labels: &HashSet<Rc<Identifier>>) -> Result<(), Error> {
    match statement {
        Statement::Goto(goto) => {
            if labels.contains(goto) {
                Ok(())
            } else {
                Err(Error::UndefinedLabel)
            }
        }

        Statement::Labeled {
            label: _,
            statement,
        } => check_gotos(statement, labels),
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
            for item in &b.0 {
                if let BlockItem::S(statement) = item {
                    check_gotos(statement, labels)?;
                }
            }
            Ok(())
        }

        Statement::While { body, .. }
        | Statement::Switch { body, .. }
        | Statement::DoWhile { body, .. }
        | Statement::For { body, .. } => check_gotos(&body, labels),

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
