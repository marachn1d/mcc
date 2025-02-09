use crate::lex::Identifier;
use crate::parse;
use crate::parse::Block;
use crate::parse::Label;
use crate::parse::Statement;
use std::rc::Rc;

use crate::parse::BlockItem;

// we need a label for each case

use std::sync::atomic::{AtomicUsize, Ordering};
static SWITCH_COUNT: AtomicUsize = AtomicUsize::new(0);

pub fn switch_cases(program: parse::Program) -> Result<super::Program, ()> {
    let mut body = Vec::new();
    for item in &mut program.0.body.0 {
        if let parse::BlockItem::S(s) = item {
            body.push(traverse_and_label(s, false)?);
        }
    }
    let function: super::Function = super::Function {
        name: b"main".into(),
        body: body.into(),
    };
    Ok(())
}

fn traverse_and_label(stmt: &mut Statement, in_switch: bool) -> Result<(), ()> {
    match stmt {
        Statement::Labeled {
            label: Label::Case { .. } | Label::Default { switch_label: _ },
            ..
        } => {
            // Error, we shouldn't find case or default outside of switch statement
            if in_switch {
                Ok(())
            } else {
                Err(())
            }
        }
        Statement::Compound(block) => {
            for item in &mut block.0 {
                if let parse::BlockItem::S(s) = item {
                    traverse_and_label(s, in_switch)?
                }
            }
            Ok(())
        }
        Statement::Switch {
            condition: _,
            body,
            cases,
            default,
        } => {
            // label switch traverses the statement, labeling each "case" it can find, and then
            *cases = label_switch(body, default)?;
            // then label any nested switch statements
            traverse_and_label(body, true)
        }
        Statement::Ret(_)
        | Statement::Exp(_)
        | Statement::Break
        | Statement::Continue
        | Statement::Null
        | Statement::Goto(_) => Ok(()),
        Statement::If { then, r#else, .. } => {
            traverse_and_label(then, in_switch)?;
            if let Some(r#else) = r#else {
                traverse_and_label(r#else, in_switch)
            } else {
                Ok(())
            }
        }
        Statement::While { body, .. }
        | Statement::DoWhile { body, .. }
        | Statement::Labeled {
            statement: body, ..
        }
        | Statement::For { body, .. } => traverse_and_label(body, in_switch),
    }
}

fn label_switch<'a>(
    body: &'a mut Statement,
    default: &mut Option<Rc<Identifier>>,
) -> Result<Box<[(u64, String)]>, ()> {
    SWITCH_COUNT.fetch_add(1, Ordering::Acquire);
    let mut vec = Vec::new();
    label_statement(body, &mut vec, default)?;
    Ok(vec.into())
}

pub fn label_statement<'a>(
    body: &'a mut Statement,
    cases: &mut Vec<(u64, String)>,
    default: &mut Option<Rc<Identifier>>,
) -> Result<(), ()> {
    match body {
        Statement::Labeled {
            label: Label::Case { case, switch_label },
            ..
        } => {
            /*
             *
             * Do something to do this like how we resolve break
            *switch_label = format!("{switch_statement_label}c{}", cases.len());
            */
            cases.push((case.0, switch_label.clone()));
            Ok(())
        }
        Statement::Labeled {
            label: Label::Default { switch_label },
            ..
        } => {
            if default.is_some(){
                // multiple defaults
                Err(())
            }else{
                //*switch_label = format!("{switch_statement_label}dflt");
                Ok(())
            }
        }
        Statement::Compound(block) => label_block(block, cases, default),
        Statement::Ret(_)
        | Statement::Exp(_)
        | Statement::Break
        | Statement::Continue // assume continues are already handled by previous semantic check
                                 // (label_loops)
        | Statement::Null
        | Statement::Switch { .. }
        | Statement::Goto(_) => Ok(()),
        Statement::If { then, r#else, .. } => {
            label_statement(then, cases, default)?;
            if let Some(r#else) = r#else {
                label_statement(r#else, cases, default)
            } else {
                Ok(())
            }
        }
        Statement::While { body, .. }
        | Statement::DoWhile { body, .. }
        | Statement::Labeled {
            statement: body, ..
        }
        | Statement::For { body, .. } => label_statement(body, cases, default),
    }
}

fn label_block<'a>(
    Block(items): &'a mut Block,
    cases: &mut Vec<(u64, String)>,
    default: &mut Option<Rc<Identifier>>,
) -> Result<(), ()> {
    for item in items {
        if let BlockItem::S(statement) = item {
            label_statement(statement, cases, default)?
        }
    }
    Ok(())
}

pub enum Error {}
