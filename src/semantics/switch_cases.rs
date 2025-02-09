use crate::parse::Block;
use crate::parse::Label;
use crate::parse::Statement;

use crate::parse::BlockItem;

// we need a label for each case

use std::sync::atomic::{AtomicUsize, Ordering};
static SWITCH_COUNT: AtomicUsize = AtomicUsize::new(0);

struct CaseRef<'a> {
    condition: &'a u64,
    label: &'a str,
}

// current plan is to do this in the tacky pass but that sucks and catches it very late, could also
// give the ast a vec of pointers, and then follow aliasing rules

pub fn switch_cases<'a>(
    body: &'a mut Statement,
    switch_statement_label: &str,
) -> Result<Box<[CaseRef<'a>]>, ()> {
    SWITCH_COUNT.fetch_add(1, Ordering::Acquire);
    let mut vec = Vec::new();
    label_statement(body, &mut vec, switch_statement_label)?;
    Ok(vec.into())
}

pub fn label_statement<'a>(
    body: &'a mut Statement,
    cases: &mut Vec<CaseRef<'a>>,
    switch_statement_label: &str,
) -> Result<(), ()> {
    match body {
        Statement::Labeled {
            label: Label::Case { case, switch_label },
            ..
        } => {
            *switch_label = format!("{switch_statement_label}c{}", cases.len());
            cases.push(CaseRef {
                condition: &case.0,
                label: switch_label,
            });
            Ok(())
        }
        Statement::Compound(block) => label_block(block, cases, switch_statement_label),
        Statement::Ret(_)
        | Statement::Exp(_)
        | Statement::Break(_)
        | Statement::Continue(_)
        | Statement::Null
        | Statement::Switch { .. }
        | Statement::Goto(_) => Ok(()),
        Statement::If { then, r#else, .. } => {
            label_statement(then, cases, switch_statement_label)?;
            if let Some(r#else) = r#else {
                label_statement(r#else, cases, switch_statement_label)
            } else {
                Ok(())
            }
        }
        Statement::While { body, .. }
        | Statement::DoWhile { body, .. }
        | Statement::Labeled {
            statement: body, ..
        }
        | Statement::For { body, .. } => label_statement(body, cases, switch_statement_label),
    }
}

fn label_block<'a>(
    Block(items): &'a mut Block,
    cases: &mut Vec<CaseRef<'a>>,
    switch_statement_label: &str,
) -> Result<(), ()> {
    for item in items {
        if let BlockItem::S(statement) = item {
            label_statement(statement, cases, switch_statement_label)?
        }
    }
    Ok(())
}
