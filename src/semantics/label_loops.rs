use std::sync::atomic::{AtomicUsize, Ordering};
static LOOPS: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug)]
pub enum Error {
    Break,
    Continue,
    ContinueInSwitch,
}

use crate::parse;
pub fn label(parse::Program(main): &mut parse::Program) -> Result<(), Error> {
    let blocks = &mut main.body.0;
    label_blocks(blocks, None)
}

fn label_blocks(block: &mut [parse::BlockItem], cur_loop: Option<&BodyName>) -> Result<(), Error> {
    for item in block {
        if let parse::BlockItem::S(s) = item {
            label_statement(s, cur_loop)?;
        }
    }
    Ok(())
}

#[derive(Debug, Clone)]
enum BodyName {
    Loop(usize),
    Switch(String),
}

impl Default for BodyName {
    fn default() -> Self {
        Self::Loop(0)
    }
}

fn label_statement(
    statement: &mut parse::Statement,
    cur_loop: Option<&BodyName>,
) -> Result<(), Error> {
    use parse::Statement as Stmt;
    match statement {
        Stmt::DoWhile { label, body, .. }
        | Stmt::For { label, body, .. }
        | Stmt::While { label, body, .. } => {
            // increment the loop nubmer to get a unique id         fetch add returns previous
            // value, then we add 1 to get our valid label
            let loop_number = LOOPS.fetch_add(1, Ordering::Acquire) + 1;
            *label = loop_name(loop_number);
            label_statement(body, Some(&BodyName::Loop(loop_number)))
        }
        Stmt::If { then, r#else, .. } => {
            label_statement(then, cur_loop)?;
            if let Some(r#else) = r#else {
                label_statement(r#else, cur_loop)
            } else {
                Ok(())
            }
        }
        Stmt::Break(label) => {
            if let Some(cur_loop) = cur_loop {
                *label = block_name(cur_loop);
                Ok(())
            } else {
                Err(Error::Break)
            }
        }
        Stmt::Continue(label) => {
            if let Some(cur_loop) = cur_loop {
                *label = block_name(cur_loop);
                Ok(())
            } else {
                Err(Error::Continue)
            }
        }

        Stmt::Compound(block) => label_blocks(&mut block.0, cur_loop),
        Stmt::Labeled {
            label: _,
            statement,
        } => label_statement(statement, cur_loop),
        Stmt::Switch {
            condition: _,
            body,
            label,
        } => {
            let new_name = switch_name();
            *label = new_name;

            label_switch(body, label)
        }
        _ => todo!(),
    }
}

fn label_switch(statement: &mut parse::Statement, label: &String) -> Result<(), Error> {
    use parse::Statement as Stmt;
    match statement {
        Stmt::Continue(_) => Err(Error::ContinueInSwitch),
        Stmt::Break(break_label) => {
            *break_label = label.clone();
            Ok(())
        }
        // handle switch case in seperate pass
        _ => label_statement(statement, None),
    }
}

fn new_label() -> usize {
    LOOPS.fetch_add(1, Ordering::Acquire) + 1
}

fn block_name(name: &BodyName) -> String {
    match name {
        BodyName::Loop(number) => loop_name(*number),
        BodyName::Switch(name) => name.clone(),
    }
}

fn loop_name(number: usize) -> String {
    format!("_loop{}", number)
}

fn switch_name() -> String {
    let num = new_label();
    format!("_switch{}", num)
}
