use std::sync::atomic::{AtomicUsize, Ordering};
static LOOPS: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug)]
pub enum Error {
    Break,
    Continue,
}

use crate::parse;
pub fn label(parse::Program(main): &mut parse::Program) -> Result<(), Error> {
    let blocks = &mut main.body.0;
    label_blocks(blocks, None)
}

fn label_blocks(block: &mut [parse::BlockItem], cur_loop: Option<usize>) -> Result<(), Error> {
    for item in block {
        if let parse::BlockItem::S(s) = item {
            label_statement(s, cur_loop)?;
        }
    }
    Ok(())
}

fn label_statement(statement: &mut parse::Statement, cur_loop: Option<usize>) -> Result<(), Error> {
    use parse::Statement as Stmt;
    match statement {
        Stmt::DoWhile { label, body, .. }
        | Stmt::For { label, body, .. }
        | Stmt::While { label, body, .. } => {
            // increment the loop nubmer to get a unique id         fetch add returns previous
            // value, then we add 1 to get our valid label
            let loop_number = LOOPS.fetch_add(1, Ordering::Acquire) + 1;
            *label = loop_name(loop_number);
            label_statement(body, Some(loop_number))
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
                *label = loop_name(cur_loop);
                Ok(())
            } else {
                Err(Error::Break)
            }
        }
        Stmt::Continue(label) => {
            if let Some(cur_loop) = cur_loop {
                *label = loop_name(cur_loop);
                Ok(())
            } else {
                Err(Error::Continue)
            }
        }

        Stmt::Compound(block) => label_blocks(&mut block.0, cur_loop),
        _ => Ok(()),
    }
}

fn new_label() -> usize {
    LOOPS.fetch_add(1, Ordering::Acquire) + 1
}

fn loop_name(number: usize) -> String {
    format!("_loop{}", number)
}
