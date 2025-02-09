use super::Label;
use std::sync::atomic::{AtomicUsize, Ordering};
static LOOPS: AtomicUsize = AtomicUsize::new(0);
use crate::lex::Identifier;
use std::rc::Rc;

#[derive(Debug)]
pub enum Error {
    Break,
    Continue,
    ContinueInSwitch,
}

use crate::parse;
pub fn label(parse::Program(main): parse::Program) -> Result<super::Program, Error> {
    Ok(super::Program(super::Function::main(label_blocks(
        main.body.0,
        None,
    )?)))
}

fn label_blocks(
    block: Box<[parse::BlockItem]>,
    cur_loop: Option<&BodyName>,
) -> Result<Box<[super::BlockItem]>, Error> {
    let mut new_block = Vec::with_capacity(block.len());
    for item in block {
        new_block.push(match item {
            parse::BlockItem::S(s) => super::BlockItem::S(label_statement(s, cur_loop)?),
            parse::BlockItem::D(d) => super::BlockItem::D(d),
        })
    }
    Ok(new_block.into())
}

#[derive(Debug, Clone)]
enum BodyName {
    Loop(LoopNumber),
    Switch(String),
}

#[derive(Debug, Clone, Copy)]
struct LoopNumber(usize);

#[derive(Debug, Clone, Copy)]
struct SwitchNumber(usize);

fn loop_number() -> usize {
    // increment the loop nubmer to get a unique id         fetch add returns previous
    // value, then we add 1 to get our valid label
    LOOPS.fetch_add(1, Ordering::Acquire) + 1
}

fn handle_body(
    body: parse::Statement,
    cur_loop: Option<&BodyName>,
) -> Result<(Box<super::Statement>, String), Error> {
    let name = loop_name();
    // statement label
    let body = Box::new(label_statement(body, cur_loop)?);
    Ok((body, name))
}

/*
 *
 * Ideal version: give continue and break and Switch unique BlockIds that will turn into BlockLabel
 * pairs on a final pass
 *
 * LabelId is parametrized so like BlockId<PostSwitch> BlockId<ForContinue> BlockId<ForBreak>
 *
 *
 *
 *
 *
 *
 *
 */

fn label_statement(
    statement: parse::Statement,
    cur_loop: Option<&BodyName>,
    //
    switch_body: Option,
) -> Result<super::Statement, Error> {
    use parse::Statement as Stmt;
    match statement {
        Stmt::DoWhile { body, condition } => {
            let (body, label) = handle_body(*body, cur_loop)?;
            Ok(super::Statement::DoWhile {
                body,
                condition,
                label,
            })
        }
        Stmt::For {
            init,
            body,
            post,
            condition,
        } => {
            let (body, label) = handle_body(*body, cur_loop)?;
            Ok(super::Statement::For {
                init,
                body,
                post,
                condition,
                label,
            })
        }
        Stmt::While { body, condition } => {
            let (body, label) = handle_body(*body, cur_loop)?;
            Ok(super::Statement::While {
                body,
                condition,
                label,
            })
        }
        Stmt::If {
            then,
            r#else,
            condition,
        } => {
            let then = Box::new(label_statement(*then, cur_loop)?);
            let r#else = match r#else {
                Some(stmt) => Some(Box::new(label_statement(*stmt, cur_loop)?)),
                None => None,
            };
            Ok(super::Statement::If {
                then,
                r#else,
                condition,
            })
        }
        Stmt::Break => {
            if let Some(cur_loop) = cur_loop {
                let label = block_name(cur_loop);
                Ok(super::Statement::Break(label))
            } else {
                Err(Error::Break)
            }
        }
        Stmt::Continue => {
            if let Some(cur_loop) = cur_loop {
                let label = block_name(cur_loop);
                Ok(super::Statement::Continue(label))
            } else {
                Err(Error::Continue)
            }
        }
        Stmt::Compound(block) => Ok(super::Statement::Compound(label_blocks(block.0, cur_loop)?)),

        Stmt::Labeled { label, statement } => {
            match label {
                // if we have a switch in our scope check default
                Label::Default { switch_label } => todo!(),
                // If we have a switch in our scope add it in and
                Label::Case { case, switch_label } => todo!(),
                // nothing
                Label::Named(name) => todo!(),
            }
        }
        Stmt::Switch {
            condition,
            body,
            default,
        } => {
            let label = switch_name();
            let (body, cases) = switch_cases(*body, label.clone())?;
            Ok(super::Statement::Switch {
                condition,
                default,
                body: Box::new(body),
                label,
                cases,
            })
        }
        Stmt::Ret(r) => Ok(super::Statement::Ret(r)),

        Stmt::Exp(e) => Ok(super::Statement::Exp(e)),

        Stmt::Goto(g) => Ok(super::Statement::Goto(g)),

        Stmt::Null => Ok(super::Statement::Null),
    }
}

fn switch_cases(
    body: parse::Statement,
    cur_loop: String,
) -> Result<(super::Statement, Box<[(u64, Rc<Identifier>)]>), Error> {
    // on switch statement
    todo!()
}

fn label_switch(
    statement: parse::Statement,
    label: &String,
) -> Result<Box<super::Statement>, Error> {
    use parse::Statement as Stmt;
    match statement {
        Stmt::Continue => Err(Error::ContinueInSwitch),
        Stmt::Break => Ok(Box::new(super::Statement::Break(label.clone()))),
        // handle switch case in seperate pass
        _ => Ok(Box::new(label_statement(statement, None)?)),
    }
}

fn block_name(name: &BodyName) -> String {
    match name {
        BodyName::Loop(number) => loop_name(),
        BodyName::Switch(name) => name.clone(),
    }
}

fn loop_name() -> String {
    let num = loop_number();
    format!("_loop{}", num)
}

fn switch_name() -> String {
    let num = loop_number();
    format!("_switch{}", num)
}
