use std::sync::atomic::{AtomicUsize, Ordering};
static LOOPS: AtomicUsize = AtomicUsize::new(0);
use super::AstProgram;

use super::BlockItem;
use super::Declaration;
use super::Label;
use super::LabelId;
use super::Program;
use super::Statement;
use parse::BlockItem as AstBlockItem;

use parse::Declaration as AstDeclaration;
use parse::Label as AstLabel;

#[derive(Debug)]
pub enum Error {
    Break,
    Continue,
    Switch,
    Case,
    DoubleDefault,
    DupliCase,
}

fn new_label() -> LabelId {
    LabelId(LOOPS.fetch_add(1, Ordering::Acquire) + 1)
}

use crate::parse;
pub fn label(program: AstProgram) -> Result<Program, Error> {
    let mut decs = Vec::new();
    for dec in program.0 {
        decs.push(label_declaration(dec, &mut Scope::default())?);
    }
    Ok(Program(decs.into()))
}

fn label_declaration(dec: AstDeclaration, cur_loop: &mut Scope) -> Result<Declaration, Error> {
    match dec {
        AstDeclaration::Var {
            name,
            init,
            storage_class,
        } => Ok(Declaration::Var {
            name,
            init,
            storage_class,
        }),

        AstDeclaration::Function {
            name,
            params,
            body: None,
            storage_class,
        } => Ok(Declaration::Function {
            name,
            params,
            body: None,
            storage_class,
        }),
        AstDeclaration::Function {
            name,
            params,
            body: Some(body),
            storage_class,
        } => label_blocks(body.0, cur_loop).map(|body| Declaration::Function {
            name,
            params,
            body: Some(body),
            storage_class,
        }),
    }
}

fn label_blocks(
    block: Box<[parse::BlockItem]>,
    cur_loop: &mut Scope,
) -> Result<Box<[BlockItem]>, Error> {
    let mut vec = Vec::with_capacity(block.len());
    for item in block {
        vec.push(match item {
            AstBlockItem::S(s) => label_statement(s, cur_loop).map(BlockItem::S)?,
            AstBlockItem::D(d) => BlockItem::D(label_declaration(d, cur_loop)?),
        });
    }
    Ok(vec.into())
}

fn label_statement(statement: parse::Statement, cur: &mut Scope) -> Result<Statement, Error> {
    use parse::Statement as Stmt;
    match statement {
        Stmt::DoWhile { body, condition } => {
            let prev_normal = cur.normal;
            let label = new_label();

            cur.normal = Some(label);
            let body = label_statement(*body, cur)?.into();
            cur.normal = prev_normal;
            Ok(Statement::DoWhile {
                body,
                label,
                condition,
            })
        }
        Stmt::For {
            body,
            post,
            init,
            condition,
        } => {
            let prev_normal = cur.normal;
            let label = new_label();

            cur.normal = Some(label);
            let body = label_statement(*body, cur)?.into();

            cur.normal = prev_normal;
            Ok(Statement::For {
                body,
                post,
                init,
                condition,
                label,
            })
        }
        Stmt::While { body, condition } => {
            let prev_normal = cur.normal;
            let label = new_label();

            cur.normal = Some(label);
            let body = label_statement(*body, cur)?.into();

            cur.normal = prev_normal;
            Ok(Statement::While {
                condition,
                body,
                label,
            })
        }
        Stmt::If {
            then,
            r#else,
            condition,
        } => {
            let then = Box::new(label_statement(*then, cur)?);
            let r#else = if let Some(r#else) = r#else {
                Some(Box::new(label_statement(*r#else, cur)?))
            } else {
                None
            };
            Ok(Statement::If {
                then,
                r#else,
                condition,
            })
        }

        Stmt::Break => match cur.cur() {
            Some(cur) => Ok(Statement::Break(cur)),
            None => Err(Error::Break),
        },
        Stmt::Continue => match cur.normal {
            Some(cur) => Ok(Statement::Continue(cur)),
            None => Err(Error::Continue),
        },

        Stmt::Compound(block) => label_blocks(block.0, cur).map(Statement::Compound),
        Stmt::Ret(e) => Ok(Statement::Ret(e)),
        Stmt::Exp(e) => Ok(Statement::Exp(e)),
        Stmt::Goto(g) => Ok(Statement::Goto(g)),
        Stmt::Label {
            label: AstLabel::Named(name),
            body,
        } => label_statement(*body, cur).map(|body| {
            Ok(Statement::Label {
                name: Label::Named(name),
                body: Box::new(body),
            })
        })?,
        // current plan is to pass a struct called scope which keeps the loop id for switch
        // statements and other Loops seperately, cur checks labelids and uses the lesser one,
        // and handles continue targets
        Stmt::Label {
            label: AstLabel::Default,
            body,
        } => {
            // borrowck is evil and won't let me copy the lable id properly so instead i'm doing
            // this evil thing
            let label = match cur.switch.as_mut() {
                // this means we got default in an env with an alreadu defined default. Illegal.
                Some(SwitchState { default: true, .. }) => Err(Error::DoubleDefault),

                // this means we got
                Some(SwitchState {
                    default,
                    cases: _,
                    label,
                }) => {
                    *default = true;
                    Ok(*label)
                }
                None => Err(Error::Switch),
            }?;
            let body = label_statement(*body, cur)?.into();
            Ok(Statement::Label {
                name: super::Label::Default(label),
                body,
            })
        }
        Stmt::Label {
            label: AstLabel::Case(exp),
            body,
        } => {
            let Some(switch) = &mut cur.switch else {
                return Err(Error::Switch);
            };
            let Some(num) = exp.number() else {
                return Err(Error::Case);
            };
            switch.cases.push(num);
            let body = label_statement(*body, cur)?.into();
            let id = cur.switch.as_ref().unwrap().label;
            Ok(Statement::Label {
                name: Label::Case { val: num, id },
                body,
            })
        }

        Stmt::Switch { val, body } => {
            let label = new_label();
            let prev_switch = cur.switch.replace(SwitchState::new(label));
            let body = label_statement(*body, cur)?.into();
            let mut switch_info = if let Some(prev) = prev_switch {
                cur.switch.replace(prev)
            } else {
                cur.switch.take()
            }
            .unwrap();
            switch_info.cases.sort();
            if switch_info.cases.windows(2).any(|x| x[0] == x[1]) {
                return Err(Error::DupliCase);
            }
            // make sure the cases are unique, hmm.
            //
            Ok(Statement::Switch {
                val,
                body,
                cases: switch_info.cases.into(),
                default: switch_info.default,
                label,
            })
        }
        Stmt::Null => Ok(Statement::Null),
    }
}

#[derive(Clone)]
struct Scope {
    normal: Option<LabelId>,
    switch: Option<SwitchState>,
}

#[derive(Clone)]
struct SwitchState {
    default: bool,
    cases: Vec<u64>,
    label: LabelId,
}

impl SwitchState {
    const fn new(id: LabelId) -> Self {
        Self {
            default: false,
            cases: Vec::new(),
            label: id,
        }
    }
}

impl Scope {
    const fn default() -> Self {
        Scope {
            normal: None,
            switch: None,
        }
    }

    fn cur(&self) -> Option<LabelId> {
        let switch_label = self.switch.as_ref().map(|x| x.label);
        match (self.normal, switch_label) {
            (Some(s), None) | (None, Some(s)) => Some(s),
            (None, None) => None,

            (Some(normal), Some(switch)) => {
                if normal.0 > switch.0 {
                    Some(normal)
                } else {
                    Some(switch)
                }
            }
        }
    }
}
