use ast::semantics::labeled;
use ast::semantics::{Label, LabelId};
use ast::Arr;


use labeled::{Block, BlockItem, Dec, Expr, FnDec, ForInit, Program, Stmnt, VarDec};

use std::sync::atomic::{AtomicUsize, Ordering};
static LOOPS: AtomicUsize = AtomicUsize::new(0);
use crate::parse;
use crate::parse::Label as AstLabel;
use parse::BlockItem as AstBlockItem;

#[derive(Debug)]
pub enum Error {
    Break,
    OObContinue,
    Continue,
    Switch,
    Case,
    DoubleDefault,
    DupliCase,
}

fn new_label() -> LabelId {
    LabelId(LOOPS.fetch_add(1, Ordering::Acquire) + 1)
}

pub fn label(program: parse::Program) -> Result<Program, Error> {
    let mut decs = Vec::new();
    for dec in program.0 {
        decs.push(label_declaration(dec, &mut Scope::default())?);
    }
    // works
    Ok(decs.into_boxed_slice())
}

fn label_declaration(dec: parse::Dec, cur_loop: &mut Scope) -> Result<Dec, Error> {
    match dec {
        parse::Dec::Var(parse::VarDec {
            name,
            init,
            sc,
            typ,
        }) => Ok(Dec::Var(VarDec {
            name,
            init: init.map(|x| x.into()),
            sc,
            typ,
        })),

        parse::Dec::Fn(parse::FnDec {
            name,
            params,
            body: None,
            sc,
            typ,
        }) => Ok(Dec::Fn(FnDec {
            name,
            params,
            body: None,
            sc,
            typ,
        })),
        parse::Dec::Fn(parse::FnDec {
            name,
            params,
            body: Some(body),
            sc,
            typ,
        }) => label_blocks(body, cur_loop).map(|body| {
            Dec::Fn(FnDec {
                name,
                params,
                body: Some(body),
                sc,
                typ,
            })
        }),
    }
}

fn label_blocks(block: Arr<parse::BlockItem>, cur_loop: &mut Scope) -> Result<Block, Error> {
    let mut vec = Vec::with_capacity(block.len());
    for item in block {
        vec.push(match item {
            AstBlockItem::S(s) => label_statement(s, cur_loop).map(BlockItem::S)?,
            AstBlockItem::D(d) => BlockItem::D(label_declaration(d, cur_loop)?),
        });
    }
    Ok(vec.into())
}

fn do_while(body: parse::Stmnt, c: parse::Expr, cur: &mut Scope) -> Result<Stmnt, Error> {
    let (body, label) = loop_stmt(body, cur)?;
    Ok(Stmnt::DoWhile {
        body,
        condition: c.into(),
        label,
    })
}

fn loop_stmt(s: parse::Stmnt, scope: &mut Scope) -> Result<(Box<Stmnt>, LabelId), Error> {
    let (old, label) = scope.new_loop();
    let s = Box::new(label_statement(s, scope)?);
    scope.reset(old);
    Ok((s, label))
}

fn loop_switch(s: parse::Stmnt, scope: &mut Scope) -> Result<(Box<Stmnt>, LabelId), Error> {
    let (old, label) = scope.new_switch();
    let s = Box::new(label_statement(s, scope)?);
    scope.reset(old);
    Ok((s, label))
}

fn while_stmnt(
    body: parse::Stmnt,
    condition: parse::Expr,
    cur: &mut Scope,
) -> Result<Stmnt, Error> {
    let (body, label) = loop_stmt(body, cur)?;
    Ok(Stmnt::While {
        condition: condition.into(),
        body,
        label,
    })
}

fn exp(exp: Option<parse::Expr>) -> Option<Expr> {
    exp.map(Expr::from)
}

fn for_stmnt(
    body: parse::Stmnt,
    post: Option<parse::Expr>,
    r#init: Option<parse::ForInit>,
    condition: Option<parse::Expr>,
    cur: &mut Scope,
) -> Result<Stmnt, Error> {
    let (body, label) = loop_stmt(body, cur)?;

    let init = init
        .map(|init| match init {
            parse::ForInit::D(parse::VarDec {
                name,
                init,
                sc,
                typ,
            }) => ForInit::D(VarDec {
                name,
                init: exp(init),
                sc,
                typ,
            }),
            parse::ForInit::E(e) => ForInit::E(e.into()),
        })
        .map(Box::new);
    Ok(Stmnt::For {
        body,
        post: exp(post),
        init,
        condition: exp(condition),
        label,
    })
}

fn label_statement(statement: parse::Stmnt, cur: &mut Scope) -> Result<Stmnt, Error> {
    //use parse::Stmnt;
    match statement {
        parse::Stmnt::DoWhile { body, condition } => do_while(*body, condition, cur),
        parse::Stmnt::For {
            body,
            post,
            init,
            condition,
        } => for_stmnt(*body, post, init, condition, cur),
        parse::Stmnt::While { body, condition } => while_stmnt(*body, condition, cur),
        parse::Stmnt::If {
            condition: c,
            then: t,
            r#else: e,
        } => if_stmnt(c, *t, e.map(|e| *e), cur),

        parse::Stmnt::Break => match cur.cur() {
            Some(cur) => Ok(Stmnt::Break(cur)),
            None => Err(Error::Break),
        },

        parse::Stmnt::Continue if cur.in_loop() => match cur.loop_id {
            Some(cur) => Ok(Stmnt::Continue(cur)),
            None => Err(Error::Continue),
        },

        parse::Stmnt::Continue => Err(Error::OObContinue),

        parse::Stmnt::Compound(block) => label_blocks(block, cur).map(Stmnt::Compound),
        parse::Stmnt::Ret(e) => Ok(Stmnt::Ret(e.into())),
        parse::Stmnt::Exp(e) => Ok(Stmnt::Exp(e.into())),
        parse::Stmnt::Goto(g) => Ok(Stmnt::Goto(g)),
        parse::Stmnt::Label { label, body } => {
            let name = match label {
                AstLabel::Named(name) => Label::Named(name),
                AstLabel::Case(c) => Label::Case {
                    c,
                    id: cur.switch.ok_or(Error::Switch)?,
                },
                AstLabel::Default => Label::Default(cur.switch.ok_or(Error::Switch)?),
            };
            label_statement(*body, cur).map(|body| Stmnt::Label {
                name,
                body: Box::new(body),
            })
        }
        // we're gonna handle this in it's own pass since we wanna do it after type conversions
        parse::Stmnt::Switch { val, body } => {
            let (body, label) = loop_switch(*body, cur)?;
            Ok(Stmnt::Switch {
                val: val.into(),
                body,
                cases: Box::new([]),
                default: false,
                label,
            })
        }
        parse::Stmnt::Null => Ok(Stmnt::Null),
    }
}

fn if_stmnt(
    c: parse::Expr,
    then: parse::Stmnt,
    r#else: Option<parse::Stmnt>,
    cur: &mut Scope,
) -> Result<Stmnt, Error> {
    let then = Box::new(label_statement(then, cur)?);
    let r#else = if let Some(r#else) = r#else {
        Some(Box::new(label_statement(r#else, cur)?))
    } else {
        None
    };
    Ok(Stmnt::If {
        then,
        r#else,
        condition: c.into(),
    })
}

#[derive(Clone, Copy)]
struct Scope {
    loop_id: Option<LabelId>,
    switch: Option<LabelId>,
}

impl Scope {
    const fn default() -> Self {
        Scope {
            loop_id: None,
            switch: None,
        }
    }

    fn reset(&mut self, old: Self) {
        *self = old;
    }

    fn new_switch(&mut self) -> (Self, LabelId) {
        let old = *self;
        let new_id = new_label();
        self.switch = Some(new_id);
        (old, new_id)
    }

    fn new_loop(&mut self) -> (Self, LabelId) {
        let old = *self;
        let new_id = new_label();
        self.loop_id = Some(new_id);
        (old, new_id)
    }

    fn in_loop(&self) -> bool {
        self.loop_id.is_some()
    }

    fn cur(&self) -> Option<LabelId> {
        if let (Some(loop_id), Some(switch)) = (self.loop_id, self.switch) {
            if loop_id.0 > switch.0 {
                Some(loop_id)
            } else {
                Some(switch)
            }
        } else {
            self.loop_id.or(self.switch)
        }
    }
}
