use ast::semantics::labeled;
use ast::semantics::typed;
use ast::semantics::{Label, LabelId};
use ast::Arr;
use ast::VarType;

use ast::Constant;

use labeled::{Block, BlockItem, Dec, Expr, FnDec, ForInit, Program, Stmnt, VarDec};

use std::sync::atomic::{AtomicUsize, Ordering};
static LOOPS: AtomicUsize = AtomicUsize::new(0);
use crate::parse;
use crate::parse::Label as AstLabel;
use parse::BlockItem as AstBlockItem;

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

fn do_while(body: parse::Stmnt, condition: parse::Expr, cur: &mut Scope) -> Result<Stmnt, Error> {
    let condition = condition.into();
    let prev_normal = cur.normal;
    let label = new_label();

    cur.normal = Some(label);
    let body = Box::new(label_statement(body, cur)?);
    cur.normal = prev_normal;
    Ok(Stmnt::DoWhile {
        body,
        condition,
        label,
    })
}

fn while_stmnt(
    body: parse::Stmnt,
    condition: parse::Expr,
    cur: &mut Scope,
) -> Result<Stmnt, Error> {
    let prev_normal = cur.normal;
    let label = new_label();

    cur.normal = Some(label);
    let body = label_statement(body, cur)?.into();

    cur.normal = prev_normal;
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
    let prev_normal = cur.normal;
    let label = new_label();

    cur.normal = Some(label);
    let body = Box::new(label_statement(body, cur)?);
    cur.normal = prev_normal;

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
        parse::Stmnt::Continue => match cur.normal {
            Some(cur) => Ok(Stmnt::Continue(cur)),
            None => Err(Error::Continue),
        },

        parse::Stmnt::Compound(block) => label_blocks(block, cur).map(Stmnt::Compound),
        parse::Stmnt::Ret(e) => Ok(Stmnt::Ret(e.into())),
        parse::Stmnt::Exp(e) => Ok(Stmnt::Exp(e.into())),
        parse::Stmnt::Goto(g) => Ok(Stmnt::Goto(g)),
        parse::Stmnt::Label {
            label: AstLabel::Named(name),
            body,
        } => label_statement(*body, cur).map(|body| {
            Ok(Stmnt::Label {
                name: Label::Named(name),
                body: Box::new(body),
            })
        })?,
        // current plan is to pass a struct called scope which keeps the loop id for switch
        // statements and other Loops seperately, cur checks labelids and uses the lesser one,
        // and handles continue targets
        parse::Stmnt::Label {
            label: parse::Label::Default,
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
            Ok(Stmnt::Label {
                name: Label::Default(label),
                body,
            })
        }
        parse::Stmnt::Label {
            label: AstLabel::Case(c),
            body,
        } => {
            let Some(switch) = &mut cur.switch else {
                return Err(Error::Switch);
            };
            switch.cases.push(c);
            let body = label_statement(*body, cur)?.into();
            let id = cur.switch.as_ref().unwrap().label;
            Ok(Stmnt::Label {
                name: Label::Case { c, id },
                body,
            })
        }

        parse::Stmnt::Switch { val, body } => {
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
            // instead of doing this, we should change the cases when we typecheck
            if let Some(val) = val.const_eval() {
                for case in &mut switch_info.cases {
                    const_cast(case, &val.ty());
                }
            }
            // cases is sorted so we can do the same check as before (resolve_loops.rs)
            if switch_info.cases.windows(2).any(|x| x[0] == x[1]) {
                return Err(Error::DupliCase);
            }

            // make sure the cases are unique, hmm.
            //
            Ok(Stmnt::Switch {
                val: val.into(),
                body,
                cases: switch_info.cases.into(),
                default: switch_info.default,
                label,
            })
        }
        parse::Stmnt::Null => Ok(Stmnt::Null),
    }
}

fn const_cast(c: &mut Constant, ty: &VarType) {
    if c.ty() != *ty {
        match c {
            Constant::Int(i) => *c = Constant::Long(*i as i64),
            Constant::Long(l) => *c = Constant::Int(*l as i32),
        };
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

#[derive(Clone)]
struct Scope {
    normal: Option<LabelId>,
    switch: Option<SwitchState>,
}

#[derive(Clone)]
struct SwitchState {
    default: bool,
    cases: Vec<Constant>,
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
