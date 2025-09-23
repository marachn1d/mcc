use ast::semantics::labeled;
use ast::semantics::{Label, LabelId};
use ast::Arr;
use ast::VarType;

use ast::Constant;

use labeled::{Block, BlockItem, Dec, Expr, FnDec, ForInit, Program, Stmnt, SwitchCase, VarDec};

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
    handle_statement(statement, cur, false)
}

fn label_switch_body_statement(statement: parse::Stmnt, cur: &mut Scope) -> Result<Stmnt, Error> {
    handle_statement(statement, cur, true)
}

fn handle_statement(
    statement: parse::Stmnt,
    cur: &mut Scope,
    in_switch: bool,
) -> Result<Stmnt, Error> {
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
        } => handle_statement(*body, cur, in_switch).map(|body| {
            Ok(Stmnt::Label {
                name: Label::Named(name),
                body: Box::new(body),
            })
        })?,
        parse::Stmnt::Label {
            label: parse::Label::Default | parse::Label::Case(_),
            ..
        } if !in_switch => Err(Error::Switch),

        parse::Stmnt::Label {
            label: label @ (parse::Label::Default | parse::Label::Case(_)),
            body,
        } => {
            let case = resolve_switch_label(label);
            let body = handle_statement(*body, cur, in_switch)?;
            //Ok(Stmnt::Label{})
            todo!()
        }
        parse::Stmnt::Switch { val, cases } => resolve_switch(cur, val, cases),
        parse::Stmnt::Null => Ok(Stmnt::Null),
    }
}

fn resolve_switch_label(case: AstLabel) -> labeled::Case {
    match case {
        AstLabel::Case(case) => labeled::Case::Case(exp(Some(case)).unwrap()),
        AstLabel::Default => labeled::Case::Default,
        AstLabel::Named(_) => unreachable!(),
    }
}

fn resolve_switch(
    scope: &mut Scope,
    val: parse::Expr,
    cases: Box<[parse::SwitchCase]>,
) -> Result<Stmnt, Error> {
    let label = new_label();
    let prev_switch = scope.switch.replace(label);
    use std::collections::HashSet;
    let mut seen_cases = HashSet::new();
    let mut new_cases: Vec<SwitchCase> = Vec::with_capacity(cases.len());
    use ast::parse::Case;
    for parse::SwitchCase { case, body } in cases {
        let case = if let Some(c) = case {
            let c = match c {
                Case::Case(c) => labeled::Case::Case(exp(Some(c)).unwrap()),
                Case::Default => labeled::Case::Default,
            };
            if !seen_cases.insert(c.clone()) {
                return Err(Error::DupliCase);
            }
            Some(c)
        } else {
            None
        };
        new_cases.push(SwitchCase::new(case, label_statement(body, scope)?))
    }
    scope.switch = prev_switch;
    Ok(Stmnt::Switch {
        val: val.into(),
        cases: new_cases.into(),
        label,
    })
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
    normal: Option<LabelId>,
    switch: Option<LabelId>,
}

impl Scope {
    fn in_switch(&self) -> bool {
        self.switch.is_some()
    }

    const fn default() -> Self {
        Scope {
            normal: None,
            switch: None,
        }
    }

    fn cur(&self) -> Option<LabelId> {
        match (self.normal, self.switch) {
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
