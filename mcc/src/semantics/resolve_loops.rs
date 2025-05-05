use ast::labeled::prelude::*;
use ast::parse;
use ast::Expr;
use parse::BlockItem as AstBlockItem;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Invalid Break")]
    Break,
    #[error("Invalid Continue")]
    Continue,
    #[error("Invalid Switch")]
    Switch,
    #[error("Invalid Case")]
    Case,
    #[error("Duplicate Default")]
    DoubleDefault,
    #[error("Duplicate Case")]
    DupliCase,
}

pub fn label(program: parse::Program) -> Result<Program, Error> {
    let mut decs = Vec::new();
    for dec in program.0 {
        decs.push(label_declaration(dec, &mut Scope::default())?);
    }
    // works
    Ok(decs.into_boxed_slice())
}

fn label_declaration<'a>(dec: parse::Dec<'a>, cur_loop: &mut Scope) -> Result<Dec<'a>, Error> {
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

fn label_blocks<'a>(
    block: Box<[parse::BlockItem<'a>]>,
    cur_loop: &mut Scope,
) -> Result<Block<'a>, Error> {
    let mut vec = Vec::with_capacity(block.len());
    for item in block {
        vec.push(match item {
            AstBlockItem::S(s) => label_statement(s, cur_loop).map(BlockItem::S)?,
            AstBlockItem::D(d) => BlockItem::D(label_declaration(d, cur_loop)?),
        });
    }
    Ok(vec.into())
}

fn do_while<'a>(
    body: parse::Stmnt<'a>,
    condition: Expr<'a>,
    cur: &mut Scope,
) -> Result<Stmnt<'a>, Error> {
    let condition = condition.into();
    let prev_normal = cur.normal;
    let label = LabelId::new();

    cur.normal = Some(label);
    let body = Box::new(label_statement(body, cur)?);
    cur.normal = prev_normal;
    Ok(Stmnt::DoWhile {
        body,
        condition,
        label,
    })
}

fn while_stmnt<'a>(
    body: parse::Stmnt<'a>,
    condition: Expr<'a>,
    cur: &mut Scope,
) -> Result<Stmnt<'a>, Error> {
    let prev_normal = cur.normal;
    let label = LabelId::new();

    cur.normal = Some(label);
    let body = label_statement(body, cur)?.into();

    cur.normal = prev_normal;
    Ok(Stmnt::While {
        condition: condition.into(),
        body,
        label,
    })
}

fn for_stmnt<'a>(
    body: parse::Stmnt<'a>,
    post: Option<Expr<'a>>,
    r#init: Option<parse::ForInit<'a>>,
    condition: Option<Expr<'a>>,
    cur: &mut Scope,
) -> Result<Stmnt<'a>, Error> {
    let prev_normal = cur.normal;
    let label = LabelId::new();

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
                init,
                sc,
                typ,
            }),
            parse::ForInit::E(e) => ForInit::E(e.into()),
        })
        .map(Box::new);
    Ok(Stmnt::For {
        body,
        post,
        init,
        condition,
        label,
    })
}

fn label_statement<'a>(statement: parse::Stmnt<'a>, cur: &mut Scope) -> Result<Stmnt<'a>, Error> {
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
        parse::Stmnt::NamedLabel { label: l, body } => {
            label_statement(*body, cur).map(|b| Stmnt::NamedLabel {
                l,
                body: Box::new(b),
            })
        }
        parse::Stmnt::Default(body) => {
            let id = cur.found_default()?;
            label_statement(*body, cur).map(|s| Stmnt::Default(with_label(s, id)))
        }

        parse::Stmnt::Case { case, body } => {
            let id = cur.found_default()?;
            label_statement(*body, cur).map(|s| Stmnt::Case {
                case,
                stmnt: with_label(s, id),
            })
        }

        parse::Stmnt::Switch { val, body } => {
            let label = LabelId::new();
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

fn with_label<'a>(s: Stmnt<'a>, id: LabelId) -> LabelStmnt<'a> {
    LabelStmnt {
        body: Box::new(s),
        id,
    }
}

fn if_stmnt<'a>(
    c: Expr<'a>,
    then: parse::Stmnt<'a>,
    r#else: Option<parse::Stmnt<'a>>,
    cur: &mut Scope,
) -> Result<Stmnt<'a>, Error> {
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

    const fn found_default(&mut self) -> Result<LabelId, Error> {
        if self.default {
            Err(Error::DoubleDefault)
        } else {
            self.default = true;
            Ok(self.label)
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

    const fn found_default(&mut self) -> Result<LabelId, Error> {
        if let Some(default) = &mut self.switch {
            default.found_default()
        } else {
            Err(Error::Switch)
        }
    }
}
