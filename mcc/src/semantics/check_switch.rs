// here is where we label and typecheck switches, which get palceholder labels in the
// label_statements section, not ideal but what can ya do
//
//
//
//
use ast::semantics::typed::{BlockItem, Dec, FnDec, Program, Stmnt};
use ast::semantics::Label;
use ast::Constant;

use std::collections::HashSet;
pub struct SwitchState {
    cases: HashSet<Constant>,
    default: bool,
}

impl SwitchState {
    pub fn new() -> Self {
        Self {
            cases: HashSet::new(),
            default: false,
        }
    }
}

pub fn check(program: &mut Program) -> Result<(), Error> {
    for block in program.iter_mut().filter_map(|dec| match dec {
        Dec::Fn(FnDec {
            body: Some(body), ..
        }) => Some(body),
        _ => None,
    }) {
        for stmnt in block.iter_mut().filter_map(|x| {
            if let BlockItem::S(s) = x {
                Some(s)
            } else {
                None
            }
        }) {
            fix_statement_switches(stmnt, None)?
        }
    }
    Ok(())
}

fn fix_statement_switches(
    stmnt: &mut Stmnt,
    mut state: Option<&mut SwitchState>,
) -> Result<(), Error> {
    match stmnt {
        Stmnt::Switch {
            body,
            cases,
            default,
            ..
        } => {
            let mut new_state = SwitchState::new();
            fix_statement_switches(body, Some(&mut new_state))?;
            *default = new_state.default;
            *cases = new_state.cases.into_iter().collect()
        }

        Stmnt::Label {
            name: Label::Case { c, .. },
            body,
        } => {
            let Some(ref mut s) = state else {
                return Err(Error::EvilCase);
            };
            if !s.cases.insert(*c) {
                return Err(Error::DupliCase(*c));
            } else {
                fix_statement_switches(body, state)?;
            }
        }
        Stmnt::Label {
            name: Label::Default(_),
            body,
        } => {
            let Some(ref mut s) = state else {
                return Err(Error::EvilCase);
            };
            if s.default {
                return Err(Error::DupliDefault);
            } else {
                s.default = true;
                fix_statement_switches(body, state)?;
            }
        }

        Stmnt::Compound(block) => {
            for stmnt in block.iter_mut().filter_map(|x| {
                if let BlockItem::S(s) = x {
                    Some(s)
                } else {
                    None
                }
            }) {
                let borrow: &mut Option<&mut SwitchState> = &mut state;
                let inner_st = borrow.as_deref_mut();
                fix_statement_switches(stmnt, inner_st)?;
            }
        }

        Stmnt::If { then, r#else, .. } => {
            let borrow: &mut Option<&mut SwitchState> = &mut state;
            let inner_st = borrow.as_deref_mut();
            fix_statement_switches(then, inner_st)?;
            if let Some(s) = r#else {
                fix_statement_switches(s, state)?;
            }
        }
        Stmnt::For { body, .. }
        | Stmnt::Label { body, .. }
        | Stmnt::While { body, .. }
        | Stmnt::DoWhile { body, .. } => fix_statement_switches(body, state)?,
        Stmnt::Break(_)
        | Stmnt::Continue(_)
        | Stmnt::Goto(_)
        | Stmnt::Null
        | Stmnt::Ret(_)
        | Stmnt::Exp(_) => (),
    }

    Ok(())
}

#[derive(Debug)]
pub enum Error {
    DupliCase(Constant),
    DupliDefault,
    EvilCase,
}
