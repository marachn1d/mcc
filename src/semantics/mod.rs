mod check_labels;
mod resolve;
mod switch_cases;
use crate::parse;
pub use check_labels::check as check_labels;
use parse::Declaration;
use parse::Expression;
use parse::ForInit;
use parse::Label;
use std::rc::Rc;
mod label_loops;
pub use resolve::resolve;

use super::lex::Identifier;

pub fn check(mut program: parse::Program) -> Result<Program, Error> {
    // Resolve variables
    let vars = resolve(&mut program).map_err(Error::Resolve)?;
    // check to see if goto labels clach with variable names
    check_labels::check(&mut program, &vars).map_err(Error::Label)?;

    // label for loops and if statements for breaks and continues (and switches)
    let program = label_loops::label(program).map_err(Error::Loops)?;
    // label switch cases and wrap switch statements with all their cases
    switch_cases::switch_cases(&mut program).map_err(|_| Error::Switch)?;
    Ok(program)
}

use std::marker::PhantomData;
#[derive(Debug)]
struct ScopeId<T> {
    start: Rc<Identifier>,
    inner: Rc<Identifier>,
    end: Rc<Identifier>,
    marker: PhantomData<T>,
}

impl<T> ScopeId<T> {
    fn new(id: usize) -> Self {
        Self {
            start: Rc::new(format!("s{id}s").into()),
            inner: Rc::new(format!("s{id}s").into()),
            end: Rc::new(format!("s{id}s").into()),
            marker: PhantomData,
        }
    }
}

#[derive(Debug)]
struct Any;
#[derive(Debug)]
struct Global;
#[derive(Debug)]
struct Switch;
#[derive(Debug)]
struct JustStatement;

#[derive(Debug)]
pub struct Program(Function);

#[derive(Debug)]
pub struct Function {
    pub name: Identifier,
    pub body: Box<[BlockItem]>,
}

impl Function {
    fn main(body: Box<[BlockItem]>) -> Self {
        let name: Box<[u8]> = Box::new(*b"main");
        Self {
            name: Identifier(name),
            body,
        }
    }

    fn new(str: impl Into<String>, body: Box<[BlockItem]>) -> Self {
        let name: Box<[u8]> = str.into().into_bytes().into();
        Self {
            name: Identifier(name),
            body,
        }
    }
}

#[derive(Debug)]
pub enum BlockItem {
    D(Declaration),
    S(Statement),
}

#[derive(Debug)]
pub enum Statement {
    Ret(Expression),
    Exp(Expression),
    If {
        condition: Expression,
        then: Box<Statement>,
        r#else: Option<Box<Self>>,
    },
    // ID for the Label we break to
    Break(ScopeId<Any>),
    // ID for the Label we continue to
    Continue(ScopeId<JustStatement>),
    While {
        condition: Expression,
        body: Box<Self>,
        // ID for the Label we continue to
        scope: ScopeId<Any>,
    },
    DoWhile {
        body: Box<Self>,
        condition: Expression,
        scope: ScopeId<Any>,
    },
    For {
        init: Option<ForInit>,
        condition: Option<Expression>,
        post: Option<Expression>,
        body: Box<Self>,
        scope: ScopeId<Any>,
    },
    Compound(Box<[BlockItem]>),
    Labeled {
        label: Label,
        statement: Box<Self>,
    },
    Goto(Rc<Identifier>),
    Null,
    Switch {
        condition: Expression,
        body: Box<Self>,
        child_scope: ScopeId<Any>,
        cases: Box<[(u64, ScopeId<Switch>)]>,
        default: Option<ScopeId<Switch>>,
    },
}

#[derive(Debug)]
pub enum Error {
    Label(LabelError),
    Loops(LoopError),
    Resolve(ResolveError),

    Switch,
}

pub use check_labels::Error as LabelError;
use label_loops::Error as LoopError;
pub use resolve::Error as ResolveError;

pub use switch_cases::Error as SwitchError;
