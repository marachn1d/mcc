mod check_labels;
mod resolve;
mod typecheck;
use crate::lex::Identifier;
use crate::parse;
use crate::parse::Expression;
use crate::parse::ForInit;
use crate::parse::ParamList;
use crate::parse::Program as AstProgram;
pub use check_labels::check as check_labels;
use parse::VariableDeclaration;
use std::rc::Rc;
mod resolve_loops;
pub use resolve::resolve;
use std::collections::HashSet;

pub fn check(mut program: AstProgram) -> Result<Program, Error> {
    let vars = resolve(&mut program).map_err(Error::Resolve)?;

    let mut program = handle_labels(program, vars)?;

    typecheck::typecheck(&mut program).map_err(Error::TypeCheck)?;
    Ok(program)
}

fn handle_labels(
    mut program: AstProgram,
    var_map: HashSet<Rc<Identifier>>,
) -> Result<Program, Error> {
    // make sure that all gotos are to real labeled statements and
    check_labels::check(&mut program, &var_map).map_err(Error::Label)?;
    resolve_loops::label(program).map_err(Error::Loops)
}

#[derive(Debug, Copy, Clone)]
pub struct LabelId(usize);

#[derive(Debug, Clone)]
pub struct StatementLabels {
    pub start: Rc<Identifier>,
    pub _break: Rc<Identifier>,
    pub _continue: Rc<Identifier>,
    pub end: Rc<Identifier>,
}

impl LabelId {
    pub fn labels(&self) -> StatementLabels {
        StatementLabels {
            start: Identifier::new_rc(&format!("s{}s", self.0).into_bytes()),
            _break: self._break(),
            _continue: self._continue(),
            end: Identifier::new_rc(&format!("s{}e", self.0).into_bytes()),
        }
    }

    pub fn _break(&self) -> Rc<Identifier> {
        Identifier::new_rc(&format!("s{}b", self.0).into_bytes())
    }

    pub fn case(&self, value: u64) -> Rc<Identifier> {
        Identifier::new_rc(&format!("sc{}{}", self.0, value).into_bytes())
    }
    pub fn default(&self) -> Rc<Identifier> {
        Identifier::new_rc(&format!("sc{}d", self.0).into_bytes())
    }
    pub fn _continue(&self) -> Rc<Identifier> {
        Identifier::new_rc(&format!("s{}c", self.0).into_bytes())
    }
}

pub type Block = Box<[BlockItem]>;

#[derive(Debug)]
pub struct Program(pub Box<[FunctionDeclaration]>);

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub name: Rc<Identifier>,
    pub params: ParamList,
    pub body: Option<Block>,
}

#[derive(Debug)]
pub enum BlockItem {
    S(Statement),
    D(Declaration),
}

#[derive(Debug)]
pub enum Declaration {
    Function {
        name: Rc<Identifier>,
        params: ParamList,
        body: Option<Block>,
    },
    Var {
        name: Rc<Identifier>,
        init: Option<Expression>,
    },
}

impl From<VariableDeclaration> for Declaration {
    fn from(VariableDeclaration { name, init }: VariableDeclaration) -> Self {
        Self::Var { name, init }
    }
}

#[derive(Debug)]
pub enum Statement {
    Ret(Expression),
    Exp(Expression),
    If {
        condition: Expression,
        then: Box<Statement>,
        r#else: Option<Box<Statement>>,
    },
    Break(LabelId),
    Continue(LabelId),
    While {
        condition: Expression,
        body: Box<Self>,
        label: LabelId,
    },
    DoWhile {
        body: Box<Self>,
        condition: Expression,
        label: LabelId,
    },
    For {
        init: Option<ForInit>,
        condition: Option<Expression>,
        post: Option<Expression>,
        body: Box<Self>,
        label: LabelId,
    },
    Compound(Block),
    Label {
        name: Label,
        body: Box<Self>,
    },
    Goto(Rc<Identifier>),
    Switch {
        val: Expression,
        body: Box<Self>,
        label: LabelId,
        cases: Box<[u64]>,
        default: bool,
    },
    Null,
}

#[derive(Debug, Clone)]
pub enum Label {
    Named(Rc<Identifier>),
    Case { val: u64, id: LabelId },
    Default(LabelId),
}

#[derive(Debug)]
pub enum Error {
    Label(LabelError),
    Loops(LoopError),
    Resolve(ResolveError),
    TypeCheck(typecheck::Error),
}

pub use check_labels::Error as LabelError;
pub use resolve::Error as ResolveError;
use resolve_loops::Error as LoopError;
