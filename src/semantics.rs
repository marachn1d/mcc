mod check_labels;
mod resolve;
mod typecheck;
use crate::lex::Identifier;
use crate::parse;
use crate::parse::Expression;
use crate::parse::ForInit;
use crate::parse::ParamList;
use crate::parse::Program as AstProgram;
use crate::parse::StorageClass;
pub use check_labels::check as check_labels;
use parse::VariableDeclaration;
pub use typecheck::Attr;
pub use typecheck::SymbolTable;
mod resolve_loops;
pub use resolve::resolve;

pub fn check(mut program: AstProgram) -> Result<(Program, SymbolTable), Error> {
    resolve(&mut program).map_err(Error::Resolve)?;

    let mut program = resolve_loops::label(program).map_err(Error::Loops)?;

    let symbol_table = typecheck::typecheck(&mut program).map_err(Error::TypeCheck)?;

    check_labels::check(&program, &symbol_table).map_err(Error::Label)?;

    Ok((program, symbol_table))
}

#[derive(Debug, Copy, Clone)]
pub struct LabelId(usize);

#[derive(Debug, Clone)]
pub struct StatementLabels {
    pub start: Identifier,
    pub r#break: Identifier,
    pub r#continue: Identifier,
    pub end: Identifier,
}

impl LabelId {
    pub fn labels(&self) -> StatementLabels {
        StatementLabels {
            start: Identifier::from(format!("s{}s", self.0)),
            r#break: self.r#break(),
            r#continue: self.r#continue(),
            end: Identifier::from(format!("s{}e", self.0)),
        }
    }

    pub fn r#break(&self) -> Identifier {
        Identifier::from(format!("s{}b", self.0))
    }

    pub fn case(&self, value: u64) -> Identifier {
        Identifier::from(format!("sc{}{}", self.0, value))
    }
    pub fn default(&self) -> Identifier {
        Identifier::from(format!("sc{}d", self.0))
    }
    pub fn r#continue(&self) -> Identifier {
        Identifier::from(format!("s{}c", self.0))
    }
}

pub type Block = Box<[BlockItem]>;

#[derive(Debug)]
pub struct Program(pub Box<[Declaration]>);

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub name: Identifier,
    pub params: ParamList,
    pub body: Option<Block>,
    pub storage_class: Option<StorageClass>,
}

#[derive(Debug)]
pub enum BlockItem {
    S(Statement),
    D(Declaration),
}

#[derive(Debug)]
pub enum Declaration {
    Function {
        name: Identifier,
        params: ParamList,
        body: Option<Block>,
        storage_class: Option<StorageClass>,
    },
    Var {
        name: Identifier,
        init: Option<Expression>,
        storage_class: Option<StorageClass>,
    },
}

impl From<VariableDeclaration> for Declaration {
    fn from(
        VariableDeclaration {
            name,
            init,
            storage_class,
        }: VariableDeclaration,
    ) -> Self {
        Self::Var {
            name,
            init,
            storage_class,
        }
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
    Goto(Identifier),
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
    Named(Identifier),
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
