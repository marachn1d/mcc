mod check_labels;
mod resolve;
pub mod typecheck;
use crate::lex::Identifier;
use crate::parse;
use crate::parse::Expression as AstExpression;
use crate::parse::ParamList;
use crate::parse::Program as AstProgram;
use crate::parse::StorageClass;
pub use check_labels::check as check_labels;
use parse::FnType;
use parse::VarType;
use parse::VariableDeclaration;
pub use typecheck::Attr;
pub use typecheck::StaticInit;
pub use typecheck::SymbolTable;
mod resolve_loops;
pub use resolve::resolve;

pub fn check(mut program: AstProgram) -> Result<(TypedProgram, SymbolTable), Error> {
    resolve(&mut program).map_err(Error::Resolve)?;

    let labeled = resolve_loops::label::<AstExpression>(program).map_err(Error::Loops)?;

    let (symbol_table, program) = typecheck::typecheck(labeled).map_err(Error::TypeCheck)?;

    check_labels::check(&program, &symbol_table).map_err(Error::Label)?;

    Ok((program, symbol_table))
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Type {
    Int,
    Long,
}

impl Type {
    const fn common_type(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Int, Self::Int) => Self::Int,
            _ => Self::Long,
        }
    }
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

    pub fn case(&self, value: i32) -> Identifier {
        Identifier::from(format!("sc{}{}", self.0, value))
    }
    pub fn default(&self) -> Identifier {
        Identifier::from(format!("sc{}d", self.0))
    }
    pub fn r#continue(&self) -> Identifier {
        Identifier::from(format!("s{}c", self.0))
    }
}

pub type Block<T> = Box<[BlockItem<T>]>;

#[derive(Debug)]
pub struct Program<T>(pub Box<[Declaration<T>]>);

pub type LabeledProgram = Program<AstExpression>;
pub type TypedProgram = Program<TypedExp>;

#[derive(Debug, Clone)]
pub struct TypedExp {
    pub r#type: VarType,
    pub exp: Box<Expression>,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Assignment {
        from: TypedExp,
        to: TypedExp,
    },

    Binary {
        left: TypedExp,
        operator: parse::BinaryOperator,
        right: TypedExp,
    },
    Cast {
        target: VarType,
        exp: TypedExp,
    },

    //factors
    PostfixIncrement(TypedExp),
    PostfixDecrement(TypedExp),
    PrefixIncrement(TypedExp),
    PrefixDecrement(TypedExp),

    Var(Identifier),
    Const(crate::lex::Constant),
    Unary {
        operator: parse::UnaryOperator,
        operand: TypedExp,
    },
    Nested(TypedExp),

    Conditional {
        condition: TypedExp,
        r#true: TypedExp,
        r#false: TypedExp,
    },
    FunctionCall {
        name: Identifier,
        args: Box<[TypedExp]>,
    },
}

#[derive(Debug)]
pub struct FunctionDeclaration<T> {
    pub name: Identifier,
    pub params: ParamList,
    pub body: Option<Block<T>>,
    pub storage_class: Option<StorageClass>,
}

#[derive(Debug)]
pub enum BlockItem<T> {
    S(Statement<T>),
    D(Declaration<T>),
}

#[derive(Debug)]
pub enum Declaration<T> {
    Function {
        name: Identifier,
        params: ParamList,
        body: Option<Block<T>>,
        storage_class: Option<StorageClass>,
        r#type: FnType,
    },
    Var {
        name: Identifier,
        init: Option<T>,
        storage_class: Option<StorageClass>,
        r#type: VarType,
    },
}

impl From<VariableDeclaration> for Declaration<AstExpression> {
    fn from(
        VariableDeclaration {
            name,
            init,
            storage_class,
            r#type,
        }: VariableDeclaration,
    ) -> Self {
        Self::Var {
            name,
            init,
            storage_class,
            r#type,
        }
    }
}

#[derive(Debug)]
pub enum Statement<T> {
    Ret(T),
    Exp(T),
    If {
        condition: T,
        then: Box<Self>,
        r#else: Option<Box<Self>>,
    },
    Break(LabelId),
    Continue(LabelId),
    While {
        condition: T,
        body: Box<Self>,
        label: LabelId,
    },
    DoWhile {
        body: Box<Self>,
        condition: T,
        label: LabelId,
    },
    For {
        init: Option<ForInit<T>>,
        condition: Option<T>,
        post: Option<T>,
        body: Box<Self>,
        label: LabelId,
    },
    Compound(Block<T>),
    Label {
        name: Label,
        body: Box<Self>,
    },
    Goto(Identifier),
    Switch {
        val: T,
        body: Box<Self>,
        label: LabelId,
        cases: Box<[i32]>,
        default: bool,
    },
    Null,
}

#[derive(Debug)]
pub enum ForInit<T> {
    E(T),
    D {
        name: Identifier,
        initializer: Option<T>,
        r#type: VarType,
        sc: Option<StorageClass>,
    },
}

#[derive(Debug, Clone)]
pub enum Label {
    Named(Identifier),
    Case { val: i32, id: LabelId },
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
