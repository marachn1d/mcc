use crate::lex::Identifier;
use crate::parse;
use parse::Label;
type Array<T> = Box<[T]>;

struct Program<F, V>(Array<Declaration<F, V>>);

enum Declaration<F, V> {
    Function(F),
    Var(V),
}

struct Parameter<V, I> {
    pub r#type: V,
    pub name: I,
}

type ParamList<V, I> = Array<Parameter<V, I>>;

/*
#[derive(Debug)]
pub enum Declaration {
    Function {
        name: Identifier,
        params: ParamList,
        body: Option<Block>,
        storage_class: Option<StorageClass>,
        r#type: FnType,
    },
    Var {
        name: Identifier,
        init: Option<Expression>,
        storage_class: Option<StorageClass>,
        r#type: VarType,
    },
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub r#type: VarType,
    pub name: Identifier,
    pub last: bool,
}
*/

type Block<S, D> = Array<BlockItem<S, D>>;

enum BlockItem<S, D> {
    Statement(S),
    Declaration(D),
}

enum ForInit<E, D> {
    Dec(D),
    Exp(E),
}

enum Statement<Exp, FnDec, VarDec, Extra> {
    Ret {
        exp: Exp,
        extra: Extra,
    },
    Exp {
        exp: Exp,
        extra: Extra,
    },
    If {
        condition: Exp,
        then: Box<Self>,
        r#else: Option<Box<Self>>,
        extra: Extra,
    },
    Break(Extra),
    Continue(Extra),
    While {
        condition: Exp,
        body: Box<Self>,
        extra: Extra,
    },
    DoWhile {
        body: Box<Self>,
        condition: Exp,
        extra: Extra,
    },
    For {
        init: Option<ForInit<VarDec, Exp>>,
        condition: Option<Exp>,
        post: Option<Exp>,
        body: Box<Self>,
        extra: Extra,
    },
    Compound {
        block: Block<Self, Declaration<FnDec, VarDec>>,
        extra: Extra,
    },
    Label {
        label: Label,
        body: Box<Self>,
        extra: Extra,
    },
    Goto {
        target: Identifier,
        extra: Extra,
    },
    Null(Extra),
    Switch {
        val: Exp,
        body: Box<Self>,
        extra: Extra,
    },
}

impl<V, F, C, I, D, E> Statement<Expression<V, C, I>, F, D, E> {
    fn map_expr(
        &mut self,
        mut f: impl FnMut(&mut Expression<V, C, I>) -> Result<(), E>,
    ) -> Result<(), E> {
        match self {
            Self::Ret { exp, .. } | Self::Exp { exp, .. } => f(exp),
            Self::If {
                condition,
                then,
                r#else,
                ..
            } => {
                f(condition)?;
                if let Some(r#else) = r#else {
                    r#else.map_expr(&mut f)?;
                }
                then.map_expr(&mut f)
            }
            Self::Break(_) | Self::Continue(_) => Ok(()),
            Self::While {
                condition, body, ..
            }
            | Self::DoWhile {
                body, condition, ..
            } => {
                f(condition)?;
                body.map_expr(&mut f)
            }
            Self::For {
                init,
                condition,
                post,
                body,
                ..
            } => {
                match init {
                    None => Ok(()),
                    Some(ForInit::Dec(Declaration::Function(f))) => {}
                    Some(ForInit::Dec(Declaration::Var(v))) => {}
                    Some(ForInit::Exp(e)) => f(e),
                }?;

                Ok(())
            }
            Self::Compound { block, .. } => todo!(),
            Self::Label { body, .. } => todo!(),
            Self::Goto { .. } => Ok(()),
            Self::Null(_) => Ok(()),
            Self::Switch { val, body, .. } => todo!(),
        }
    }
}

pub type Expr = Expression<parse::VarType, crate::lex::Constant, Identifier>;

impl<V, C, I> Expression<V, C, I> {
    fn map<E>(&mut self, mut f: impl FnMut(&mut Self) -> Result<bool, E>) -> Result<(), E> {
        if f(self)? {
            Ok(())
        } else {
            match self {
                Self::Assignment(s) => {
                    f(&mut s.0)?;
                    f(&mut s.1)?;
                }
                Self::Binary { left, right, op: _ } => {
                    f(left)?;
                    f(right)?;
                }
                Self::Unary { operand: exp, .. } | Self::Nested(exp) | Self::Cast { exp, .. } => {
                    f(exp)?;
                }
                Self::IncDec { .. } | Self::Var(_) => {}
                Self::Conditional {
                    condition,
                    r#true,
                    r#false,
                } => {
                    f(condition)?;
                    f(r#true)?;
                    f(r#false)?;
                }
                Self::FunctionCall { args, .. } => {
                    for arg in args {
                        f(arg)?;
                    }
                }
                Self::Const(_) => {}
            };

            Ok(())
        }
    }
    fn transform<E, X, Y, Z>(
        self,
        mut f_v: impl FnMut(V) -> Result<X, E>,
        mut f_c: impl FnMut(C) -> Result<Y, E>,
        mut f_i: impl FnMut(I) -> Result<Z, E>,
    ) -> Result<Expression<X, Y, Z>, E> {
        let mut transform = |x: Self| x.transform(&mut f_v, &mut f_c, &mut f_i);

        let mut box_transform = |x: Box<Self>| transform(*x).map(Box::new);

        Ok(match self {
            Self::Assignment(s) => {
                Expression::Assignment(Box::new((transform(s.0)?, transform(s.1)?)))
            }
            Self::Binary { left, right, op } => Expression::Binary {
                left: box_transform(left)?,
                right: box_transform(right)?,
                op,
            },
            Self::Unary { operand, operator } => Expression::Unary {
                operator,
                operand: box_transform(operand)?,
            },

            Self::Nested(exp) => Expression::Nested(box_transform(exp)?),
            Self::Cast { exp, target } => Expression::Cast {
                exp: box_transform(exp)?,
                target: f_v(target)?,
            },
            Self::IncDec { fix, op, exp } => Expression::IncDec {
                fix,
                op,
                exp: box_transform(exp)?,
            },
            Self::Var(name) => Expression::Var(f_i(name)?),
            Self::Conditional {
                condition,
                r#true,
                r#false,
            } => Expression::Conditional {
                condition: box_transform(condition)?,
                r#true: box_transform(r#true)?,
                r#false: box_transform(r#false)?,
            },
            Self::FunctionCall { args, name } => {
                let mut vec = Vec::with_capacity(args.len());
                for arg in args {
                    vec.push(transform(arg)?);
                }
                Expression::FunctionCall {
                    name: f_i(name)?,
                    args: vec.into_boxed_slice(),
                }
            }
            Self::Const(c) => Expression::Const(f_c(c)?),
        })
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Fixedness {
    Pre,
    Post,
}

#[derive(Copy, Clone, Debug)]
pub enum IncDecOp {
    Inc,
    Dec,
}

#[derive(Debug, Clone)]
pub enum Expression<VarType, Constant, Identifier> {
    Assignment(Box<(Self, Self)>),
    Binary {
        left: Box<Self>,
        right: Box<Self>,
        op: crate::parse::BinaryOperator,
    },
    Cast {
        target: VarType,
        exp: Box<Self>,
    },

    IncDec {
        fix: Fixedness,
        op: IncDecOp,
        exp: Box<Self>,
    },

    Var(Identifier),
    Const(Constant),
    Unary {
        operator: crate::parse::UnaryOperator,
        operand: Box<Self>,
    },
    Nested(Box<Self>),

    Conditional {
        condition: Box<Self>,
        r#true: Box<Self>,
        r#false: Box<Self>,
    },
    FunctionCall {
        name: Identifier,
        args: Box<[Self]>,
    },
}
