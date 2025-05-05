use ast::c_vals::*;
use ast::typed::prelude::*;
use ast::typed::{Binary, Expr, Unary};
use ast::Key;
use ast::StorageClass;
use ast::{Bop, UnOp};

use std::collections::HashMap;

use std::collections::hash_map::Entry;

pub type SymbolTable<'a> = HashMap<Key<'a>, Attr>;
#[derive(Debug)]
pub enum Attr {
    Static {
        typ: VarType,
        init: Option<InitialVal>,
        global: bool,
    },
    Automatic(VarType),
    Fn {
        defined: bool,
        global: bool,
        typ: FnType,
    },
}

impl Attr {
    pub const fn global(&self) -> bool {
        match self {
            Self::Static { global, .. } | Self::Fn { global, .. } => *global,
            Self::Automatic(_) => false,
        }
    }

    pub const fn var_type(&self) -> Result<&VarType, Error> {
        match self {
            Self::Static { typ, .. } | Self::Automatic(typ) => Ok(typ),
            Self::Fn { .. } => Err(Error::ExpectedVarType),
        }
    }

    pub const fn fn_type(&self) -> Result<&FnType, Error> {
        match self {
            Self::Fn { typ, .. } => Ok(typ),
            Self::Static { .. } | Self::Automatic(_) => Err(Error::ExpectedFnType),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum InitialVal {
    Tentative,
    Initial(StaticInit),
}

impl InitialVal {
    pub const fn get_static(&self, ty: VarType) -> StaticInit {
        match (self, ty) {
            (InitialVal::Initial(s), _) => *s,
            (InitialVal::Tentative, VarType::Int | VarType::UInt) => StaticInit::Int(0),
            (InitialVal::Tentative, VarType::Long | VarType::ULong) => StaticInit::Long(0),
        }
    }

    pub fn as_long(&self) -> i64 {
        match self {
            Self::Initial(StaticInit::Long(i)) => *i,
            Self::Initial(StaticInit::Int(i)) => (*i).into(),
            Self::Tentative => 0,
        }
    }

    pub const fn as_int(&self) -> i32 {
        match self {
            Self::Initial(StaticInit::Int(i)) => *i,
            Self::Initial(StaticInit::Long(i)) => *i as i32,
            Self::Tentative => 0,
        }
    }
}

// check FunctionDeclaration, VariableDeclaration,
use ast::labeled as lab;
pub fn typecheck(p: lab::Program) -> Result<(SymbolTable, Program), Error> {
    let mut table = SymbolTable::new();
    let mut decs = Vec::with_capacity(p.len());
    for dec in p {
        decs.push(top_level_declaration(dec, &mut table)?);
    }

    Ok((table, decs.into()))
}

fn top_level_declaration<'a>(
    dec: lab::Dec<'a>,
    table: &mut SymbolTable<'a>,
) -> Result<Dec<'a>, Error> {
    match dec {
        lab::Dec::Fn(f) => function_declaration(f, table, false).map(Dec::Fn),
        lab::Dec::Var(v) => top_level_var(v, table).map(Dec::Var),
    }
}

fn top_level_var<'a>(
    lab::VarDec {
        name,
        init,
        typ,
        sc,
    }: lab::VarDec<'a>,
    table: &mut SymbolTable<'a>,
) -> Result<VarDec<'a>, Error> {
    let mut initial = if sc == Some(StorageClass::Extern) {
        extern_initializer(&init)?
    } else {
        Some(top_level_initializer(init.as_ref())?)
    };

    // we're global unless static
    let mut global = sc != Some(StorageClass::Static);

    let table_entry = table.get(&name);

    if let Some(Attr::Static {
        init: old_init,
        global: old_global,
        typ: old_type,
    }) = table_entry
    {
        check_linkage(&mut global, *old_global, &sc)?;
        check_initializer_conflict(old_init, &mut initial)?;
        if typ != *old_type {
            return Err(Error::ConflictingType);
        }
    } else if table_entry.is_some() {
        return Err(Error::ConflictingType);
    };

    table.insert(
        name,
        Attr::Static {
            init: initial,
            global,
            typ,
        },
    );

    let init = init
        .map(|init| typecheck_expression(init, table))
        .transpose()?;
    Ok(VarDec {
        name,
        init,
        sc,
        typ,
    })
}

fn extern_initializer(init: &Option<ast::Expr>) -> Result<Option<InitialVal>, Error> {
    init.as_ref()
        .map(|expr| {
            expr.static_init()
                .map(InitialVal::Initial)
                .ok_or(Error::NotConstInitialized)
        })
        .transpose()
}

fn top_level_initializer(init: Option<&ast::Expr>) -> Result<InitialVal, Error> {
    if let Some(init) = init {
        init.static_init()
            .map(InitialVal::Initial)
            .ok_or(Error::NotConstInitialized)
    } else {
        Ok(InitialVal::Tentative)
    }
}

fn check_linkage(
    global: &mut bool,
    old_global: bool,
    sc: &Option<StorageClass>,
) -> Result<(), Error> {
    // if it's extern, then we go with it, otherwise they should be the same
    if *sc == Some(StorageClass::Extern) {
        *global = old_global;
        Ok(())
    } else if *global != old_global {
        Err(Error::ConflictingLinkage)
    } else {
        Ok(())
    }
}

fn check_initializer_conflict(
    old: &Option<InitialVal>,
    new: &mut Option<InitialVal>,
) -> Result<(), Error> {
    match (&new, old) {
        // if they're both declared something's wrong, even if it's the same definition
        (Some(InitialVal::Initial(_)), Some(InitialVal::Initial(_))) => {
            Err(Error::ConflictingDeclaration)
        }
        // if we have an initial pass then we take it
        (Some(InitialVal::Initial(c)), _) | (_, Some(InitialVal::Initial(c))) => {
            *new = Some(InitialVal::Initial(*c));
            Ok(())
        }
        // if we have a tentative we take it
        (Some(InitialVal::Tentative), _) | (_, Some(InitialVal::Tentative)) => {
            *new = Some(InitialVal::Tentative);
            Ok(())
        }
        // otherwise it's none, so we leave it as none
        (None, None) => Ok(()),
    }
}

fn declaration<'a>(dec: lab::Dec<'a>, table: &mut SymbolTable<'a>) -> Result<Dec<'a>, Error> {
    match dec {
        lab::Dec::Fn(f) => function_declaration(f, table, true).map(Dec::Fn),
        lab::Dec::Var(v) => variable_declaration(v, table).map(Dec::Var),
    }
}

fn variable_declaration<'a>(
    lab::VarDec {
        name,
        init,
        typ,
        sc,
    }: lab::VarDec<'a>,
    table: &mut SymbolTable<'a>,
) -> Result<VarDec<'a>, Error> {
    let init = match (sc, &init) {
        (Some(StorageClass::Extern), Some(_)) => return Err(Error::DeclaredExtern),
        (Some(StorageClass::Extern), None) => {
            match table.entry(name) {
                Entry::Occupied(e) => {
                    match e.get() {
                        Attr::Fn { .. } => return Err(Error::FnAsVar),
                        Attr::Static { typ: old_type, .. } | Attr::Automatic(old_type) => {
                            if *old_type != typ {
                                return Err(Error::ConflictingType);
                            }
                        }
                    }
                    if let Attr::Fn { .. } = e.get() {
                        return Err(Error::FnAsVar);
                    }
                }
                Entry::Vacant(e) => {
                    e.insert(Attr::Static {
                        init: None,
                        global: true,
                        typ,
                    });
                }
            }
            None
        }
        (Some(StorageClass::Static), Some(ast::expr::Expr::Const(c))) => {
            table.insert(
                name,
                Attr::Static {
                    init: Some(InitialVal::Initial(StaticInit::from(*c))),
                    global: false,
                    typ,
                },
            );
            Some(Expr::Const { c: *c, ty: c.ty() })
        }
        (Some(StorageClass::Static), Some(_)) => return Err(Error::NotConstInitialized),
        (Some(StorageClass::Static), None) => {
            table.insert(
                name,
                Attr::Static {
                    init: Some(InitialVal::Initial(StaticInit::Long(0))),
                    global: false,
                    typ,
                },
            );

            Some(Expr::Const {
                c: Constant::Long(0),
                ty: VarType::Int,
            })
        }
        (None, _) => {
            table.insert(name, Attr::Automatic(typ));
            if let Some(init) = init {
                let mut exp = typecheck_expression(init, table)?;
                convert_to(&mut exp, &typ);
                Some(exp)
            } else {
                None
            }
        }
    };
    Ok(VarDec {
        name,
        init,
        sc,
        typ,
    })
}

fn check_boxed_expr<'a>(
    expression: ast::Expr<'a>,
    table: &mut SymbolTable<'a>,
) -> Result<Box<Expr<'a>>, Error> {
    typecheck_expression(expression, table).map(Box::new)
}

fn typecheck_expression<'a>(
    expression: ast::Expr<'a>,
    table: &mut SymbolTable<'a>,
) -> Result<Expr<'a>, Error> {
    match expression {
        ast::Expr::FunctionCall { name, args } => typecheck_fn_call(name, args, table),
        ast::Expr::Var(name) => typecheck_var(name, table),
        ast::Expr::Assignment { dst, src } => {
            let dst = check_boxed_expr(*dst, table)?;
            let mut src = check_boxed_expr(*src, table)?;

            let ty = dst.typ();
            convert_to(&mut src, &ty);
            Ok(Expr::Assignment { dst, src, ty })
        }
        ast::Expr::Bin(ast::Binary {
            left,
            right,
            operator,
        }) => {
            // if it's relational or logical and or logical or then it's gonna be int
            let mut left = typecheck_expression(*left, table).map(Box::from)?;
            let mut right = typecheck_expression(*right, table).map(Box::from)?;
            // result of this expression is an int
            let Some(ty) = left.typ().common_type(&right.typ()) else {
                return Err(Error::InvalidCast);
            };
            convert_to(&mut left, &ty);
            convert_to(&mut right, &ty);
            Ok(Expr::Bin(Binary {
                left,
                operator,
                right,
                ty: if operator.relational() || matches!(operator, Bop::LogAnd | Bop::LogOr) {
                    VarType::Int
                } else {
                    ty
                },
            }))
        }
        ast::Expr::Nested(exp) => typecheck_expression(*exp, table),
        ast::Expr::Const(c @ Constant::Int(_)) => Ok(Expr::Const {
            c,
            ty: VarType::Int,
        }),

        ast::Expr::Const(c @ Constant::Long(_)) => Ok(Expr::Const {
            c,
            ty: VarType::Long,
        }),

        ast::Expr::Const(_) => todo!(),

        ast::Expr::Cast { target, exp } => typecheck_expression(*exp, table).map(|e| Expr::Cast {
            ty: target,
            target,
            exp: Box::new(e),
        }),

        ast::Expr::Unary(ast::Unary { exp, op }) => {
            typecheck_expression(*exp, table).map(Box::new).map(|exp| {
                Expr::Unary(Unary {
                    ty: if op == UnOp::Not {
                        VarType::Int
                    } else {
                        exp.typ()
                    },
                    exp,
                    op,
                })
            })
        }

        ast::Expr::Conditional {
            condition,
            r#true,
            r#false,
        } => {
            let condition = check_boxed_expr(*condition, table)?;
            let mut r#true = check_boxed_expr(*r#true, table)?;
            let mut r#false = check_boxed_expr(*r#false, table)?;

            let Some(common) = r#true.typ().common_type(&r#false.r#typ()) else {
                return Err(Error::InvalidCast);
            };

            convert_to(&mut r#true, &common);
            convert_to(&mut r#false, &common);

            Ok(Expr::Conditional {
                ty: common,
                condition,
                r#true,
                r#false,
            })
        }
        ast::Expr::IncDec { op, exp } => {
            //Expression::PostfixIncrement(
            let exp = check_boxed_expr(*exp, table)?;
            Ok(Expr::IncDec {
                op,
                ty: exp.typ(),
                exp,
            })
        }
    }
}

fn convert_to(exp: &mut Expr, ty: &VarType) {
    if &exp.typ() != ty {
        *exp = Expr::Cast {
            ty: *ty,
            target: *ty,
            exp: Box::from(exp.clone()),
        };
    }
}

fn typecheck_fn_call<'a>(
    name: Key<'a>,
    args: Box<[ast::Expr<'a>]>,
    table: &mut SymbolTable<'a>,
) -> Result<Expr<'a>, Error> {
    // cloning for now, need to find a nicer way to do this
    let Some(typ) = table.get(&name) else {
        return Err(Error::UndefinedFn);
    };

    let FnType { ret, params } = typ.fn_type()?.clone();

    if params.len() != args.len() {
        Err(Error::WrongArgs)
    } else {
        let mut new_args = Vec::with_capacity(args.len());
        for (param, arg) in args.into_iter().zip(params) {
            let mut typed_param = typecheck_expression(param, table)?;
            convert_to(&mut typed_param, &arg);
            new_args.push(typed_param);
        }

        let ty = if let Some(ty) = ret { ty } else { VarType::Int };

        Ok(Expr::FunctionCall {
            ty,
            name,
            args: new_args.into(),
        })
    }
}

fn typecheck_var<'a>(key: Key<'a>, table: &mut SymbolTable<'a>) -> Result<Expr<'a>, Error> {
    let ty = *table
        .get(&key)
        .map(Attr::var_type)
        .ok_or(Error::UndefinedVar)??;
    Ok(Expr::Var { ty, key })
}

fn check_entry(
    entry: &Entry<Key, Attr>,
    new_params: &ParamList,
    has_body: bool,
) -> Result<(), Error> {
    let Entry::Occupied(e) = entry else {
        return Ok(());
    };
    let Attr::Fn {
        defined,
        global: _,
        typ: FnType { ret: _, params },
    } = e.get()
    else {
        return Err(Error::ConflictingType);
    };

    if *defined && has_body {
        Err(Error::DuplicateDefinition)
    } else {
        param_typecheck(new_params, params)
    }
}

fn param_typecheck(new: &ParamList, old: &[VarType]) -> Result<(), Error> {
    if new.len() != old.len() {
        return Err(Error::WrongParams);
    }
    let new_iter = new.iter().map(|param| param.typ);
    for (new, &old) in new_iter.zip(old) {
        if new != old {
            return Err(Error::WrongParams);
        }
    }
    Ok(())
}

fn function_declaration<'a>(
    lab::FnDec {
        name,
        body,
        params,
        typ,
        mut sc,
    }: lab::FnDec<'a>,
    table: &mut SymbolTable<'a>,
    block_scope: bool,
) -> Result<FnDec<'a>, Error> {
    let global = sc.is_none_or(|sc| sc == StorageClass::Extern);

    let has_body = body.is_some();

    let entry = table.entry(name);

    check_entry(&entry, &params, has_body)?;

    match entry {
        Entry::Occupied(mut e) if !block_scope => {
            let Attr::Fn {
                defined,
                global: was_global,
                r#typ: _,
            } = e.get_mut()
            else {
                unreachable!()
            };

            if has_body {
                *defined = true;
            }
            /*
             * a function declared without the 'static'
             * keyword always has external linkage
             */

            /*
             * Can't define a symbol with external linkage,
             * then redefine it with internal linkage
             */

            match (&sc, was_global) {
                // is static now, was previously extern or none
                // should conflict but we'll keep it
                (Some(StorageClass::Static), true) => {
                    return Err(Error::StaticRedec);
                }
                // is static now, was previously static
                (Some(StorageClass::Static), false) => {}
                // this is fine
                (Some(StorageClass::Extern), true) => {}
                // this conflicts
                (Some(StorageClass::Extern), false) => {}
                //
                (None, true) => {
                    // (No visibility qualitifer, previously declared (maybe as extern)

                    // This is fine is fine
                }
                (None, false) => {
                    sc = Some(StorageClass::Static);
                    // (No visibility qualitifer, but was previously declared as static
                }
            };
        }
        Entry::Occupied(_) => {}
        Entry::Vacant(e) => {
            e.insert(Attr::Fn {
                typ: typ.clone(),
                defined: has_body,
                global,
            });
        }
    };

    for param in params.iter() {
        table.insert(param.name, Attr::Automatic(param.typ));
    }

    let body = if let Some(body) = body {
        let mut new_body = Vec::new();
        for item in body {
            new_body.push(typecheck_blockitem(item, typ.ret, table)?);
        }
        Some(new_body.into_boxed_slice())
    } else {
        None
    };
    Ok(FnDec {
        name,
        params,
        body,
        sc,
        typ,
    })
}

fn typecheck_blockitem<'a>(
    block_item: lab::BlockItem<'a>,
    ret: Option<VarType>,
    table: &mut SymbolTable<'a>,
) -> Result<BlockItem<'a>, Error> {
    match block_item {
        lab::BlockItem::D(dec) => declaration(dec, table).map(BlockItem::D),
        lab::BlockItem::S(s) => typecheck_statement(s, ret, table).map(BlockItem::S),
    }
}

fn typecheck_statement<'a>(
    stmt: lab::Stmnt<'a>,
    return_type: Option<VarType>,
    table: &mut SymbolTable<'a>,
) -> Result<Stmnt<'a>, Error> {
    match stmt {
        lab::Stmnt::Compound(stmts) => {
            let mut statements = Vec::with_capacity(stmts.len());
            for item in stmts {
                statements.push(typecheck_blockitem(item, return_type, table)?);
            }
            let statements = statements.into_boxed_slice();

            Ok(Stmnt::Compound(statements))
        }
        lab::Stmnt::Exp(e) => typecheck_expression(e, table).map(Stmnt::Exp),
        lab::Stmnt::If {
            condition,
            then,
            r#else,
        } => typecheck_expression(condition, table).and_then(|condition| {
            typecheck_statement(*then, return_type, table)
                .map(Box::new)
                .and_then(|then| {
                    r#else
                        .map(|r#else| {
                            typecheck_statement(*r#else, return_type, table).map(Box::new)
                        })
                        .transpose()
                        .map(|r#else| Stmnt::If {
                            condition,
                            then,
                            r#else,
                        })
                })
        }),
        lab::Stmnt::DoWhile {
            body,
            condition,
            label,
        } => {
            let body = typecheck_statement(*body, return_type, table)?;
            let condition = typecheck_expression(condition, table)?;
            Ok(Stmnt::DoWhile {
                body: body.into(),
                condition,
                label,
            })
        }
        lab::Stmnt::While {
            body,
            condition,
            label,
        } => {
            let body = typecheck_statement(*body, return_type, table)?;
            let condition = typecheck_expression(condition, table)?;
            Ok(Stmnt::While {
                body: body.into(),
                condition,
                label,
            })
        }

        lab::Stmnt::NamedLabel { body, l } => Ok(Stmnt::NamedLabel {
            l,
            body: typecheck_statement(*body, return_type, table)?.into(),
        }),

        lab::Stmnt::Case { case, stmnt } => Ok(Stmnt::Case {
            case,
            stmnt: label_stmnt(stmnt, return_type, table)?,
        }),

        lab::Stmnt::Default(stmnt) => label_stmnt(stmnt, return_type, table).map(Stmnt::Default),

        lab::Stmnt::For {
            init,
            condition,
            post,
            body,
            label,
        } => {
            let init = init
                .map(|init| match *init {
                    lab::ForInit::D(v) => variable_declaration(v, table).map(ForInit::D),

                    lab::ForInit::E(e) => typecheck_expression(e, table).map(ForInit::E),
                })
                .transpose()?
                .map(Box::new);

            let condition = condition
                .map(|c| typecheck_expression(c, table))
                .transpose()?;
            let post = post.map(|p| typecheck_expression(p, table)).transpose()?;

            let body = Box::new(typecheck_statement(*body, return_type, table)?);
            Ok(Stmnt::For {
                init,
                condition,
                post,
                body,
                label,
            })
        }
        lab::Stmnt::Ret(e) => {
            if let Some(return_type) = return_type {
                let mut r = typecheck_expression(e, table)?;
                convert_to(&mut r, &return_type);
                Ok(Stmnt::Ret(r))
            } else {
                panic!()
            }
        }
        lab::Stmnt::Switch {
            val: v,
            body: b,
            label,
            cases,
            default,
        } => Ok(Stmnt::Switch {
            val: typecheck_expression(v, table)?,
            body: Box::new(typecheck_statement(*b, return_type, table)?),
            label,
            cases,
            default,
        }),

        lab::Stmnt::Null => Ok(Stmnt::Null),
        lab::Stmnt::Goto(g) => Ok(Stmnt::Goto(g)),
        lab::Stmnt::Break(l) => Ok(Stmnt::Break(l)),
        lab::Stmnt::Continue(c) => Ok(Stmnt::Continue(c)),
    }
}

fn label_stmnt<'a>(
    lab::LabelStmnt { id, body }: lab::LabelStmnt<'a>,
    r: Option<VarType>,
    tbl: &mut SymbolTable<'a>,
) -> Result<LabelStmnt<'a>, Error> {
    typecheck_statement(*body, r, tbl).map(|body| LabelStmnt {
        label: id,
        body: body.into(),
    })
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Duplicate Definition")]
    DuplicateDefinition,

    #[error("Conflicting Declaration")]
    ConflictingDeclaration,

    #[error("Undefined Variable")]
    UndefinedVar,

    #[error("Undefined Function")]
    UndefinedFn,

    #[error("Invalid Parameters")]
    WrongParams,

    #[error("Function used as Variable")]
    FnAsVar,

    #[error("Variable used as Function")]
    VarAsFn,

    #[error("Invalid Argument Types")]
    WrongArgs,

    #[error("Conflicting Type Usage")]
    ConflictingType,

    #[error("Static Global")]
    StaticGlobal,

    #[error("Static Not Const Initialized")]
    NotConstInitialized,

    #[error("Conflicting Linkage")]
    ConflictingLinkage,

    #[error("Declared Extern")]
    DeclaredExtern,

    #[error("Static Redeclared")]
    StaticRedec,

    #[error("Invalid Cast")]
    InvalidCast,

    #[error("Expected Variable Type")]
    ExpectedVarType,

    #[error("Expected Function Type")]
    ExpectedFnType,
}
