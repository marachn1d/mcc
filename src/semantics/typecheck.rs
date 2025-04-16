use super::ast::label_prelude as ast;
use super::ast::type_prelude::*;
use crate::parse::Bop;
use crate::semantics::StorageClass;

use crate::parse::{FnType, VarType};
use std::collections::HashMap;

use std::collections::hash_map::Entry;

pub type SymbolTable = HashMap<Identifier, Attr>;
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

use crate::lex::Constant;

#[derive(Debug, Copy, Clone)]
pub enum InitialVal {
    Tentative,
    Initial(StaticInit),
}

#[derive(Debug, Copy, Clone)]
pub enum StaticInit {
    Int(i32),
    Long(i64),
}

impl std::fmt::Display for StaticInit {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Int(0) => f.write_str(".zero 4"),
            Self::Long(0) => f.write_str(".zero 8"),
            Self::Int(i) => write!(f, ".long {i}"),
            Self::Long(i) => write!(f, ".quad {i}"),
        }
    }
}

impl From<Constant> for StaticInit {
    fn from(c: Constant) -> Self {
        match c {
            Constant::Int(i) => Self::Int(i),

            Constant::Long(i) => Self::Long(i),
        }
    }
}

impl InitialVal {
    pub fn get_static(&self) -> StaticInit {
        match self {
            InitialVal::Initial(s) => *s,
            InitialVal::Tentative => StaticInit::Int(0),
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

pub fn typecheck(p: ast::Program) -> Result<(SymbolTable, Program), Error> {
    let mut table = SymbolTable::new();
    let mut decs = Vec::with_capacity(p.len());
    for dec in p {
        decs.push(top_level_declaration(dec, &mut table)?);
    }

    Ok((table, decs.into()))
}

fn top_level_declaration(dec: ast::Dec, table: &mut SymbolTable) -> Result<Dec, Error> {
    match dec {
        ast::Dec::Fn(f) => function_declaration(f, table, false).map(Dec::Fn),
        ast::Dec::Var(v) => top_level_var(v, table).map(Dec::Var),
    }
}

fn top_level_var(
    ast::VarDec {
        name,
        init,
        typ,
        sc,
    }: ast::VarDec,
    table: &mut SymbolTable,
) -> Result<VarDec, Error> {
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
        name.clone(),
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

fn declaration(dec: ast::Dec, table: &mut SymbolTable) -> Result<Dec, Error> {
    match dec {
        ast::Dec::Fn(f) => function_declaration(f, table, true).map(Dec::Fn),
        ast::Dec::Var(v) => variable_declaration(v, table).map(Dec::Var),
    }
}

fn variable_declaration(
    ast::VarDec {
        name,
        init,
        typ,
        sc,
    }: ast::VarDec,
    table: &mut SymbolTable,
) -> Result<VarDec, Error> {
    let init = match (sc, &init) {
        (Some(StorageClass::Extern), Some(_)) => return Err(Error::DeclaredExtern),
        (Some(StorageClass::Extern), None) => {
            match table.entry(name.clone()) {
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
        (Some(StorageClass::Static), Some(ast::Expr::Const(c))) => {
            table.insert(
                name.clone(),
                Attr::Static {
                    init: Some(InitialVal::Initial(StaticInit::from(*c))),
                    global: false,
                    typ,
                },
            );
            Some(Expr::Const {
                cnst: *c,
                ty: VarType::Int,
            })
        }
        (Some(StorageClass::Static), Some(_)) => return Err(Error::NotConstInitialized),
        (Some(StorageClass::Static), None) => {
            table.insert(
                name.clone(),
                Attr::Static {
                    init: Some(InitialVal::Initial(StaticInit::Long(0))),
                    global: false,
                    typ,
                },
            );

            Some(Expr::Const {
                cnst: Constant::Long(0),
                ty: VarType::Int,
            })
        }
        (None, _) => {
            table.insert(name.clone(), Attr::Automatic(typ));
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

fn check_boxed_expr(expression: ast::Expr, table: &mut SymbolTable) -> Result<Box<Expr>, Error> {
    typecheck_expression(expression, table).map(Box::new)
}

fn typecheck_expression(expression: ast::Expr, table: &mut SymbolTable) -> Result<Expr, Error> {
    match expression {
        ast::Expr::FunctionCall { name, args } => typecheck_fn_call(name, args, table),
        ast::Expr::Var(name) => typecheck_var(name, table),
        ast::Expr::Assignment { dst, src } => {
            let dst = check_boxed_expr(*dst, table)?;
            let mut src = check_boxed_expr(*src, table)?;

            let ty = dst.ty();
            convert_to(&mut src, &dst.ty());
            Ok(Expr::Assignment { dst, src, ty })
        }
        ast::Expr::Binary {
            left,
            right,
            operator: op @ (Bop::LeftShift | Bop::RightShift),
        } => {
            let left = typecheck_expression(*left, table).map(Box::from)?;
            let mut right = typecheck_expression(*right, table).map(Box::from)?;
            convert_to(&mut right, &VarType::Int);
            Ok(Expr::Binary {
                left,
                operator: op,
                right,
                ty: VarType::Int,
            })
        }
        ast::Expr::Binary {
            left,
            right,
            operator,
        } => {
            use crate::parse::Bop;
            let mut left = typecheck_expression(*left, table).map(Box::from)?;
            let mut right = typecheck_expression(*right, table).map(Box::from)?;
            if matches!(operator, Bop::LogAnd | Bop::LogOr) {
                Ok(Expr::Binary {
                    left,
                    operator,
                    right,
                    ty: VarType::Int,
                })
            } else if let Some(common) = left.ty().common_type(&right.ty()) {
                convert_to(&mut left, &common);
                convert_to(&mut right, &common);
                let ty = if operator.relational() {
                    VarType::Int
                } else {
                    common
                };
                Ok(Expr::Binary {
                    operator,
                    left,
                    right,
                    ty,
                })
            } else {
                Err(Error::InvalidCast)
            }
        }
        /*
        AstExpression::PostfixIncrement(exp)
        | AstExpression::PostfixDecrement(exp)
        | AstExpression::PrefixIncrement(exp)
        | AstExpression::PrefixDecrement(exp)
        */
        ast::Expr::Nested(exp) => typecheck_expression(*exp, table),
        ast::Expr::Const(cnst @ Constant::Int(_)) => Ok(Expr::Const {
            cnst,
            ty: VarType::Int,
        }),

        ast::Expr::Const(cnst @ Constant::Long(_)) => Ok(Expr::Const {
            cnst,
            ty: VarType::Long,
        }),

        ast::Expr::Cast { target, exp } => typecheck_expression(*exp, table).map(|e| Expr::Cast {
            ty: target,
            target,
            exp: Box::new(e),
        }),

        ast::Expr::Unary { operator, operand } => {
            use crate::parse::UnOp;
            typecheck_expression(*operand, table)
                .map(Box::new)
                .map(|operand| Expr::Unary {
                    ty: if operator == UnOp::Not {
                        VarType::Int
                    } else {
                        operand.ty()
                    },
                    operand,
                    operator,
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

            let Some(common) = r#true.ty().common_type(&r#false.r#ty()) else {
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
                ty: exp.ty(),
                exp,
            })
        }
    }
}

fn convert_to(exp: &mut Expr, ty: &VarType) {
    if &exp.ty() != ty {
        *exp = Expr::Cast {
            ty: *ty,
            target: *ty,
            exp: Box::from(exp.clone()),
        };
    }
}

fn typecheck_fn_call(
    name: Identifier,
    args: Box<[ast::Expr]>,
    table: &mut SymbolTable,
) -> Result<Expr, Error> {
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

fn typecheck_var(name: Identifier, table: &mut SymbolTable) -> Result<Expr, Error> {
    let ty = *table
        .get(&name)
        .map(Attr::var_type)
        .ok_or(Error::UndefinedVar)??;
    Ok(Expr::Var { ty, name })
}

fn check_entry(
    entry: &Entry<Identifier, Attr>,
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

fn function_declaration(
    ast::FnDec {
        name,
        body,
        params,
        typ,
        mut sc,
    }: ast::FnDec,
    table: &mut SymbolTable,
    block_scope: bool,
) -> Result<FnDec, Error> {
    let global = sc.is_none_or(|sc| sc == StorageClass::Extern);

    let has_body = body.is_some();

    let entry = table.entry(name.clone());

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
        table.insert(param.name.clone(), Attr::Automatic(param.typ));
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

fn typecheck_blockitem(
    block_item: ast::BlockItem,
    ret: Option<VarType>,
    table: &mut SymbolTable,
) -> Result<BlockItem, Error> {
    match block_item {
        ast::BlockItem::D(dec) => declaration(dec, table).map(BlockItem::D),
        ast::BlockItem::S(s) => typecheck_statement(s, ret, table).map(BlockItem::S),
    }
}

fn typecheck_statement(
    stmt: ast::Stmnt,
    return_type: Option<VarType>,
    table: &mut SymbolTable,
) -> Result<Stmnt, Error> {
    match stmt {
        ast::Stmnt::Compound(stmts) => {
            let mut statements = Vec::with_capacity(stmts.len());
            for item in stmts {
                statements.push(typecheck_blockitem(item, return_type, table)?);
            }
            let statements = statements.into_boxed_slice();

            Ok(Stmnt::Compound(statements))
        }
        ast::Stmnt::Exp(e) => typecheck_expression(e, table).map(Stmnt::Exp),
        ast::Stmnt::If {
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
        ast::Stmnt::DoWhile {
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
        ast::Stmnt::While {
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
        ast::Stmnt::Label { body, name } => Ok(Stmnt::Label {
            name,
            body: typecheck_statement(*body, return_type, table)?.into(),
        }),
        ast::Stmnt::For {
            init,
            condition,
            post,
            body,
            label,
        } => {
            let init = init
                .map(|init| match *init {
                    ast::ForInit::D(v) => variable_declaration(v, table).map(ForInit::D),

                    ast::ForInit::E(e) => typecheck_expression(e, table).map(ForInit::E),
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
        ast::Stmnt::Ret(e) => {
            if let Some(return_type) = return_type {
                let mut r = typecheck_expression(e, table)?;
                convert_to(&mut r, &return_type);
                Ok(Stmnt::Ret(r))
            } else {
                panic!()
            }
        }
        ast::Stmnt::Switch {
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

        ast::Stmnt::Null => Ok(Stmnt::Null),
        ast::Stmnt::Goto(g) => Ok(Stmnt::Goto(g)),
        ast::Stmnt::Break(l) => Ok(Stmnt::Break(l)),
        ast::Stmnt::Continue(c) => Ok(Stmnt::Continue(c)),
    }
}

#[derive(Debug)]
pub enum Error {
    DuplicateDefinition,
    ConflictingDeclaration,

    UndefinedVar,

    UndefinedFn,

    WrongParams,

    FnAsVar,
    VarAsFn,
    WrongArgs,
    ConflictingType,
    StaticGlobal,

    NotConstInitialized,
    ConflictingLinkage,
    DeclaredExtern,
    StaticRedec,
    InvalidCast,
    ExpectedVarType,
    ExpectedFnType,
}
