use ast::parse::StorageClass;
use ast::parse::{Bop, ParamList};
use ast::semantics::labeled;
use ast::semantics::typed::{BlockItem, Dec, Expr, FnDec, ForInit, Program, Stmnt, VarDec};
use ast::semantics::{Attr, InitialVal, SymbolTable};
use ast::{parse::FnType, parse::StaticInit, Ident, VarType};

use std::collections::hash_map::Entry;
use std::mem::MaybeUninit;

// check FunctionDeclaration, VariableDeclaration,

pub fn typecheck(p: labeled::Program) -> Result<(SymbolTable, Program), Error> {
    let mut table = SymbolTable::new();
    let mut decs = Vec::with_capacity(p.len());
    for dec in p {
        decs.push(top_level_declaration(dec, &mut table)?);
    }

    Ok((table, decs.into()))
}

fn top_level_declaration(dec: labeled::Dec, table: &mut SymbolTable) -> Result<Dec, Error> {
    match dec {
        labeled::Dec::Fn(f) => function_declaration(f, table, false).map(Dec::Fn),
        labeled::Dec::Var(v) => top_level_var(v, table).map(Dec::Var),
    }
}

fn top_level_var(
    labeled::VarDec {
        name,
        init,
        typ,
        sc,
    }: labeled::VarDec,
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

fn extern_initializer(init: &Option<labeled::Expr>) -> Result<Option<InitialVal>, Error> {
    init.as_ref()
        .map(|expr| {
            expr.static_init()
                .map(InitialVal::Initial)
                .ok_or(Error::NotConstInitialized)
        })
        .transpose()
}

fn top_level_initializer(init: Option<&labeled::Expr>) -> Result<InitialVal, Error> {
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

fn declaration(dec: labeled::Dec, table: &mut SymbolTable) -> Result<Dec, Error> {
    match dec {
        labeled::Dec::Fn(f) => function_declaration(f, table, true).map(Dec::Fn),
        labeled::Dec::Var(v) => variable_declaration(v, table).map(Dec::Var),
    }
}

fn variable_declaration(
    labeled::VarDec {
        name,
        init,
        typ,
        sc,
    }: labeled::VarDec,
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
        (Some(StorageClass::Static), exp) => {
            let init = match exp {
                Some(labeled::Expr::Const(c)) => c.static_init().convert_to(&typ),
                None => StaticInit::zeroed(typ),
                Some(_) => return Err(Error::NotConstInitialized),
            };
            table.insert(
                name.clone(),
                Attr::Static {
                    init: Some(InitialVal::Initial(init)),
                    global: false,
                    typ,
                },
            );
            Some(Expr::Const {
                cnst: init.into(),
                ty: typ,
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

fn check_boxed_expr(
    expression: labeled::Expr,
    table: &mut SymbolTable,
) -> Result<Box<Expr>, Error> {
    typecheck_expression(expression, table).map(Box::new)
}

fn typecheck_expression(expression: labeled::Expr, table: &mut SymbolTable) -> Result<Expr, Error> {
    match expression {
        labeled::Expr::FunctionCall { name, args } => typecheck_fn_call(name, args, table),
        labeled::Expr::Var(name) => typecheck_var(name, table),
        labeled::Expr::Assignment { dst, src } => {
            let dst = check_boxed_expr(*dst, table)?;
            let mut src = check_boxed_expr(*src, table)?;

            let ty = dst.ty();
            convert_to(&mut src, &dst.ty());
            Ok(Expr::Assignment { dst, src, ty })
        }
        labeled::Expr::Binary {
            left,
            right,
            operator,
        } => {
            // if it's relational or logical and or logical or then it's gonna be int
            let mut left = typecheck_expression(*left, table).map(Box::from)?;
            let mut right = typecheck_expression(*right, table).map(Box::from)?;

            // if it's a bitshift, then it's the left's type, if it's a boolean operator, it's an
            // int,
            let ty = if operator.bitshift() {
                left.ty()
            } else {
                left.ty()
                    .common_type(&right.ty())
                    .ok_or(Error::InvalidCast)?
            };

            let mut lhs = MaybeUninit::uninit();

            if operator.compound() {
                lhs.write(left.clone());
            }
            convert_to(&mut left, &ty);
            convert_to(&mut right, &ty);

            if operator.compound() {
                // for compound operators, we have to convert the inner binary operator first, and
                // then convert it to the left side's type
                // if it's a compound operator, we've already confirmed that left is an lvalue, so we
                // don't have to call typecheck_expression on it

                let lhs = unsafe { lhs.assume_init() };
                let mut bin = Expr::Binary {
                    left,
                    operator: operator.de_compound().unwrap(),
                    right,
                    ty,
                };
                convert_to(&mut bin, &lhs.ty());
                Ok(Expr::Assignment {
                    ty: lhs.ty(),
                    dst: lhs,
                    src: Box::new(bin),
                })
            } else {
                let ty = if operator.relational() || matches!(operator, Bop::LogAnd | Bop::LogOr) {
                    VarType::INT
                } else {
                    ty
                };

                Ok(Expr::Binary {
                    left,
                    operator,
                    right,
                    ty,
                })
            }
        }
        labeled::Expr::Nested(exp) => typecheck_expression(*exp, table),
        labeled::Expr::Const(cnst) => Ok(Expr::Const {
            cnst,
            ty: cnst.ty(),
        }),

        labeled::Expr::Cast { target, exp } => {
            typecheck_expression(*exp, table).map(|e| Expr::Cast {
                ty: target,
                target,
                exp: Box::new(e),
            })
        }

        labeled::Expr::Unary { operator, operand } => {
            use crate::parse::UnOp;
            typecheck_expression(*operand, table)
                .map(Box::new)
                .map(|operand| Expr::Unary {
                    ty: if operator == UnOp::Not {
                        VarType::INT
                    } else {
                        operand.ty()
                    },
                    operand,
                    operator,
                })
        }

        labeled::Expr::Conditional {
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
        labeled::Expr::IncDec { op, exp } => {
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
    name: Ident,
    args: Box<[labeled::Expr]>,
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

        let ty = if let Some(ty) = ret { ty } else { VarType::INT };

        Ok(Expr::FunctionCall {
            ty,
            name,
            args: new_args.into(),
        })
    }
}

fn typecheck_var(name: Ident, table: &mut SymbolTable) -> Result<Expr, Error> {
    let ty = *table
        .get(&name)
        .map(Attr::var_type)
        .ok_or(Error::UndefinedVar)??;
    Ok(Expr::Var { ty, name })
}

fn check_entry(
    entry: &Entry<Ident, Attr>,
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
    labeled::FnDec {
        name,
        body,
        params,
        typ,
        mut sc,
    }: labeled::FnDec,
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
            new_body.push(typecheck_blockitem(item, typ.ret, table, None)?);
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
    block_item: labeled::BlockItem,
    ret: Option<VarType>,
    table: &mut SymbolTable,
    switch_val_type: Option<VarType>,
) -> Result<BlockItem, Error> {
    match block_item {
        labeled::BlockItem::D(dec) => declaration(dec, table).map(BlockItem::D),
        labeled::BlockItem::S(s) => {
            typecheck_statement(s, ret, table, switch_val_type).map(BlockItem::S)
        }
    }
}

fn typecheck_statement(
    stmt: labeled::Stmnt,
    return_type: Option<VarType>,
    table: &mut SymbolTable,
    switch_val_type: Option<VarType>,
) -> Result<Stmnt, Error> {
    match stmt {
        labeled::Stmnt::Compound(stmts) => {
            let mut statements = Vec::with_capacity(stmts.len());
            for item in stmts {
                statements.push(typecheck_blockitem(
                    item,
                    return_type,
                    table,
                    switch_val_type,
                )?);
            }
            let statements = statements.into_boxed_slice();

            Ok(Stmnt::Compound(statements))
        }
        labeled::Stmnt::Exp(e) => typecheck_expression(e, table).map(Stmnt::Exp),
        labeled::Stmnt::If {
            condition,
            then,
            r#else,
        } => typecheck_expression(condition, table).and_then(|condition| {
            typecheck_statement(*then, return_type, table, switch_val_type)
                .map(Box::new)
                .and_then(|then| {
                    r#else
                        .map(|r#else| {
                            typecheck_statement(*r#else, return_type, table, switch_val_type)
                                .map(Box::new)
                        })
                        .transpose()
                        .map(|r#else| Stmnt::If {
                            condition,
                            then,
                            r#else,
                        })
                })
        }),
        labeled::Stmnt::DoWhile {
            body,
            condition,
            label,
        } => {
            let body = typecheck_statement(*body, return_type, table, switch_val_type)?;
            let condition = typecheck_expression(condition, table)?;
            Ok(Stmnt::DoWhile {
                body: body.into(),
                condition,
                label,
            })
        }
        labeled::Stmnt::While {
            body,
            condition,
            label,
        } => {
            let body = typecheck_statement(*body, return_type, table, switch_val_type)?;
            let condition = typecheck_expression(condition, table)?;
            Ok(Stmnt::While {
                body: body.into(),
                condition,
                label,
            })
        }
        /*
         *  need to handle switch statement types here instead of resolve_loops or add another pass
         *  lowk maybe add another pass just for switch statements
         */
        labeled::Stmnt::Label {
            body,
            name: ast::semantics::Label::Case { mut c, id },
        } => {
            if let Some(val_ty) = switch_val_type {
                c = c.convert_to(&val_ty);
                Ok(Stmnt::Label {
                    name: ast::semantics::Label::Case { c, id },
                    body: typecheck_statement(*body, return_type, table, switch_val_type)?.into(),
                })
            } else {
                Err(Error::OutOfSwitch)
            }
        }

        labeled::Stmnt::Label { body, name } => Ok(Stmnt::Label {
            name,
            body: typecheck_statement(*body, return_type, table, switch_val_type)?.into(),
        }),
        labeled::Stmnt::For {
            init,
            condition,
            post,
            body,
            label,
        } => {
            let init = init
                .map(|init| match *init {
                    labeled::ForInit::D(v) => variable_declaration(v, table).map(ForInit::D),

                    labeled::ForInit::E(e) => typecheck_expression(e, table).map(ForInit::E),
                })
                .transpose()?
                .map(Box::new);

            let condition = condition
                .map(|c| typecheck_expression(c, table))
                .transpose()?;
            let post = post.map(|p| typecheck_expression(p, table)).transpose()?;

            let body = Box::new(typecheck_statement(
                *body,
                return_type,
                table,
                switch_val_type,
            )?);
            Ok(Stmnt::For {
                init,
                condition,
                post,
                body,
                label,
            })
        }
        labeled::Stmnt::Ret(e) => {
            if let Some(return_type) = return_type {
                let mut r = typecheck_expression(e, table)?;
                convert_to(&mut r, &return_type);
                Ok(Stmnt::Ret(r))
            } else {
                panic!()
            }
        }
        labeled::Stmnt::Switch {
            val: v,
            body: b,
            label,
            cases,
            default,
        } => {
            let val = typecheck_expression(v, table)?;
            let new_inner_val_type = Some(val.ty());

            Ok(Stmnt::Switch {
                val,
                body: Box::new(typecheck_statement(
                    *b,
                    return_type,
                    table,
                    new_inner_val_type,
                )?),
                label,
                cases,
                default,
            })
        }

        labeled::Stmnt::Null => Ok(Stmnt::Null),
        labeled::Stmnt::Goto(g) => Ok(Stmnt::Goto(g)),
        labeled::Stmnt::Break(l) => Ok(Stmnt::Break(l)),
        labeled::Stmnt::Continue(c) => Ok(Stmnt::Continue(c)),
    }
}

#[derive(Debug)]
pub enum Error {
    DuplicateDefinition,
    DupliCase,
    ConflictingDeclaration,

    OutOfSwitch,

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
    Expected(ast::semantics::Expected),
}

impl From<ast::semantics::Expected> for Error {
    fn from(value: ast::semantics::Expected) -> Self {
        Self::Expected(value)
    }
}
