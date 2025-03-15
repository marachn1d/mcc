use super::Identifier;
use super::{
    BlockItem, Declaration, Expression, ForInit, ParamList, Program, Statement, StorageClass,
    TypedExp,
};
use crate::parse::Expression as AstExpression;
use crate::parse::{Binary, FnType, Unary, VarType};
use std::collections::HashMap;

use std::collections::hash_map::Entry;

pub type SymbolTable = HashMap<Identifier, Attr>;
#[derive(Debug)]
pub enum Attr {
    Static {
        r#type: VarType,
        init: Option<InitialVal>,
        global: bool,
    },
    Automatic(VarType),
    Fn {
        defined: bool,
        global: bool,
        r#type: FnType,
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
            Self::Static { r#type, .. } | Self::Automatic(r#type) => Ok(r#type),
            Self::Fn { .. } => Err(Error::ExpectedVarType),
        }
    }

    pub const fn fn_type(&self) -> Result<&FnType, Error> {
        match self {
            Self::Fn { r#type, .. } => Ok(r#type),
            Self::Static { .. } | Self::Automatic(_) => Err(Error::ExpectedFnType),
        }
    }
}

use crate::lex::Constant;

#[derive(Debug, Copy, Clone)]
pub enum InitialVal {
    Tentative,
    Initial(Constant),
}

impl InitialVal {
    pub fn as_long(&self) -> i64 {
        match self {
            Self::Initial(Constant::Long(i)) => *i,
            Self::Initial(Constant::Integer(i)) => (*i).into(),
            Self::Tentative => 0,
        }
    }

    pub const fn as_int(&self) -> i32 {
        match self {
            Self::Initial(Constant::Integer(i)) => *i,
            Self::Initial(Constant::Long(i)) => *i as i32,
            Self::Tentative => 0,
        }
    }
}

// check FunctionDeclaration, VariableDeclaration,
use super::LabeledProgram;
use super::TypedProgram;

pub fn typecheck(p: LabeledProgram) -> Result<(SymbolTable, TypedProgram), Error> {
    let mut table = SymbolTable::new();
    let mut decs = Vec::with_capacity(p.0.len());
    for dec in p.0 {
        decs.push(top_level_declaration(dec, &mut table)?);
    }

    Ok((table, Program(decs.into())))
}

fn top_level_declaration(
    dec: Declaration<AstExpression>,
    table: &mut SymbolTable,
) -> Result<Declaration<TypedExp>, Error> {
    match dec {
        Declaration::Function {
            name,
            params,
            body,
            storage_class,
            r#type,
        } => function_declaration(name, params, body, storage_class, r#type, table, false),
        Declaration::Var {
            name,
            init,
            storage_class,
            r#type,
        } => top_level_var(name, init, storage_class, r#type, table),
    }
}

fn top_level_var(
    name: Identifier,
    init: Option<AstExpression>,
    storage_class: Option<StorageClass>,
    r#type: VarType,
    table: &mut SymbolTable,
) -> Result<Declaration<TypedExp>, Error> {
    let mut initial = tlv_initial(&init, &storage_class)?;

    // if we're not static, then we're global, if we are static, then we're not global?
    let mut global = storage_class != Some(StorageClass::Static);

    let table_entry = table.get(&name);

    if let Some(Attr::Static {
        init: old_init,
        global: old_global,
        r#type: _,
    }) = table_entry
    {
        check_linkage(&mut global, *old_global, &storage_class)?;
        check_initializer(old_init, &mut initial)?;
    } else if table_entry.is_some() {
        return Err(Error::ConflictingType);
    };

    table.insert(
        name.clone(),
        Attr::Static {
            init: initial,
            global,
            r#type,
        },
    );

    let initializer = if let Some(init) = init {
        Some(typecheck_expression(init, table)?)
    } else {
        None
    };

    Ok(Declaration::Var {
        name,
        init: initializer,
        storage_class,
        r#type,
    })
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

fn check_initializer(old: &Option<InitialVal>, new: &mut Option<InitialVal>) -> Result<(), Error> {
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

fn tlv_initial(
    init: &Option<AstExpression>,
    sc: &Option<StorageClass>,
) -> Result<Option<InitialVal>, Error> {
    match (init, sc) {
        (Some(AstExpression::Const(c)), _) => Ok(Some(InitialVal::Initial(*c))),
        (None, Some(StorageClass::Extern)) => Ok(None),
        (None, Some(StorageClass::Static)) => Ok(Some(InitialVal::Tentative)),
        (None, None) => Ok(Some(InitialVal::Tentative)),
        (_, _) => Err(Error::NotConstInitialized),
    }
}

fn declaration(
    dec: Declaration<AstExpression>,
    table: &mut SymbolTable,
) -> Result<Declaration<TypedExp>, Error> {
    match dec {
        Declaration::Function {
            name,
            params,
            body,
            storage_class,
            r#type,
        } => function_declaration(name, params, body, storage_class, r#type, table, true),
        Declaration::Var {
            name,
            init,
            storage_class,
            r#type,
        } => variable_declaration(name, init, r#type, storage_class, table),
    }
}

fn variable_declaration(
    name: Identifier,
    init: Option<AstExpression>,
    r#type: VarType,
    sc: Option<StorageClass>,
    table: &mut SymbolTable,
) -> Result<Declaration<TypedExp>, Error> {
    vardec_inner(name, init, r#type, sc, table).map(|(name, init, typ, sc)| Declaration::Var {
        name,
        init,
        storage_class: sc,
        r#type: typ,
    })
}

fn forinit_vardec(
    name: Identifier,
    init: Option<AstExpression>,
    r#type: VarType,
    sc: Option<StorageClass>,
    table: &mut SymbolTable,
) -> Result<ForInit<TypedExp>, Error> {
    vardec_inner(name, init, r#type, sc, table).map(|(name, init, typ, sc)| ForInit::D {
        name,
        initializer: init,
        sc,
        r#type: typ,
    })
}

fn vardec_inner(
    name: Identifier,
    init: Option<AstExpression>,
    r#type: VarType,
    sc: Option<StorageClass>,
    table: &mut SymbolTable,
) -> Result<(Identifier, Option<TypedExp>, VarType, Option<StorageClass>), Error> {
    let init = match (sc, &init) {
        (Some(StorageClass::Extern), Some(_)) => return Err(Error::DeclaredExtern),
        (Some(StorageClass::Extern), None) => {
            match table.entry(name.clone()) {
                Entry::Occupied(e) => {
                    if let Attr::Fn { .. } = e.get() {
                        return Err(Error::FnAsVar);
                    }
                }
                Entry::Vacant(e) => {
                    e.insert(Attr::Static {
                        init: None,
                        global: true,
                        r#type,
                    });
                }
            }
            None
        }
        (Some(StorageClass::Static), Some(AstExpression::Const(i))) => {
            table.insert(
                name.clone(),
                Attr::Static {
                    init: Some(InitialVal::Initial(*i)),
                    global: false,
                    r#type,
                },
            );
            Some(TypedExp {
                exp: Expression::Const(*i).into(),
                r#type: VarType::Int,
            })
        }
        (Some(StorageClass::Static), Some(_)) => return Err(Error::NotConstInitialized),
        (Some(StorageClass::Static), None) => {
            table.insert(
                name.clone(),
                Attr::Static {
                    init: Some(InitialVal::Initial(Constant::Long(0))),
                    global: false,
                    r#type,
                },
            );

            Some(TypedExp {
                exp: Expression::Const(Constant::Long(0)).into(),
                r#type: VarType::Int,
            })
        }
        (None, _) => {
            table.insert(name.clone(), Attr::Automatic(r#type));
            if let Some(init) = init {
                Some(typecheck_expression(init, table)?)
            } else {
                None
            }
        }
    };
    Ok((name, init, r#type, sc))
}

fn typecheck_expression(
    expression: AstExpression,
    table: &mut SymbolTable,
) -> Result<TypedExp, Error> {
    match expression {
        AstExpression::FunctionCall { name, args } => typecheck_fn_call(name, args, table),
        AstExpression::Var(name) => typecheck_var(name, table),
        AstExpression::Assignment(assignment) => {
            let left = typecheck_expression(assignment.0, table)?;
            let mut right = typecheck_expression(assignment.1, table)?;

            let new_type = left.r#type;
            convert_to(&mut right, &left.r#type);
            Ok(TypedExp {
                r#type: new_type,
                exp: Expression::Assignment {
                    from: left,
                    to: right,
                }
                .into(),
            })
        }
        AstExpression::Binary(Binary {
            left,
            right,
            operator,
        }) => {
            use crate::parse::BinaryOperator;
            let mut left = typecheck_expression(*left, table)?;
            let mut right = typecheck_expression(*right, table)?;
            if matches!(operator, BinaryOperator::LogAnd | BinaryOperator::LogOr) {
                Ok(TypedExp {
                    r#type: VarType::Int,
                    exp: Expression::Binary {
                        left,
                        right,
                        operator,
                    }
                    .into(),
                })
            } else if let Some(common) = left.r#type.common_type(&right.r#type) {
                convert_to(&mut left, &common);
                convert_to(&mut right, &common);
                let exp = Expression::Binary {
                    operator,
                    left,
                    right,
                }
                .into();
                Ok(if operator.relational() {
                    TypedExp {
                        r#type: VarType::Int,
                        exp,
                    }
                } else {
                    TypedExp {
                        r#type: common,
                        exp,
                    }
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
        AstExpression::Nested(exp) => typecheck_expression(*exp, table),
        AstExpression::Const(Constant::Integer(i)) => Ok(TypedExp {
            r#type: VarType::Int,
            exp: Expression::Const(Constant::Integer(i)).into(),
        }),

        AstExpression::Const(Constant::Long(l)) => Ok(TypedExp {
            r#type: VarType::Int,
            exp: Expression::Const(Constant::Long(l)).into(),
        }),

        AstExpression::Cast { target, exp } => {
            typecheck_expression(*exp, table).map(|exp| TypedExp {
                r#type: target,
                exp: Box::new(Expression::Cast { target, exp }),
            })
        }

        AstExpression::Unary(Unary { exp, op }) => {
            use crate::parse::UnaryOperator;
            typecheck_expression(*exp, table).map(|exp| TypedExp {
                r#type: if op == UnaryOperator::Not {
                    VarType::Int
                } else {
                    exp.r#type.clone()
                },
                exp: Box::new(Expression::Unary {
                    operator: op,
                    operand: exp,
                }),
            })
        }

        AstExpression::Conditional {
            condition,
            r#true,
            r#false,
        } => {
            let condition = typecheck_expression(*condition, table)?;
            let mut r#true = typecheck_expression(*r#true, table)?;
            let mut r#false = typecheck_expression(*r#false, table)?;

            let Some(common) = r#true.r#type.common_type(&r#false.r#type) else {
                return Err(Error::InvalidCast);
            };

            convert_to(&mut r#true, &common);

            convert_to(&mut r#false, &common);

            Ok(TypedExp {
                r#type: common,
                exp: Expression::Conditional {
                    condition,
                    r#true,
                    r#false,
                }
                .into(),
            })
        }
        AstExpression::PostfixIncrement(e) => {
            //Expression::PostfixIncrement(
            let inner_typ = typecheck_expression(*e, table)?;
            Ok(TypedExp {
                r#type: inner_typ.r#type,
                exp: Expression::PostfixIncrement(inner_typ).into(),
            })
        }
        AstExpression::PostfixDecrement(e) => {
            //Expression::PostfixIncrement(
            let inner_typ = typecheck_expression(*e, table)?;
            Ok(TypedExp {
                r#type: inner_typ.r#type,
                exp: Expression::PostfixDecrement(inner_typ).into(),
            })
        }
        AstExpression::PrefixIncrement(e) => {
            //Expression::PostfixIncrement(
            let inner_typ = typecheck_expression(*e, table)?;
            Ok(TypedExp {
                r#type: inner_typ.r#type,
                exp: Expression::PrefixIncrement(inner_typ).into(),
            })
        }
        AstExpression::PrefixDecrement(e) => {
            //Expression::PostfixIncrement(
            let inner_typ = typecheck_expression(*e, table)?;
            Ok(TypedExp {
                r#type: inner_typ.r#type,
                exp: Expression::PrefixDecrement(inner_typ).into(),
            })
        }
    }
}

fn convert_to(exp: &mut TypedExp, ty: &VarType) {
    if &exp.r#type != ty {
        let cast = Expression::Cast {
            target: ty.clone(),
            exp: exp.clone(),
        }
        .into();
        *exp = TypedExp {
            r#type: ty.clone(),
            exp: cast,
        }
    }
}

fn typecheck_fn_call(
    name: Identifier,
    args: Box<[AstExpression]>,
    table: &mut SymbolTable,
) -> Result<TypedExp, Error> {
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

        let r#type = if let Some(param) = ret {
            param
        } else {
            VarType::Int
        };

        Ok(TypedExp {
            r#type,
            exp: Expression::FunctionCall {
                name,
                args: new_args.into(),
            }
            .into(),
        })
    }
}

fn typecheck_var(name: Identifier, table: &mut SymbolTable) -> Result<TypedExp, Error> {
    let symbol_type = table
        .get(&name)
        .ok_or(Error::UndefinedVar)
        .map(Attr::var_type)??;
    Ok(TypedExp {
        r#type: *symbol_type,
        exp: Expression::Var(name).into(),
    })
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
        r#type: FnType { ret: _, params },
    } = e.get()
    else {
        return Err(Error::ConflictingType);
    };

    if *defined && has_body {
        Err(Error::DuplicateDefinition)
    } else if params
        .iter()
        .zip(new_params)
        .all(|(old, new)| *old == new.r#type)
    {
        Ok(())
    } else {
        Err(Error::WrongParams)
    }
}

fn function_declaration(
    name: Identifier,
    params: ParamList,
    body: Option<Box<[BlockItem<AstExpression>]>>,
    mut sc: Option<StorageClass>,
    r#type: FnType,
    table: &mut SymbolTable,
    block_scope: bool,
) -> Result<Declaration<TypedExp>, Error> {
    let global = sc.is_none_or(|sc| sc == StorageClass::Extern);

    let has_body = body.is_some();

    let entry = table.entry(name.clone());

    check_entry(&entry, &params, has_body)?;

    match entry {
        Entry::Occupied(mut e) if !block_scope => {
            let Attr::Fn {
                defined,
                global: was_global,
                r#type: _,
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
                r#type: r#type.clone(),
                defined: has_body,
                global,
            });
        }
    };

    for param in params.iter() {
        table.insert(param.name.clone(), Attr::Automatic(param.r#type));
    }

    let body = if let Some(body) = body {
        let mut new_body = Vec::new();
        for item in body {
            new_body.push(typecheck_blockitem(item, r#type.ret, table)?);
        }
        Some(new_body.into_boxed_slice())
    } else {
        None
    };
    Ok(Declaration::Function {
        name,
        params,
        body,
        storage_class: sc,
        r#type,
    })
}

fn typecheck_blockitem(
    block_item: BlockItem<AstExpression>,
    ret: Option<VarType>,
    table: &mut SymbolTable,
) -> Result<BlockItem<TypedExp>, Error> {
    match block_item {
        BlockItem::D(dec) => declaration(dec, table).map(BlockItem::D),
        BlockItem::S(s) => typecheck_statement(s, ret, table).map(BlockItem::S),
    }
}

fn typecheck_statement(
    stmt: Statement<AstExpression>,
    return_type: Option<VarType>,
    table: &mut SymbolTable,
) -> Result<Statement<TypedExp>, Error> {
    match stmt {
        Statement::Compound(stmts) => {
            let mut statements = Vec::with_capacity(stmts.len());
            for item in stmts {
                statements.push(typecheck_blockitem(item, return_type, table)?);
            }
            let statements = statements.into_boxed_slice();

            Ok(Statement::Compound(statements))
        }
        Statement::Exp(e) => typecheck_expression(e, table).map(Statement::Exp),
        Statement::If {
            condition,
            then,
            r#else,
        } => {
            typecheck_expression(condition, table)?;
            typecheck_statement(*then, return_type, table)?;
            if let Some(r#else) = r#else {
                typecheck_statement(*r#else, return_type, table)
            } else {
                todo!()
            }
        }
        Statement::DoWhile {
            body,
            condition,
            label,
        } => {
            let body = typecheck_statement(*body, return_type, table)?;
            let condition = typecheck_expression(condition, table)?;
            Ok(Statement::DoWhile {
                body: body.into(),
                condition,
                label,
            })
        }
        Statement::While {
            body,
            condition,
            label,
        } => {
            let body = typecheck_statement(*body, return_type, table)?;
            let condition = typecheck_expression(condition, table)?;
            Ok(Statement::While {
                body: body.into(),
                condition,
                label,
            })
        }
        Statement::Label { body, name } => Ok(Statement::Label {
            name,
            body: typecheck_statement(*body, return_type, table)?.into(),
        }),
        Statement::For {
            init,
            condition,
            post,
            body,
            label,
        } => {
            let init = init
                .map(|init| match init {
                    ForInit::D {
                        name,
                        initializer,
                        sc,
                        r#type,
                    } => forinit_vardec(name, initializer, r#type, sc, table),

                    ForInit::E(e) => typecheck_expression(e, table).map(ForInit::E),
                })
                .transpose()?;

            let condition = condition
                .map(|c| typecheck_expression(c, table))
                .transpose()?;
            let post = post.map(|p| typecheck_expression(p, table)).transpose()?;

            let body = Box::new(typecheck_statement(*body, return_type, table)?);
            Ok(Statement::For {
                init,
                condition,
                post,
                body,
                label,
            })
        }
        Statement::Ret(e) => {
            if let Some(return_type) = return_type {
                let mut r = typecheck_expression(e, table)?;
                convert_to(&mut r, &return_type);
                Ok(Statement::Ret(r))
            } else {
                panic!()
            }
        }
        Statement::Switch {
            val,
            body,
            label: _,
            cases: _,
            default: _,
        } => {
            typecheck_expression(val, table)?;
            typecheck_statement(*body, return_type, table)
        }
        Statement::Null | Statement::Goto(_) | Statement::Break(_) | Statement::Continue(_) => {
            todo!()
        }
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
