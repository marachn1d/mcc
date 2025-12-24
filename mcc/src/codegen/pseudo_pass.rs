use asm::tacky::{
    FunctionDefinition as TackyFD, Instruction as TackyInstruction, Program as TackyProgram,
    StaticVar as TackySV, TackyBinary, TopLevel as TackyTL, Value,
};
use asm::x86::asm_type;
use asm::x86::{
    pseudo_regs as pseudop, AsmType, Binary, CondCode, FunctionDefinition, Op, Program, Pseudo,
    PseudoOp, Register, ShiftTy, StaticVar, TopLevel,
};
use ast::parse::UnOp;
use ast::{Ident, VarType};

use ast::semantics::Attr;

use ast::semantics::SymbolTable;

use asm::x86::{BackendSymbol, BackendTable};

pub fn emit(program: TackyProgram, table: SymbolTable) -> (Program<Pseudo>, BackendTable) {
    let mut decs = Vec::with_capacity(program.0.len());
    for dec in program.0 {
        decs.push(match dec {
            TackyTL::Fn(f) => TopLevel::Fn(convert_function(f, &table)),
            TackyTL::StaticVar(TackySV {
                name,
                global,
                init,
                typ,
            }) => TopLevel::StaticVar(StaticVar {
                name,
                global,
                init,
                alignment: typ.alignment(),
                ty: asm_type(typ),
            }),
        })
    }
    (Program(decs.into()), update_table(table))
}

pub fn update_table(mut table: SymbolTable) -> BackendTable {
    let mut tbl = BackendTable::new();
    for (key, val) in table.drain() {
        tbl.insert(key, BackendSymbol::from(val));
    }
    tbl
}

const fn get_type(ty: &VarType) -> AsmType {
    match ty {
        VarType::Int(_) => AsmType::Longword,
        VarType::Long(_) => AsmType::Quadword,
    }
}

fn convert_val(val: &Value) -> PseudoOp {
    match val {
        Value::Constant(c) => Op::imm(c.long().signed()).pseudo(),
        Value::Var(v) => PseudoOp::PseudoRegister(v.clone()),
    }
}

fn var_type(var: &Ident, table: &SymbolTable) -> AsmType {
    get_type(&var_tacky_type(var, table))
}

fn var_tacky_type(var: &Ident, table: &SymbolTable) -> VarType {
    let Some(Attr::Automatic(typ) | Attr::Static { typ, .. }) = table.get(var) else {
        panic!(
            "unexpected symbol result: {:?} (expected automatic)",
            table.get(var)
        )
    };
    *typ
}

fn val_tacky_type(val: &Value, table: &SymbolTable) -> VarType {
    match val {
        Value::Constant(c) => c.ty(),
        Value::Var(v) => var_tacky_type(v, table),
    }
}

fn val_type(val: &Value, table: &SymbolTable) -> AsmType {
    match val {
        Value::Constant(c) => c.map(|_| AsmType::Longword, |_| AsmType::Quadword),
        Value::Var(v) => var_type(v, table),
    }
}

fn convert_function(
    TackyFD {
        name,
        body,
        params,
        global,
    }: TackyFD,
    table: &SymbolTable,
) -> FunctionDefinition<Pseudo> {
    let mut instructions = Vec::new();

    for (dst, src) in params
        .iter()
        .zip(PseudoOp::SYSV_ARG_REGS.map(PseudoOp::register))
    {
        instructions.push(Pseudo::Mov {
            regs: (src, PseudoOp::PseudoRegister(dst.clone())),
            ty: var_type(dst, table),
        })
    }

    let mut start = 16;
    for param_n in params.iter().skip(6) {
        instructions.push(Pseudo::Mov {
            regs: (
                Op::Stack(start).into(),
                PseudoOp::PseudoRegister(param_n.clone()),
            ),
            ty: var_type(param_n, table),
        });
        start += 8;
    }

    for op in body {
        convert_instruction(op, &mut instructions, table);
    }

    FunctionDefinition {
        name,
        params,
        body: instructions.into(),
        global,
    }
}

fn convert_instruction(
    instruction: TackyInstruction,
    instructions: &mut Vec<Pseudo>,
    table: &SymbolTable,
) {
    use TackyInstruction as TackyOp;
    match instruction {
        TackyOp::FunCall { name, args, dst } => {
            convert_funcall(name, args, dst, instructions, table)
        }
        TackyOp::Binary {
            operator,
            source_1,
            source_2,
            dst,
        } => {
            convert_binary(operator, source_1, source_2, dst, instructions, table);
        }
        TackyOp::Return(var) => {
            instructions.extend([
                Pseudo::Mov {
                    ty: val_type(&var, table),
                    regs: (var.into(), pseudop::AX),
                },
                Pseudo::Ret,
            ]);
        }
        TackyOp::Unary { op, source, dst } => {
            convert_unary(instructions, op, source, dst, table);
        }
        TackyOp::Copy { src, dst } => {
            instructions.push(Pseudo::Mov {
                ty: val_type(&src, table),
                regs: (src.into(), dst.into()),
            });
        }
        TackyOp::JumpIfZero { condition, target } => {
            let condition_code = CondCode::E;

            let ty = val_type(&condition, table);
            instructions.extend(convert_conditional_jump(
                condition,
                target,
                condition_code,
                ty,
            ));
        }
        TackyOp::JumpIfNotZero { condition, target } => {
            let condition_code = CondCode::NE;
            let ty = val_type(&condition, table);
            instructions.extend(convert_conditional_jump(
                condition,
                target,
                condition_code,
                ty,
            ));
        }
        TackyOp::Jump { target } => instructions.push(Pseudo::Jmp(target)),
        TackyOp::Label(label) => instructions.push(Pseudo::Label(label)),
        TackyOp::SignExtend { src, dst } => {
            instructions.push(Pseudo::movsx(src.into(), dst.into()))
        }
        TackyOp::Truncate { src, dst } => {
            let dst = match dst {
                Value::Constant(c) => Value::Constant(c.convert_to(&VarType::INT)),
                _ => dst,
            };
            instructions.push(Pseudo::mov(src.into(), dst.into(), AsmType::Longword));
        }
        TackyOp::ZeroExtend { src, dst } => {
            instructions.push(Pseudo::mov_zero_extend(src.into(), dst.into()))
        }
    };
}

fn should_pad(args: &[Value]) -> bool {
    if args.len() <= 6 {
        false
    } else {
        // each arg from here is 8 more bytes, but we want to be 16 byte
        // aligned, so we should align the stack
        //
        // even number of non stack args, so the whole thing will be odd if the stack args are odd,
        // in which case we should pad
        !args.len().is_multiple_of(2)
    }
}

fn push_args(args: &[Value], instructions: &mut Vec<Pseudo>, table: &SymbolTable) -> Option<usize> {
    const TABLE: [Register; 6] = PseudoOp::SYSV_ARG_REGS;

    for (i, arg) in args.iter().take(6).enumerate() {
        instructions.push(Pseudo::mov(
            arg.clone().into(),
            PseudoOp::register(TABLE[i]),
            val_type(arg, table),
        ))
    }

    if args.len() > 6 {
        Some(push_stack_args(&args[6..], instructions, table))
    } else {
        None
    }
}

fn push_val(val: &Value, instructions: &mut Vec<Pseudo>, table: &SymbolTable) {
    if val_type(val, table) == AsmType::Quadword {
        let val = match val {
            Value::Constant(c) => Op::imm(c.long().signed()).pseudo(),
            Value::Var(v) => PseudoOp::PseudoRegister(v.clone()),
        };
        instructions.push(Pseudo::Push(val))
    } else {
        instructions.extend([
            Pseudo::Mov {
                ty: AsmType::Longword,
                regs: (convert_val(val), pseudop::AX),
            },
            Pseudo::Push(Register::Ax.into()),
        ])
    }
}

fn push_stack_args(
    stack_args: &[Value],
    instructions: &mut Vec<Pseudo>,
    table: &SymbolTable,
) -> usize {
    let mut byte_count = 0;
    for arg in stack_args.iter().rev() {
        push_val(arg, instructions, table);
        byte_count += 8;
    }
    byte_count
}

fn convert_funcall(
    name: Ident,
    args: Box<[Value]>,
    dst: Value,
    instructions: &mut Vec<Pseudo>,
    table: &SymbolTable,
) {
    let padding = if should_pad(&args) {
        instructions.push(Pseudo::allocate_stack(8));
        8
    } else {
        0
    };

    let cleanup_bytes = push_args(&args, instructions, table);

    instructions.push(Pseudo::Call(name.clone()));

    if let Some(cleanup) = cleanup_bytes {
        instructions.push(Pseudo::deallocate_stack((cleanup + padding) as i64));
    }

    instructions.push(Pseudo::Mov {
        ty: val_type(&dst, table),
        regs: (Register::Ax.into(), dst.into()),
    });
}

fn convert_unary(
    instructions: &mut Vec<Pseudo>,
    op: UnOp,
    src: Value,
    dst: Ident,
    table: &SymbolTable,
) {
    let ty = val_type(&src, table);
    let src = src.into();
    if op == UnOp::Not {
        let dst_ty = var_type(&dst, table);
        let dst = PseudoOp::from(dst);
        instructions.extend([
            Pseudo::Cmp {
                ty,
                regs: (Op::imm(0).into(), src),
            },
            Pseudo::Mov {
                ty: dst_ty,
                regs: (Op::imm(0).into(), dst.clone()),
            },
            Pseudo::SetCC {
                condition: CondCode::E,
                op: dst,
            },
        ]);
    } else {
        let dst = PseudoOp::from(dst);
        instructions.extend([
            Pseudo::Mov {
                ty,
                regs: (src, dst.clone()),
            },
            Pseudo::Unary {
                ty,
                operator: op.into(),
                operand: dst,
            },
        ]);
    }
}

fn convert_conditional_jump(
    condition: Value,
    label: Ident,
    code: CondCode,
    ty: AsmType,
) -> [Pseudo; 2] {
    [
        Pseudo::Cmp {
            ty,
            regs: (Op::imm(0).into(), condition.into()),
        },
        Pseudo::JmpCC {
            condition: code,
            label,
        },
    ]
}

fn convert_binary(
    op: TackyBinary,
    source_1: Value,
    source_2: Value,
    dst: Value,
    instructions: &mut Vec<Pseudo>,
    table: &SymbolTable,
) {
    let src_tacky_ty = val_tacky_type(&source_1, table);

    let src_ty = val_type(&source_1, table);

    let source_1 = PseudoOp::from(source_1);
    let source_2 = PseudoOp::from(source_2);

    match process_binop(op, src_tacky_ty) {
        Binop::Relational(condition) => {
            let dst_ty = val_type(&dst, table);
            //eprintln!("relational {source_1:?} {op:?} {source_2:?} type is {src_ty:?}");
            let dst = PseudoOp::from(dst);

            instructions.extend([
                Pseudo::cmp(source_2, source_1, src_ty),
                Pseudo::mov(Op::imm(0).into(), dst.clone(), dst_ty),
                Pseudo::SetCC { condition, op: dst },
            ]);
        }
        Binop::Normal(operator) => {
            let dst = PseudoOp::from(dst);
            instructions.extend([
                Pseudo::mov(source_1, dst.clone(), src_ty),
                Pseudo::binary(operator, source_2, dst, src_ty),
            ]);
        }
        Binop::Div {
            reg: result_register,
            signed,
        } => {
            let dst = PseudoOp::from(dst);

            if signed {
                instructions.extend([
                    Pseudo::mov(source_1, Register::Ax.into(), src_ty),
                    Pseudo::Cdq(src_ty),
                    Pseudo::idiv(source_2, src_ty),
                    Pseudo::mov(result_register.into(), dst, src_ty),
                ]);
            } else {
                instructions.extend([
                    Pseudo::mov(source_1, Register::Ax.into(), src_ty),
                    Pseudo::mov(Op::imm(0).into(), Register::Dx.into(), src_ty),
                    Pseudo::div(source_2, src_ty),
                    Pseudo::mov(result_register.into(), dst, src_ty),
                ])
            }
        }
    };
}

const fn process_binop(op: TackyBinary, ty: VarType) -> Binop {
    match op {
        TackyBinary::Add => Binop::Normal(Binary::Add),
        TackyBinary::Subtract => Binop::Normal(Binary::Sub),
        TackyBinary::Multiply => Binop::Normal(Binary::Mult),
        TackyBinary::BitAnd => Binop::Normal(Binary::And),
        TackyBinary::BitOr => Binop::Normal(Binary::Or),
        TackyBinary::Xor => Binop::Normal(Binary::Xor),
        TackyBinary::LeftShift => {
            let shift_ty = if ty.signed() {
                ShiftTy::Arithmetic
            } else {
                ShiftTy::Logical
            };
            Binop::Normal(Binary::ShiftLeft(shift_ty))
        }
        TackyBinary::RightShift => {
            let shift_ty = if ty.signed() {
                ShiftTy::Arithmetic
            } else {
                ShiftTy::Logical
            };
            Binop::Normal(Binary::ShiftRight(shift_ty))
        }
        TackyBinary::EqualTo => Binop::Relational(CondCode::E),
        TackyBinary::NotEqual => Binop::Relational(CondCode::NE),
        TackyBinary::LessThan => {
            if ty.signed() {
                Binop::Relational(CondCode::L)
            } else {
                Binop::Relational(CondCode::B)
            }
        }
        TackyBinary::Leq => {
            if ty.signed() {
                Binop::Relational(CondCode::LE)
            } else {
                Binop::Relational(CondCode::BE)
            }
        }
        TackyBinary::GreaterThan => {
            if ty.signed() {
                Binop::Relational(CondCode::G)
            } else {
                Binop::Relational(CondCode::A)
            }
        }

        TackyBinary::Geq => {
            if ty.signed() {
                Binop::Relational(CondCode::GE)
            } else {
                Binop::Relational(CondCode::AE)
            }
        }

        TackyBinary::Divide => Binop::Div {
            reg: Register::Ax,
            signed: ty.signed(),
        },
        TackyBinary::Remainder => Binop::Div {
            reg: Register::Dx,
            signed: ty.signed(),
        },
    }
}

enum Binop {
    Relational(CondCode),
    Normal(Binary),
    Div { reg: Register, signed: bool },
}
