use super::assembly;

use crate::lex::Identifier as TackyIdent;
use assembly::tacky::TackyBinary;
use assembly::tacky::TackyUnary;
use assembly::tacky::Value;
use assembly::Binary;
use assembly::Op;
use assembly::OpVec;

use assembly::CondCode;
use assembly::Pseudo;
use assembly::PseudoOp;
use assembly::TackyInstruction;
use assembly::TopLevel;
use assembly::{FunctionDefinition, Program, Register};

pub fn emit(program: Program<TackyInstruction>) -> Program<Pseudo> {
    let mut decs = Vec::with_capacity(program.0.len());
    for dec in program.0 {
        decs.push(match dec {
            TopLevel::Fn(f) => TopLevel::Fn(convert_function(f)),
            TopLevel::StaticVar(v) => TopLevel::StaticVar(v),
        })
    }
    Program(decs.into())
}

fn convert_function(
    FunctionDefinition {
        name,
        body,
        params,
        global,
    }: FunctionDefinition<TackyInstruction>,
) -> FunctionDefinition<Pseudo> {
    const REGISTER_ARGS: [Register; 6] = [
        Register::Di,
        Register::Si,
        Register::Dx,
        Register::Cx,
        Register::R8,
        Register::R9,
    ];

    let mut instructions = OpVec::new();

    for (dst, src) in params
        .iter()
        .zip(REGISTER_ARGS.map(|x| PseudoOp::Normal(Op::Register(x))))
    {
        instructions.push([Pseudo::Mov {
            src,
            dst: PseudoOp::PseudoRegister(dst.clone()),
        }])
    }

    let mut start = 16;
    for param_n in params.iter().skip(6) {
        instructions.push([Pseudo::Mov {
            src: Op::Stack(start).into(),
            dst: PseudoOp::PseudoRegister(param_n.clone()),
        }]);
        start += 8;
    }

    for op in body {
        convert_instruction(op, &mut instructions);
    }

    FunctionDefinition {
        name,
        params,
        body: instructions.into(),
        global,
    }
}

use super::Identifier;

fn convert_instruction(instruction: TackyInstruction, instructions: &mut OpVec<Pseudo>) {
    use TackyInstruction as TackyOp;
    match instruction {
        TackyOp::FunCall { name, args, dst } => convert_funcall(name, args, dst, instructions),
        TackyOp::Binary {
            operator,
            source_1,
            source_2,
            dst,
        } => {
            convert_binary(
                operator,
                source_1.into(),
                source_2.into(),
                dst.into(),
                instructions,
            );
        }
        TackyOp::Return(var) => {
            let src = var.into();
            let dst = Register::Ax;
            let dst = Op::Register(dst);
            let dst = PseudoOp::Normal(dst);
            instructions.push([Pseudo::Mov { src, dst }, Pseudo::Ret]);
        }
        TackyOp::Unary {
            op,
            source: src,
            dst,
        } => {
            convert_unary(instructions, op, src.into(), dst.into());
        }
        TackyOp::Copy { src, dst } => {
            instructions.push_one(Pseudo::Mov {
                src: src.into(),
                dst: dst.into(),
            });
        }
        TackyOp::JumpIfZero { condition, target } => {
            let condition_code = CondCode::E;
            instructions.push(convert_conditional_jump(condition, target, condition_code));
        }
        TackyOp::JumpIfNotZero { condition, target } => {
            let condition_code = CondCode::NE;
            instructions.push(convert_conditional_jump(condition, target, condition_code));
        }
        TackyOp::Jump { target } => instructions.push_one(Pseudo::Jmp(target)),
        TackyOp::Label(label) => instructions.push_one(Pseudo::Label(label)),
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
        args.len() % 2 != 0
    }
}

fn push_args(args: &[Value], instructions: &mut OpVec<Pseudo>) -> Option<usize> {
    const TABLE: [Register; 6] = Op::REGISTER_TABLE;

    for (i, arg) in args.iter().take(6).enumerate() {
        instructions.push_one(Pseudo::mov(arg.clone().into(), TABLE[i].into()))
    }

    if args.len() > 6 {
        Some(push_stack_args(&args[6..], instructions))
    } else {
        None
    }
}

fn push_stack_args(stack_args: &[Value], instructions: &mut OpVec<Pseudo>) -> usize {
    let mut byte_count = 0;
    for arg in stack_args.iter().rev() {
        match arg.clone() {
            Value::Constant(c) => instructions.push_one(Pseudo::Push(Op::Imm(c).into())),

            Value::Var(c) => instructions.push([
                Pseudo::Mov {
                    src: PseudoOp::PseudoRegister(c),
                    dst: Register::Ax.into(),
                },
                Pseudo::Push(Register::Ax.into()),
            ]),
            /*
            PseudoOp::Normal(Op::Imm(_) | Op::Register(_)) => {
                instructions.push_one(Pseudo::Push(op))
            }
            _ => instructions.push([
                Pseudo::Mov {
                    src: op,
                    dst: Register::Ax.into(),
                },
                Pseudo::Push(Register::Ax.into()),
            ]),
            */
        }
        byte_count += 8;
    }
    byte_count
}

fn convert_funcall(
    name: Identifier,
    args: Box<[Value]>,
    dst: Value,
    instructions: &mut OpVec<Pseudo>,
) {
    let padding = if should_pad(&args) {
        instructions.push_one(Pseudo::AllocateStack(8));
        8
    } else {
        0
    };

    let cleanup_bytes = push_args(&args, instructions);

    instructions.push_one(Pseudo::Call(name.clone()));

    if let Some(cleanup) = cleanup_bytes {
        instructions.push_one(Pseudo::DeallocateStack(cleanup + padding));
    }

    instructions.push_one(Pseudo::Mov {
        src: Register::Ax.into(),
        dst: dst.into(),
    });
}

fn convert_unary(instructions: &mut OpVec<Pseudo>, op: TackyUnary, src: PseudoOp, dst: PseudoOp) {
    if op == TackyUnary::Not {
        instructions.push([
            Pseudo::Cmp {
                left: Op::Imm(0).into(),
                right: src,
            },
            Pseudo::Mov {
                src: Op::Imm(0).into(),
                dst: dst.clone(),
            },
            Pseudo::SetCC {
                condition: CondCode::E,
                op: dst,
            },
        ]);
    } else {
        instructions.push([
            Pseudo::Mov {
                src,
                dst: dst.clone(),
            },
            Pseudo::Unary {
                operator: op.into(),
                operand: dst,
            },
        ]);
    }
}

fn convert_conditional_jump(condition: Value, label: TackyIdent, code: CondCode) -> [Pseudo; 2] {
    [
        Pseudo::Cmp {
            left: Op::Imm(0).into(),
            right: condition.into(),
        },
        Pseudo::JmpCC {
            condition: code,
            label,
        },
    ]
}

fn convert_binary(
    op: TackyBinary,
    source_1: PseudoOp,
    source_2: PseudoOp,
    dst: PseudoOp,
    instructions: &mut OpVec<Pseudo>,
) {
    match process_binop(op) {
        Binop::Relational(condition) => {
            instructions.push([
                Pseudo::cmp(source_2, source_1),
                Pseudo::mov(Op::Imm(0).into(), dst.clone()),
                Pseudo::SetCC { condition, op: dst },
            ]);
        }
        Binop::Normal(operator) => {
            instructions.push([
                Pseudo::mov(source_1, dst.clone()),
                Pseudo::binary(operator, source_2, dst),
            ]);
        }
        Binop::Div(result_register) => {
            instructions.push([
                Pseudo::mov(source_1, Register::Ax.into()),
                Pseudo::Cdq,
                Pseudo::idiv(source_2),
                Pseudo::mov(result_register.into(), dst),
            ]);
        }
    };
}

const fn process_binop(op: TackyBinary) -> Binop {
    match op {
        TackyBinary::Add => Binop::Normal(Binary::Add),
        TackyBinary::Subtract => Binop::Normal(Binary::Sub),
        TackyBinary::Multiply => Binop::Normal(Binary::Mult),
        TackyBinary::BitAnd => Binop::Normal(Binary::And),
        TackyBinary::BitOr => Binop::Normal(Binary::Or),
        TackyBinary::Xor => Binop::Normal(Binary::Xor),
        TackyBinary::LeftShift => Binop::Normal(Binary::ShiftLeft),
        TackyBinary::RightShift => Binop::Normal(Binary::ShiftRight),
        TackyBinary::EqualTo => Binop::Relational(CondCode::E),
        TackyBinary::NotEqual => Binop::Relational(CondCode::NE),
        TackyBinary::LessThan => Binop::Relational(CondCode::L),
        TackyBinary::GreaterThan => Binop::Relational(CondCode::G),
        TackyBinary::Leq => Binop::Relational(CondCode::LE),
        TackyBinary::Geq => Binop::Relational(CondCode::GE),

        TackyBinary::Divide => Binop::Div(Register::Ax),
        TackyBinary::Remainder => Binop::Div(Register::Dx),
    }
}

enum Binop {
    Relational(CondCode),
    Normal(Binary),
    Div(Register),
}
