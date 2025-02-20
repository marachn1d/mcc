use super::assembly;

use crate::lex::Identifier as TackyIdent;
use assembly::tacky::TackyBinary;
use assembly::tacky::TackyUnary;
use assembly::tacky::Value;
use assembly::Binary;
use assembly::Op;
use assembly::OpVec;
use std::rc::Rc;

use assembly::CondCode;
use assembly::Pseudo;
use assembly::PseudoOp;
use assembly::TackyInstruction;

use assembly::{FunctionDefinition, Program, Register};

pub fn emit(program: Program<TackyInstruction>) -> Program<Pseudo> {
    let mut functions = Vec::with_capacity(program.0.len());
    for function in program.0 {
        functions.push(convert_function(function));
    }
    Program(functions.into())
}

struct SysVCallConvIter {
    next_arg: usize,
}

impl Iterator for SysVCallConvIter {
    type Item = PseudoOp;

    fn next(&mut self) -> Option<PseudoOp> {
        let res = Some(match self.next_arg {
            0 => Register::Di.into(),
            1 => Register::Si.into(),
            2 => Register::Dx.into(),
            3 => Register::Cx.into(),
            4 => Register::R8.into(),
            5 => Register::R9.into(),
            n @ 6.. => Op::Stack((n - 4) * 8).into(),
        });
        self.next_arg += 1;
        res
    }
}

impl SysVCallConvIter {
    const fn new() -> Self {
        Self { next_arg: 0 }
    }
}

fn convert_function(
    FunctionDefinition { name, body, params }: FunctionDefinition<TackyInstruction>,
) -> FunctionDefinition<Pseudo> {
    let mut instructions = OpVec::new();

    for (dst, src) in params.iter().zip(SysVCallConvIter::new()) {
        instructions.push([Pseudo::Mov {
            src,
            dst: PseudoOp::PseudoRegister(dst.clone()),
        }])
    }

    for op in body {
        convert_instruction(op, &mut instructions);
    }

    FunctionDefinition {
        name,
        params,
        body: instructions.into(),
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
        eprintln!("shouldn't pad");
        false
    } else {
        // each arg from here is 8 more bytes, but we want to be 16 byte
        // aligned, so we should align the stack
        //
        // even number of non stack args, so the whole thing will be odd if the stack args are odd,
        // in which case we should pad
        let res = args.len() % 2 != 0;
        if res {
            eprintln!("should");
        } else {
            eprintln!("shouldn't");
        }
        res
    }
}

fn push_args(args: &[Value], instructions: &mut OpVec<Pseudo>) {
    const TABLE: [Register; 6] = Op::REGISTER_TABLE;

    for (i, arg) in args.iter().take(6).enumerate() {
        instructions.push_one(Pseudo::mov(arg.clone().into(), TABLE[i].into()))
    }

    if args.len() > 6 {
        push_stack_args(&args[6..], instructions);
    }
}

const fn stack_args_bytes(args: &[Value]) -> usize {
    match args.len() {
        0..=6 => 0,
        more => (more - 6) * 8,
    }
}

fn push_stack_args(stack_args: &[Value], instructions: &mut OpVec<Pseudo>) {
    for arg in stack_args.iter().rev() {
        match arg {
            Value::Var(v) => {
                instructions.push_one(Pseudo::Push(PseudoOp::PseudoRegister(v.clone())))
            }
            Value::Constant(c) => {
                instructions.push([
                    Pseudo::Mov {
                        src: Op::Imm(*c).into(),
                        dst: Register::Ax.into(),
                    },
                    Pseudo::Push(Register::Ax.into()),
                ]);
            }
        }
    }
}

fn convert_funcall(
    name: Rc<Identifier>,
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

    push_args(&args, instructions);
    instructions.push_one(Pseudo::Call(name.clone()));

    let cleanup_bytes = padding + stack_args_bytes(&args);
    if cleanup_bytes != 0 {
        instructions.push_one(Pseudo::DeallocateStack(cleanup_bytes));
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

fn convert_conditional_jump(
    condition: Value,
    label: Rc<TackyIdent>,
    code: CondCode,
) -> [Pseudo; 2] {
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
