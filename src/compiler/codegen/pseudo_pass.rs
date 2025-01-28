use super::assembly;

use crate::compiler::lex::Identifier as TackyIdent;
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

use assembly::{Function, Program, Register};

pub fn emit(program: Program<TackyInstruction>) -> Program<Pseudo> {
    let function = convert_function(program.0);
    Program(function)
}

fn convert_function(Function { name, body }: Function<TackyInstruction>) -> Function<Pseudo> {
    let mut instructions = OpVec::new();
    for op in body {
        convert_instruction(op, &mut instructions);
    }

    Function {
        name,
        body: instructions.into(),
    }
}

fn convert_instruction(instruction: TackyInstruction, instructions: &mut OpVec<Pseudo>) {
    use TackyInstruction as TackyOp;
    match instruction {
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
            instructions.push(convert_conditional_jump(condition, target, condition_code))
        }
        TackyOp::JumpIfNotZero { condition, target } => {
            let condition_code = CondCode::NE;
            instructions.push(convert_conditional_jump(condition, target, condition_code))
        }
        TackyOp::Jump { target } => instructions.push_one(Pseudo::Jmp(target)),
        TackyOp::Label(label) => instructions.push_one(Pseudo::Label(label)),
    };
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
        ])
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
        ])
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
