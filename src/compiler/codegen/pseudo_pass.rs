use super::assembly;

use assembly::push_instructions;
use assembly::tacky::TackyBinary;
use assembly::Binary;
use assembly::Op;
use assembly::Pseudo;
use assembly::PseudoOp;
use assembly::TackyInstruction;

use assembly::{Function, Program, Register};

pub fn emit(program: Program<TackyInstruction>) -> Program<Pseudo> {
    let function = convert_function(program.0);
    Program(function)
}

fn convert_function(Function { name, body }: Function<TackyInstruction>) -> Function<Pseudo> {
    let mut instructions = Vec::new();
    for op in body {
        convert_instruction(op, &mut instructions);
    }

    Function {
        name,
        body: instructions.into(),
    }
}

fn convert_instruction(instruction: TackyInstruction, instructions: &mut Vec<Pseudo>) {
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
            instructions.push(Pseudo::Mov { src, dst });
            instructions.push(Pseudo::Ret);
        }
        TackyOp::Unary {
            op,
            source: src,
            dst,
        } => {
            instructions.push(Pseudo::Mov {
                src: src.into(),
                dst: dst.clone().into(),
            });
            instructions.push(Pseudo::Unary {
                operator: op.into(),
                operand: dst.into(),
            });
        }
        _ => {}
    };
}

fn convert_binary(
    op: TackyBinary,
    source_1: PseudoOp,
    source_2: PseudoOp,
    dst: PseudoOp,
    instructions: &mut Vec<Pseudo>,
) {
    match op {
        // if its add subtrcat or multiply, then the only difference is the binary operator, so
        // I use an if elif else to handle it
        operator @ (TackyBinary::Add
        | TackyBinary::Subtract
        | TackyBinary::Multiply
        | TackyBinary::BitAnd
        | TackyBinary::BitOr
        | TackyBinary::Xor
        | TackyBinary::LeftShift
        | TackyBinary::RightShift) => {
            // eww eww eww
            let operator = match operator {
                TackyBinary::Add => Binary::Add,
                TackyBinary::Subtract => Binary::Sub,
                TackyBinary::Multiply => Binary::Mult,
                TackyBinary::BitAnd => Binary::And,
                TackyBinary::BitOr => Binary::Or,
                TackyBinary::Xor => Binary::Xor,
                TackyBinary::LeftShift => Binary::ShiftLeft,
                TackyBinary::RightShift => Binary::ShiftRight,
                _ => unreachable!(),
            };
            push_instructions(
                instructions,
                [
                    Pseudo::mov(source_1, dst.clone()),
                    Pseudo::binary(operator, source_2, dst),
                ],
            );
        }
        operator @ (TackyBinary::Divide | TackyBinary::Remainder) => {
            // IDiv stores the result of division in AX, and the remiainder in Dx, but
            // otherwise the Remainder and Division instructions are the same
            let result = if operator == TackyBinary::Divide {
                Register::Ax.into()
            } else {
                Register::Dx.into()
            };
            push_instructions(
                instructions,
                [
                    Pseudo::mov(source_1, Register::Ax.into()),
                    Pseudo::Cdq,
                    Pseudo::idiv(source_2),
                    Pseudo::mov(result, dst),
                ],
            );
        }
        _ => {}
    };
}
