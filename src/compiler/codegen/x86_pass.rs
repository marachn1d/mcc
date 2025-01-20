use super::assembly;

use super::Binary;
use super::{Function, Identifier, Program};
use assembly::push_instructions;
use assembly::Op;
use assembly::Pseudo;
use assembly::PseudoOp;
use assembly::Register;
use assembly::X86;
use std::collections::HashMap;
use std::rc::Rc;

pub fn fix_ast(program: Program<Pseudo>) -> Program<X86> {
    let main = fix_function(program.0);
    Program(main)
}

fn fix_function(function: Function<Pseudo>) -> Function<X86> {
    let mut stack_frame = StackFrame::default();
    let mut body: Vec<X86> = Vec::with_capacity(function.body.len() + 1);
    body.push(X86::AllocateStack(0));

    for op in get_iter(function.body) {
        fix_instruction(op, &mut stack_frame, &mut body);
    }

    if stack_frame.size == 0 {
        // eww eww eww sorry sorry sorry
        body.remove(0);
    } else {
        body[0] = X86::AllocateStack(stack_frame.size);
    }

    Function {
        name: function.name,
        body: body.into(),
    }
}

fn get_iter<T>(boxed_slice: Box<[T]>) -> std::vec::IntoIter<T> {
    <Box<[T]> as IntoIterator>::into_iter(boxed_slice)
}
#[derive(Default)]
struct StackFrame {
    map: HashMap<Rc<Identifier>, isize>,
    size: isize,
}

impl StackFrame {
    fn get(&mut self, ident: &Rc<Identifier>) -> isize {
        if let Some(offset) = self.map.get(ident) {
            *offset
        } else {
            self.size -= 4;
            self.map.insert(ident.clone(), self.size);
            self.size
        }
    }
    fn fix_operand(&mut self, operand: PseudoOp) -> Op {
        match operand {
            PseudoOp::Normal(o) => o,
            PseudoOp::PseudoRegister(name) => self.fix_by_name(&name),
        }
    }

    fn fix_by_name(&mut self, name: &Rc<Identifier>) -> Op {
        Op::Stack(self.get(name))
    }
}

fn fix_instruction(op: Pseudo, stack_frame: &mut StackFrame, vec: &mut Vec<X86>) {
    match op {
        Pseudo::Mov { src, dst } => {
            let src = stack_frame.fix_operand(src);
            let dst = stack_frame.fix_operand(dst);
            // we can't have the stack in source, so replace it with two instructions
            if let Op::Stack(_) = src {
                let temp_register = Op::Register(Register::R10);
                push_instructions(
                    vec,
                    [X86::mov(src, temp_register), X86::mov(temp_register, dst)],
                );
            } else {
                vec.push(X86::mov(src, dst));
            }
        }

        Pseudo::Unary { operator, operand } => {
            let operand = stack_frame.fix_operand(operand);
            vec.push(X86::Unary { operator, operand });
        }
        Pseudo::Binary {
            operator,
            op,
            dst_op,
        } => fix_binary(operator, op, dst_op, stack_frame, vec),
        Pseudo::Idiv { divisor } => {
            let divisor = stack_frame.fix_operand(divisor);
            if let Op::Imm(value) = divisor {
                let temp_register = Op::Register(Register::R10);
                push_instructions(
                    vec,
                    [
                        X86::mov(Op::Imm(value), temp_register),
                        X86::Idiv {
                            divisor: temp_register,
                        },
                    ],
                );
            } else {
                vec.push(X86::Idiv { divisor });
            }
        }
        Pseudo::AllocateStack(n) => vec.push(X86::AllocateStack(n)),
        Pseudo::Ret => vec.push(X86::Ret),
        Pseudo::Cdq => vec.push(X86::Cdq),
    };
}

fn fix_binary(
    operator: Binary,
    op: PseudoOp,
    dst_op: PseudoOp,
    stack_frame: &mut StackFrame,
    instructions: &mut Vec<X86>,
) {
    match (operator, op, dst_op) {
        // mult can't use a stack address as it's destination
        (Binary::Mult, op, PseudoOp::PseudoRegister(dst_name)) => {
            let op = stack_frame.fix_operand(op);
            let dst_op = stack_frame.fix_by_name(&dst_name);
            push_instructions(
                instructions,
                [
                    X86::mov(dst_op, Register::R11.into()),
                    X86::binary(operator, op, Register::R11.into()),
                    X86::mov(Register::R11.into(), dst_op),
                ],
            );
        }
        // add and subtract can't operate with just addresses
        (
            Binary::Add | Binary::Sub | Binary::And | Binary::Xor | Binary::Or,
            PseudoOp::PseudoRegister(op_name),
            PseudoOp::PseudoRegister(dst_name),
        ) => {
            let src = stack_frame.fix_by_name(&op_name);
            let dst = stack_frame.fix_by_name(&dst_name);
            push_instructions(
                instructions,
                [
                    X86::mov(src, Register::R10.into()),
                    X86::binary(operator, Register::R10.into(), dst),
                ],
            );
        }
        // normal case so the other one can be like the fallthrough
        (Binary::ShiftLeft | Binary::ShiftRight, PseudoOp::Normal(op @ Op::Imm(_)), dst) => {
            let dst = stack_frame.fix_operand(dst);
            instructions.push(X86::binary(operator, op, dst));
        }
        (Binary::ShiftLeft | Binary::ShiftRight, op, dst) => {
            let shift_reg = Register::Cx;
            let op = stack_frame.fix_operand(op);
            let dst = stack_frame.fix_operand(dst);
            push_instructions(
                instructions,
                [
                    X86::mov(op, shift_reg.into()),
                    X86::binary(operator, Register::Cl.into(), dst),
                ],
            );
        }
        // other cases: handle normally
        (operator, PseudoOp::PseudoRegister(op), PseudoOp::Normal(dst_op)) => {
            let op = stack_frame.fix_by_name(&op);
            instructions.push(X86::binary(operator, op, dst_op));
        }
        (operator, PseudoOp::Normal(op), PseudoOp::PseudoRegister(dst_op)) => {
            let dst_op = stack_frame.fix_by_name(&dst_op);
            instructions.push(X86::binary(operator, op, dst_op));
        }
        (operator, PseudoOp::Normal(op), PseudoOp::Normal(dst_op)) => {
            instructions.push(X86::binary(operator, op, dst_op));
        }
    }
}
