use super::assembly;
use super::Binary;
use super::{Identifier, Program};
use assembly::BackendSymbol;
use assembly::FunctionDefinition;
use assembly::Op;
use assembly::OpVec;
use assembly::Pseudo;
use assembly::PseudoOp;
use assembly::Register;
use assembly::SymbolTable;
use assembly::TopLevel;
use assembly::X86;
use std::collections::HashMap;
pub fn fix_ast(program: Program<Pseudo>, table: &SymbolTable) -> Program<X86> {
    let mut decs = Vec::with_capacity(program.0.len());
    for dec in program.0 {
        decs.push(match dec {
            TopLevel::Fn(f) => TopLevel::Fn(convert_function(f, table)),
            TopLevel::StaticVar(s) => TopLevel::StaticVar(s),
        })
    }
    Program(decs.into())
}

fn convert_function(
    FunctionDefinition {
        name,
        params,
        body,
        global,
    }: FunctionDefinition<Pseudo>,
    table: &SymbolTable,
) -> FunctionDefinition<X86> {
    let mut stack_frame = StackFrame::new(table);
    let body_vec: Vec<X86> = Vec::with_capacity(body.len() + 1);
    let mut body_vec: OpVec<X86> = body_vec.into();
    body_vec.push_one(X86::allocate_stack(0));

    for op in get_iter(body) {
        fix_instruction(op, &mut stack_frame, &mut body_vec);
    }

    let mut body: Vec<X86> = body_vec.0;

    if stack_frame.size == 0 {
        // eww eww eww sorry sorry sorry
        body.remove(0);
    } else {
        body[0] = X86::allocate_stack(stack_frame.rounded_size());
    }

    FunctionDefinition {
        name,
        params,
        body: body.into(),
        global,
    }
}

fn get_iter<T>(boxed_slice: Box<[T]>) -> std::vec::IntoIter<T> {
    <Box<[T]> as IntoIterator>::into_iter(boxed_slice)
}
struct StackFrame<'a> {
    map: HashMap<Identifier, isize>,
    size: usize,
    table: &'a SymbolTable,
}

const fn align_quadword(size: usize) -> usize {
    let remainder = (size + 8) % 16;
    if remainder == 0 {
        size
    } else {
        // should align to 16
        size + (16 - remainder)
    }
}

impl<'a> StackFrame<'a> {
    fn new(table: &'a SymbolTable) -> Self {
        Self {
            map: HashMap::new(),
            size: 0,
            table,
        }
    }

    fn var_type(&self, ident: &Identifier) -> Option<AsmType> {
        self.table
            .get(ident)
            .and_then(|input| match input {
                BackendSymbol::Obj { ty, .. } => Some(ty),
                BackendSymbol::Fn { .. } => None,
            })
            .copied()
    }

    fn get(&mut self, ident: &Identifier) -> Op {
        if let Some(offset) = self.map.get(ident) {
            Op::Stack(*offset)
        } else {
            match self.table.get(ident).unwrap() {
                BackendSymbol::Fn { .. } => panic!("expected var {ident}, got fn"),
                BackendSymbol::Obj {
                    ty: _,
                    is_static: true,
                } => Op::Data(ident.clone()),
                BackendSymbol::Obj {
                    ty,
                    is_static: false,
                } => {
                    let size = match ty {
                        AsmType::Longword => 4,
                        AsmType::Quadword => align_quadword(self.size),
                    };
                    self.size += size;
                    let offset = -(self.size as isize);
                    self.map.insert(ident.clone(), offset);
                    Op::Stack(offset)
                    /*
                    self.size += 4;
                     let offset = -(self.size as isize);
                     self.map.insert(ident.clone(), offset);
                     Op::Stack(offset)
                             */
                }
            }
        }
    }

    fn fix_operand(&mut self, operand: PseudoOp) -> Op {
        match operand {
            PseudoOp::Normal(o) => o,
            PseudoOp::PseudoRegister(name) => self.fix_by_name(&name),
        }
    }

    fn fix_by_name(&mut self, name: &Identifier) -> Op {
        self.get(name)
    }

    /*
    const fn rounded_size(&self) -> isize {
        (self.size / 16) + 16
    }
    */

    const fn rounded_size(&self) -> i64 {
        ((self.size / 16 + 1) * 16) as i64
    }
}

fn fix_instruction(op: Pseudo, stack_frame: &mut StackFrame, vec: &mut OpVec<X86>) {
    match op {
        Pseudo::Push(val) => {
            let val = stack_frame.fix_operand(val);
            vec.push_one(X86::Push(val));
        }
        Pseudo::Call(fun) => vec.push_one(X86::Call(fun)),
        Pseudo::Mov {
            src: PseudoOp::Normal(Op::Imm(i)),
            dst: PseudoOp::PseudoRegister(dst),
            ty: AsmType::Quadword,
        } => {
            let dst = stack_frame.fix_by_name(&dst);
            let src = Op::Imm(i);
            vec.push([
                X86::mov(src, Op::Register(Register::R10), AsmType::Quadword),
                X86::mov(Op::Register(Register::R10), dst, AsmType::Quadword),
            ]);
        }
        Pseudo::Mov {
            src: PseudoOp::Normal(Op::Imm(i)),
            dst: PseudoOp::PseudoRegister(dst),
            ty: AsmType::Longword,
        } => {
            if i > i32::MAX as i64 {
                let dst = stack_frame.fix_by_name(&dst);
                let src = Op::Imm((i as i32) as i64);
                vec.push([
                    X86::mov(src, Op::Register(Register::R10), AsmType::Longword),
                    X86::mov(Op::Register(Register::R10), dst, AsmType::Longword),
                ]);
            }
        }
        Pseudo::Mov { src, dst, ty } => {
            let src = stack_frame.fix_operand(src);
            let dst = stack_frame.fix_operand(dst);
            // we can't have the stack in source, so replace it with two instructions
            if let Op::Stack(_) | Op::Data(_) = src {
                let temp_register = Op::Register(Register::R10);
                vec.push([
                    X86::mov(src, temp_register.clone(), ty),
                    X86::mov(temp_register, dst, ty),
                ]);
            } else {
                vec.push_one(X86::mov(src, dst, ty));
            }
        }

        Pseudo::Unary {
            operator,
            operand,
            ty,
        } => {
            let operand = stack_frame.fix_operand(operand);
            vec.push_one(X86::Unary {
                operator,
                operand,
                ty,
            });
        }
        Pseudo::Binary {
            operator,
            op,
            dst_op,
            ty,
        } => fix_binary(operator, op, dst_op, stack_frame, vec, ty),
        Pseudo::Idiv { divisor, ty } => {
            let divisor = stack_frame.fix_operand(divisor);
            if let Op::Imm(value) = divisor {
                let temp_register = Op::Register(Register::R10);
                vec.push([
                    X86::mov(Op::Imm(value), temp_register.clone(), ty),
                    X86::Idiv {
                        divisor: temp_register,
                        ty,
                    },
                ]);
            } else {
                vec.push_one(X86::Idiv { divisor, ty });
            }
        }
        Pseudo::Ret => vec.push_one(X86::Ret),
        Pseudo::Cdq(ty) => vec.push_one(X86::Cdq(ty)),
        Pseudo::Cmp {
            left: PseudoOp::PseudoRegister(left_name),
            right: PseudoOp::PseudoRegister(right_name),
            ty,
        } => {
            let (left, right) = (
                stack_frame.fix_by_name(&left_name),
                stack_frame.fix_by_name(&right_name),
            );
            vec.push([
                X86::mov(left, Register::R10.into(), ty),
                X86::cmp(Register::R10.into(), right, ty),
            ]);
        }
        Pseudo::Cmp {
            left,
            right: PseudoOp::Normal(Op::Imm(right_val)),
            ty,
        } => {
            let left = stack_frame.fix_operand(left);
            vec.push([
                X86::mov(Op::Imm(right_val), Register::R11.into(), ty),
                X86::cmp(left, Register::R11.into(), ty),
            ]);
        }
        Pseudo::Cmp { left, right, ty } => {
            let (left, right) = (
                stack_frame.fix_operand(left),
                stack_frame.fix_operand(right),
            );
            vec.push_one(X86::Cmp { left, right, ty })
        }
        Pseudo::Jmp(label) => vec.push_one(X86::Jmp(label)),
        Pseudo::Label(name) => vec.push_one(X86::Label(name)),
        Pseudo::JmpCC { condition, label } => vec.push_one(X86::JmpCC { condition, label }),
        Pseudo::SetCC { condition, op } => {
            let op = stack_frame.fix_operand(op);
            vec.push_one(X86::SetCC { condition, op })
        }
        Pseudo::Movsx {
            ty,
            src,
            dst: PseudoOp::PseudoRegister(p),
        } => {
            let src = movsx_src(src, vec, stack_frame);
            let dst = stack_frame.fix_by_name(&p);
            vec.push([
                X86::Movsx {
                    ty,
                    src,
                    dst: Op::Register(Register::R11),
                },
                X86::Mov {
                    ty: AsmType::Quadword,
                    src: Op::Register(Register::R11),
                    dst,
                },
            ])
        }

        Pseudo::Movsx { ty, src, dst } => {
            let src = movsx_src(src, vec, stack_frame);
            vec.push_one(X86::Movsx {
                ty,
                src,
                dst: stack_frame.fix_operand(dst),
            })
        }
    }
}

// returns new source
fn movsx_src(src: PseudoOp, vec: &mut OpVec<X86>, sf: &mut StackFrame) -> Op {
    if let PseudoOp::Normal(Op::Imm(i)) = src {
        vec.push_one(X86::Mov {
            ty: AsmType::Longword,
            src: Op::Imm(i),
            dst: Op::Register(Register::R10),
        });

        Op::Register(Register::R10)
    } else {
        sf.fix_operand(src)
    }
}

use crate::codegen::assembly::x86::AsmType;
fn fix_binary(
    operator: Binary,
    op: PseudoOp,
    dst_op: PseudoOp,
    stack_frame: &mut StackFrame,
    instructions: &mut OpVec<X86>,
    ty: AsmType,
) {
    match (operator, op, dst_op) {
        // mult can't use a stack address as it's destination
        (Binary::Mult, op, PseudoOp::PseudoRegister(dst_name)) => {
            let op = stack_frame.fix_operand(op);
            let dst_op = stack_frame.fix_by_name(&dst_name);
            instructions.push([
                X86::mov(dst_op.clone(), Register::R11.into(), ty),
                X86::binary(operator, op, Register::R11.into(), ty),
                X86::mov(Register::R11.into(), dst_op, ty),
            ]);
        }

        (Binary::Add | Binary::Sub | Binary::Mult, PseudoOp::Normal(op @ Op::Imm(i)), dst_op)
            if i > i32::MAX as i64 =>
        {
            let dst = stack_frame.fix_operand(dst_op);
            instructions.push([
                X86::mov(op, Register::R10.into(), AsmType::Quadword),
                X86::binary(operator, Register::R10.into(), dst, ty),
            ])
        }
        // add and subtract can't operate with just addresses
        (
            Binary::Add | Binary::Sub | Binary::And | Binary::Xor | Binary::Or,
            PseudoOp::PseudoRegister(op_name),
            PseudoOp::PseudoRegister(dst_name),
        ) => {
            let src = stack_frame.fix_by_name(&op_name);
            let dst = stack_frame.fix_by_name(&dst_name);
            let src_type = stack_frame.var_type(&op_name).unwrap();
            instructions.push([
                X86::mov(src, Register::R10.into(), src_type),
                X86::binary(operator, Register::R10.into(), dst, ty),
            ]);
        }
        // normal case so the other one can be like the fallthrough
        (Binary::ShiftLeft | Binary::ShiftRight, PseudoOp::Normal(op @ Op::Imm(_)), dst) => {
            let dst = stack_frame.fix_operand(dst);
            instructions.push_one(X86::binary(operator, op, dst, ty));
        }
        (Binary::ShiftLeft | Binary::ShiftRight, op, dst) => {
            let shift_reg = Register::Cx;
            let op = stack_frame.fix_operand(op);
            let dst = stack_frame.fix_operand(dst);
            instructions.push([
                X86::mov(op, shift_reg.into(), ty),
                X86::binary(operator, Register::Cx.into(), dst, ty),
            ]);
        }
        // other cases: handle normally
        (operator, PseudoOp::PseudoRegister(op), PseudoOp::Normal(dst_op)) => {
            let op = stack_frame.fix_by_name(&op);
            instructions.push_one(X86::binary(operator, op, dst_op, ty));
        }
        (operator, PseudoOp::Normal(op), PseudoOp::PseudoRegister(dst_op)) => {
            let dst_op = stack_frame.fix_by_name(&dst_op);
            instructions.push_one(X86::binary(operator, op, dst_op, ty));
        }
        (operator, PseudoOp::Normal(op), PseudoOp::Normal(dst_op)) => {
            instructions.push_one(X86::binary(operator, op, dst_op, ty));
        }
    }
}
