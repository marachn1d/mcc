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

use assembly::x86::OpPair;
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

    for op in body.into_iter() {
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

struct StackFrame<'a> {
    map: HashMap<Identifier, isize>,
    size: usize,
    table: &'a SymbolTable,
}

struct BlockState<'a> {
    vec: &'a mut OpVec<X86>,
    sf: StackFrame<'a>,
}

struct NeedsReg(Op);

const fn align_quadword(size: usize) -> usize {
    let remainder = (size + 8) % 16;
    if remainder == 0 {
        size
    } else {
        // should align to 16
        size + (16 - remainder)
    }
}

type RuleRes = std::result::Result<Op, Op>;

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

    fn check(&mut self, op: PseudoOp, rules: RuleSet) -> RuleRes {
        match op {
            op if rules.none() => Ok(self.fix_operand(op)),
            PseudoOp::PseudoRegister(name) if rules.no_stack() => Err(self.fix_by_name(&name)),
            PseudoOp::Normal(op @ Op::Imm(i)) if rules.imm_allowed(i) => Err(op),
            op => Ok(self.fix_operand(op)),
        }
    }

    fn check_pair(&mut self, pair: OpPair<PseudoOp>, rules: PairSet) -> (RuleRes, RuleRes) {
        let left = self.check(pair.0, rules.left());
        let right = self.check(pair.1.clone(), rules.right());
        if let (left @ Ok(Op::Stack(_) | Op::Data(_)), Ok(right @ (Op::Stack(_) | Op::Data(_)))) =
            (left.clone(), right.clone())
        {
            (left, Err(right))
        } else {
            (left, right)
        }
    }

    fn fix_pair(&mut self, (l, r): (PseudoOp, PseudoOp)) -> (RuleRes, RuleRes) {
        (Ok(self.fix_operand(l)), Ok(self.fix_operand(r)))
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

mod rule;

use rule::{PairSet, RuleSet};

fn mov_to_reg(op: Op, dst: Register, ty: AsmType, vec: &mut OpVec<X86>) {
    vec.push_one(X86::mov(op, dst.into(), ty))
}

fn mov_pseudo(op: PseudoOp, dst: Register, ty: AsmType, sf: &mut StackFrame, vec: &mut OpVec<X86>) {
    mov_to_reg(sf.fix_operand(op), dst, ty, vec);
}

fn fix_instruction(op: Pseudo, sf: &mut StackFrame, vec: &mut OpVec<X86>) {
    match op {
        // push rule: needs dword/NoQuadImm
        Pseudo::Push(op) => match sf.check(op, rule::PUSH) {
            Ok(op) => vec.push_one(X86::Push(op)),
            Err(op) => vec.push([
                X86::mov(op, Register::R10.into(), AsmType::Quadword),
                X86::Push(Register::R10.into()),
            ]),
        },
        Pseudo::Call(fun) => vec.push_one(X86::Call(fun)),
        Pseudo::Mov { ty, regs } => {
            // mov has no rule for dst so we can unwrap here
            let (src, dst) = sf.check_pair(regs, rule::MOV);

            let src = match src {
                Ok(src) => src,
                Err(src) => {
                    mov_to_reg(src, Register::R10, AsmType::Quadword, vec);
                    Op::Register(Register::R10)
                }
            };

            vec.push_one(X86::mov(src, dst.unwrap(), ty))
        }
        Pseudo::Unary {
            operator,
            operand: o,
            ty,
        } => {
            vec.push_one(X86::Unary {
                operator,
                operand: sf.fix_operand(o),
                ty,
            });
        }
        // see fix_binary
        Pseudo::Binary { operator, regs, ty } => {
            let (src, dst) = sf.check_pair(
                regs,
                match operator {
                    Binary::And
                    | Binary::Or
                    | Binary::Add
                    | Binary::Sub
                    | Binary::Xor
                    | Binary::Mult => rule::BIN_ADDSUB,
                    Binary::ShiftLeft | Binary::ShiftRight => rule::BIN_BITSHIFT,
                },
            );

            let src = match src {
                Ok(src) => src,
                Err(src) => {
                    X86::mov(src, Register::R10.into(), ty);
                    Register::R10.into()
                }
            };

            match dst {
                Ok(dst) => vec.push_one(X86::binary(operator, src, dst, ty)),
                Err(dst) => vec.push([
                    X86::mov(dst.clone(), Register::R11.into(), ty),
                    X86::binary(operator, src, Register::R11.into(), ty),
                    X86::mov(Register::R11.into(), dst, ty),
                ]),
            }
        }

        // can't be an immediate
        Pseudo::Idiv { divisor, ty } => match sf.check(divisor, rule::IDIV) {
            Ok(divisor) => {
                vec.push_one(X86::Idiv { divisor, ty });
            }
            Err(divisor) => {
                let temp_register = Op::Register(Register::R10);
                vec.push([
                    X86::mov(divisor, temp_register.clone(), ty),
                    X86::Idiv {
                        divisor: temp_register,
                        ty,
                    },
                ]);
            }
        },
        Pseudo::Ret => vec.push_one(X86::Ret),
        Pseudo::Cdq(ty) => vec.push_one(X86::Cdq(ty)),
        // cmp is no quad, one mem, need some kinda bitfield or smth
        Pseudo::Cmp { regs, ty } => {
            let (l, r) = sf.check_pair(regs, rule::CMP);
            let l = match l {
                Ok(l) => l,
                Err(l) => {
                    X86::mov(l, Register::R10.into(), ty);
                    Register::R10.into()
                }
            };
            match r {
                Ok(r) => vec.push_one(X86::cmp(l, r, ty)),
                Err(r) => vec.push([
                    X86::mov(r, Register::R11.into(), ty),
                    X86::cmp(l, Register::R11.into(), ty),
                ]),
            }
        }
        Pseudo::Jmp(label) => vec.push_one(X86::Jmp(label)),
        Pseudo::Label(name) => vec.push_one(X86::Label(name)),
        Pseudo::JmpCC { condition, label } => vec.push_one(X86::JmpCC { condition, label }),
        Pseudo::SetCC { condition, op: o } => vec.push_one(X86::SetCC {
            condition,
            op: sf.fix_operand(o),
        }),
        // no stack dst
        // no imm src
        Pseudo::Movsx {
            ty,
            regs, //src,
                  //dst: PseudoOp::PseudoRegister(p),
        } => {
            let (src, dst) = sf.check_pair(regs, rule::MOVSX);
            let src = match src {
                Ok(src) => src,
                Err(src) => {
                    X86::mov(src, Register::R10.into(), ty);
                    Register::R10.into()
                }
            };
            match dst {
                Ok(dst) => vec.push_one(X86::movsx(src, dst)),
                Err(dst) => vec.push([
                    X86::mov(dst.clone(), Register::R11.into(), ty),
                    X86::movsx(src, Register::R11.into()),
                    X86::mov(Register::R11.into(), dst, AsmType::Quadword),
                ]),
            }
        }
    }
}

// no mem dst
// no quad immediate

use crate::codegen::assembly::x86::AsmType;
// no stack dst
// max 1 addr for add sub ^ |
// bitshift: no Imm src
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
