use super::assembly;

use super::Binary;
use super::{Identifier, Program};
use crate::codegen::assembly::x86::AsmType;
use assembly::x86::op_regs as op;
use assembly::x86::OpPair;
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

    #[allow(dead_code)]
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
            PseudoOp::PseudoRegister(name) if rules.no_mem() => Err(self.fix_by_name(&name)),
            PseudoOp::Normal(op @ Op::Imm(i)) if rules.imm_not_allowed(i) => Err(op),
            op => Ok(self.fix_operand(op)),
        }
    }

    fn check_pair(&mut self, pair: OpPair<PseudoOp>, rules: PairSet) -> (RuleRes, RuleRes) {
        let left = self.check(pair.0, rules.left());
        let right = self.check(pair.1.clone(), rules.right());
        match (left, right) {
            (
                Ok(left @ (Op::Stack(_) | Op::Data(_))) | Err(left @ (Op::Stack(_) | Op::Data(_))),
                right @ Ok(Op::Stack(_) | Op::Data(_)),
            ) if rules.max_1_stack() => (Err(left), right),
            other => other,
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

pub mod rule;

use rule::{PairSet, RuleSet};

fn fix_instruction(op: Pseudo, sf: &mut StackFrame, vec: &mut OpVec<X86>) {
    use rule::RULES;
    match op {
        // push rule: needs dword/NoQuadImm
        Pseudo::Push(op) => match sf.check(op, RULES.push) {
            Ok(op) => vec.push_one(X86::Push(op)),
            Err(op) => vec.push([X86::mov(op, op::R10, AsmType::Quadword), X86::Push(op::R10)]),
        },
        Pseudo::Call(fun) => vec.push_one(X86::Call(fun)),
        Pseudo::Mov {
            ty: AsmType::Quadword,
            // problem i have right now, I want
            regs:
                (
                    PseudoOp::Normal(src @ Op::Imm(i)),
                    PseudoOp::PseudoRegister(name) | PseudoOp::Normal(Op::Data(name)),
                ),
        } if i > i32::MAX as i64 => vec.push([
            X86::mov(src, op::R10, AsmType::Quadword),
            X86::mov(op::R10, sf.fix_by_name(&name), AsmType::Quadword),
        ]),
        Pseudo::Mov { ty, regs } => {
            let (src, dst) = sf.check_pair(regs, RULES.mov);

            let src = match src {
                Ok(src) => src,
                Err(src) => {
                    vec.push_one(X86::mov(src, op::R10, ty));
                    Op::Register(Register::R10)
                }
            };
            match dst {
                Ok(dst) => vec.push_one(X86::mov(src.clone(), dst, ty)),
                Err(dst) => vec.push([X86::mov(src, op::R11, ty), X86::mov(op::R11, dst, ty)]),
            }
        }
        Pseudo::Unary {
            operator,
            operand: o,
            ty,
        } => match sf.check(o, RULES.unop) {
            Ok(operand) => vec.push_one(X86::Unary {
                operator,
                operand,
                ty,
            }),
            Err(o) => vec.push([
                X86::mov(o.clone(), op::R10, ty),
                X86::Unary {
                    operator,
                    operand: op::R10,
                    ty,
                },
                X86::mov(op::R10, o, ty),
            ]),
        },

        // see fix_binary
        Pseudo::Binary { operator, regs, ty } => {
            let (src, dst) = sf.check_pair(
                regs,
                match operator {
                    Binary::And | Binary::Xor | Binary::Or => RULES.bitwise,
                    Binary::Add => RULES.add,
                    Binary::Sub => RULES.sub,
                    Binary::Mult => RULES.mul,
                    Binary::ShiftLeft | Binary::ShiftRight => RULES.shift,
                },
            );

            let temp_op = Op::from(match operator {
                Binary::ShiftLeft | Binary::ShiftRight => Register::Cx,
                _ => Register::R10,
            });

            let src = match src {
                Ok(src) => src,
                Err(src) => {
                    vec.push_one(X86::mov(src, temp_op.clone(), ty));
                    temp_op
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
        Pseudo::Idiv { divisor, ty } => match sf.check(divisor, RULES.div) {
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
        Pseudo::Div { divisor, ty } => match sf.check(divisor, RULES.div) {
            Ok(divisor) => {
                vec.push_one(X86::Div { divisor, ty });
            }
            Err(divisor) => {
                let temp_register = Op::Register(Register::R10);
                vec.push([
                    X86::mov(divisor, temp_register.clone(), ty),
                    X86::Div {
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
            let (l, r) = sf.check_pair(regs, RULES.cmp);
            let l = match l {
                Ok(l) => l,
                Err(l) => {
                    vec.push_one(X86::mov(l, Register::R10.into(), ty));
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
            let (src, dst) = sf.check_pair(regs, RULES.movsx);

            let src = match src {
                Ok(src) => src,
                Err(src) => {
                    vec.push_one(X86::mov(src, Register::R10.into(), ty));
                    Register::R10.into()
                }
            };
            match dst {
                Ok(dst) => vec.push_one(X86::movsx(src, dst)),
                Err(dst) => vec.push([
                    X86::movsx(src, Register::R11.into()),
                    X86::mov(Register::R11.into(), dst, AsmType::Quadword),
                ]),
            }
        }
        Pseudo::Movzx {
            ty,
            regs: (src, PseudoOp::Normal(op @ Op::Register(_))),
        } => {
            let src = match sf.check(src, RULES.div) {
                Ok(src) => src,
                Err(src) => {
                    vec.push_one(X86::mov(src, Register::R10.into(), ty));
                    Register::R10.into()
                }
            };
            vec.push_one(X86::mov(src, op, AsmType::Longword))
        }
        Pseudo::Movzx { ty, regs } => {
            let (src, dst) = sf.check_pair(regs, RULES.movsx);

            let src = match src {
                Ok(src) => src,
                Err(src) => {
                    vec.push_one(X86::mov(src, Register::R10.into(), ty));
                    Register::R10.into()
                }
            };
            let (Ok(dst) | Err(dst)) = dst;
            vec.push([
                X86::mov(src, R11, AsmType::Longword),
                X86::mov(R11, dst, AsmType::Quadword),
            ])
        }
    }
}
use assembly::x86::op_regs::*;

// no mem dst
// no quad immediate
