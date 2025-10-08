use asm::x86::{
    op_regs as op, AsmType, Binary, FunctionDefinition, Op, Program, Pseudo, PseudoOp, Register,
    TopLevel, X86,
};
use ast::Ident;

use asm::x86::{BackendSymbol, BackendTable};
use std::collections::HashMap;
pub fn fix_ast(program: Program<Pseudo>, table: &BackendTable) -> Program<X86> {
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
    table: &BackendTable,
) -> FunctionDefinition<X86> {
    let mut stack_frame = StackFrame::new(table);
    let mut body_vec: Vec<X86> = Vec::with_capacity(body.len() + 1);
    body_vec.push(X86::allocate_stack(0));

    for op in body.into_iter() {
        fix_instruction(op, &mut stack_frame, &mut body_vec);
    }

    if stack_frame.size == 0 {
        // eww eww eww sorry sorry sorry
        body_vec.remove(0);
    } else {
        body_vec[0] = X86::allocate_stack(stack_frame.rounded_size());
    }

    FunctionDefinition {
        name,
        params,
        body: body_vec.into(),
        global,
    }
}

struct StackFrame<'a> {
    map: HashMap<Ident, isize>,
    size: usize,
    table: &'a BackendTable,
}

const fn align_quadword(size: usize) -> usize {
    const QWORD: usize = 8;
    let new_size = size + QWORD;
    // round down to next multiple of 8
    match new_size % 16 {
        0 => QWORD,
        remainder => QWORD + (16 - remainder),
    }
}

type RuleRes = std::result::Result<Op, Op>;

impl<'a> StackFrame<'a> {
    fn new(table: &'a BackendTable) -> Self {
        Self {
            map: HashMap::new(),
            size: 0,
            table,
        }
    }

    #[allow(dead_code)]
    fn var_type(&self, ident: &Ident) -> Option<AsmType> {
        self.table
            .get(ident)
            .and_then(|input| match input {
                BackendSymbol::Obj { ty, .. } => Some(ty),
                BackendSymbol::Fn { .. } => None,
            })
            .copied()
    }

    fn alloc(&mut self, ty: &AsmType, ident: &Ident) -> Op {
        let size = match ty {
            AsmType::Longword => 4,
            AsmType::Quadword => align_quadword(self.size),
        };

        self.size += size;

        let offset = -(self.size as isize);

        self.map.insert(ident.clone(), offset);
        Op::Stack(offset)
    }

    fn get(&mut self, ident: &Ident) -> Op {
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
                } => self.alloc(ty, ident),
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

    fn check_pair(&mut self, pair: (PseudoOp, PseudoOp), rules: PairSet) -> (RuleRes, RuleRes) {
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

    fn fix_by_name(&mut self, name: &Ident) -> Op {
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
use rule::PairRule;

fn fix_instruction(mut op: Pseudo, sf: &mut StackFrame, vec: &mut Vec<X86>) {
    // give Pseudo an associated function to return a constructor for normal x86
    if let Some(rule) = PairRule::rule(&op) {
        match rule {
            PairRule::Unary(op_rule) => todo!(),
            PairRule::Binary {
                left,
                right,
                max_1_stack,
            } => todo!(),
        }
    }

    /*
        match op {
            // push rule: needs dword/NoQuadImm
            Pseudo::Push(op) => match sf.check(op, RULES.push) {
                Ok(op) => vec.push(X86::Push(op)),
                Err(op) => vec.extend([X86::mov(op, op::R10, AsmType::Quadword), X86::Push(op::R10)]),
            },
            Pseudo::Call(fun) => vec.push(X86::Call(fun)),
            Pseudo::Mov {
                // no quadword immediates
                ty: AsmType::Quadword,
                regs:
                    (
                        PseudoOp::Normal(src @ Op::UImm(u)),
                        PseudoOp::PseudoRegister(name) | PseudoOp::Normal(Op::Data(name)),
                    ),
            } if u > u32::MAX as u64 => vec.extend([
                X86::mov(src, op::R10, AsmType::Quadword),
                X86::mov(op::R10, sf.fix_by_name(&name), AsmType::Quadword),
            ]),

            Pseudo::Mov {
                ty: AsmType::Quadword,
                // problem i have right now, I want
                regs:
                    (
                        PseudoOp::Normal(src @ Op::Imm(i)),
                        PseudoOp::PseudoRegister(name) | PseudoOp::Normal(Op::Data(name)),
                    ),
            } if i > i32::MAX as i64 => vec.extend([
                X86::mov(src, op::R10, AsmType::Quadword),
                X86::mov(op::R10, sf.fix_by_name(&name), AsmType::Quadword),
            ]),
            Pseudo::Mov { ty, regs } => {
                let (src, dst) = sf.check_pair(regs, RULES.mov);

                let src = match src {
                    Ok(src) => src,
                    Err(src) => {
                        vec.push(X86::mov(src, op::R10, ty));
                        Op::Register(Register::R10)
                    }
                };
                match dst {
                    Ok(dst) => vec.push(X86::mov(src.clone(), dst, ty)),
                    Err(dst) => vec.extend([X86::mov(src, op::R11, ty), X86::mov(op::R11, dst, ty)]),
                }
            }
            Pseudo::Unary {
                operator,
                operand: o,
                ty,
            } => match sf.check(o, RULES.unop) {
                Ok(operand) => vec.push(X86::Unary {
                    operator,
                    operand,
                    ty,
                }),
                Err(o) => vec.extend([
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
                        vec.push(X86::mov(src, temp_op.clone(), ty));
                        temp_op
                    }
                };

                match dst {
                    Ok(dst) => vec.push(X86::binary(operator, src, dst, ty)),
                    Err(dst) => vec.extend([
                        X86::mov(dst.clone(), Register::R11.into(), ty),
                        X86::binary(operator, src, Register::R11.into(), ty),
                        X86::mov(Register::R11.into(), dst, ty),
                    ]),
                }
            }

            // can't be an immediate
            Pseudo::Idiv { divisor, ty } => fix_div(divisor, ty, sf, vec, true),
            Pseudo::Div { divisor, ty } => fix_div(divisor, ty, sf, vec, false),
            Pseudo::Ret => vec.push(X86::Ret),
            Pseudo::Cdq(ty) => vec.push(X86::Cdq(ty)),
            // cmp is no quad, one mem, need some kinda bitfield or smth
            Pseudo::Cmp { regs, ty } => {
                let (l, r) = sf.check_pair(regs, RULES.cmp);
                let l = match l {
                    Ok(l) => l,
                    Err(l) => {
                        vec.push(X86::mov(l, Register::R10.into(), ty));
                        Register::R10.into()
                    }
                };
                match r {
                    Ok(r) => vec.push(X86::cmp(l, r, ty)),
                    Err(r) => vec.extend([
                        X86::mov(r, Register::R11.into(), ty),
                        X86::cmp(l, Register::R11.into(), ty),
                    ]),
                }
            }
            Pseudo::Jmp(label) => vec.push(X86::Jmp(label)),
            Pseudo::Label(name) => vec.push(X86::Label(name)),
            Pseudo::JmpCC { condition, label } => vec.push(X86::JmpCC { condition, label }),
            Pseudo::SetCC { condition, op: o } => vec.push(X86::SetCC {
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
                        vec.push(X86::mov(src, Register::R10.into(), ty));
                        Register::R10.into()
                    }
                };
                match dst {
                    Ok(dst) => vec.push(X86::movsx(src, dst)),
                    Err(dst) => vec.extend([
                        X86::movsx(src, Register::R11.into()),
                        X86::mov(Register::R11.into(), dst, AsmType::Quadword),
                    ]),
                }
            }
            Pseudo::MovZeroExtend((src, dst)) => {
                if dst.is_register() {
                    vec.push(X86::mov(
                        sf.fix_operand(src),
                        sf.fix_operand(dst),
                        AsmType::Longword,
                    ))
                } else {
                    let (src, dst) = (sf.fix_operand(src), sf.fix_operand(dst));
                    vec.extend([
                        X86::mov(src, Register::R11.into(), AsmType::Longword),
                        X86::mov(Register::R11.into(), dst, AsmType::Quadword),
                    ])
                }
            }
        }
    */
}

/*
fn fix_div(divisor: PseudoOp, ty: AsmType, sf: &mut StackFrame, vec: &mut Vec<X86>, sign: bool) {
    match sf.check(divisor, RULES.div) {
        Ok(divisor) => {
            vec.push(X86::Idiv { divisor, ty });
        }
        Err(divisor) => {
            let temp_register = Op::Register(Register::R10);
            vec.extend([
                X86::mov(divisor, temp_register.clone(), ty),
                if sign {
                    X86::Idiv {
                        divisor: temp_register,
                        ty,
                    }
                } else {
                    X86::Div {
                        divisor: temp_register,
                        ty,
                    }
                },
            ]);
        }
    }
}
*/

// no mem dst
// no quad immediate
