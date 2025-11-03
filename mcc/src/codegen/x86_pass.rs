use asm::x86::{
    op_regs as op, AsmType, Binary, FunctionDefinition, Op, Program, Pseudo, PseudoOp, Register,
    TopLevel, X86,
};
use ast::Ident;

use asm::x86::{BackendSymbol, BackendTable};
use std::collections::HashMap;
use std::mem::MaybeUninit;
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

// tbh I'm not the happiest with this code, it's not really clear, but it's very dedpulicated and
// it's kinda done so I figure i should just let it be like this for now
fn fix_instruction(op: Pseudo, sf: &mut StackFrame, vec: &mut Vec<X86>) {
    //eprintln!("fixing {op:?}");
    if let Some(rule) = PairRule::rule(&op) {
        let l_rule = rule.left();

        let mut l_op = sf.fix_operand(l_rule.operand().clone());
        let mut r_op = None;
        let mut r_dst = None;

        if l_rule.needs_cl() {
            vec.push(X86::mov(l_op.clone(), op::CX, op.ty().unwrap()));
            l_op = op::CX;
        } else if l_rule.needs_fix() {
            let ty = if matches!(op, Pseudo::Movsx { .. }) {
                AsmType::Longword
            } else {
                op.ty().unwrap()
            };
            vec.push(X86::mov(l_op.clone(), op::R10, ty));
            l_op = op::R10;
        }

        if let Some(r_rule) = rule.right() {
            let r = sf.fix_operand(r_rule.operand().clone());
            if r_rule.needs_fix() || (l_op.is_memory() && r.is_memory() && rule.max_1_stack()) {
                vec.push(X86::mov(r.clone(), op::R11, op.ty().unwrap()));
                if rule.r_is_dst() {
                    r_dst = Some(r.clone())
                }
                r_op = Some(op::R11);
            } else {
                r_op = Some(r);
            }
        }
        let fixed = if let Some(r_op) = r_op.clone() {
            op.as_fixed(&[l_op.clone(), r_op])
        } else {
            op.as_fixed(&[l_op.clone()])
        };
        vec.push(fixed);

        if let Some(r_dst) = r_dst {
            vec.push(X86::mov(r_op.unwrap(), r_dst, op.ty().unwrap()))
        }
    } else {
        let fixed_regs: Box<[Op]> = op
            .regs()
            .into_iter()
            .map(|x| sf.fix_operand(x.clone()))
            .collect();
        vec.push(op.as_fixed(&fixed_regs))
    }
    //eprintln!("result: {:?}", &vec[end_idx..]);
}

// no mem dst
// no quad immediate
