use super::{Instruction, StaticInit, Value, VarType};

pub fn constant_fold(tacky: &mut [Instruction]) -> bool {
    let mut changed = false;
    for instruction in tacky {
        use Value::Constant;
        changed |= match instruction {
            Instruction::SignExtend {
                src: Constant(s),
                dst: d,
            } => {
                let src: StaticInit = (*s).into();
                let src: ast::Constant = src.as_long().into();
                *instruction = Instruction::Copy {
                    src: Constant(src),
                    dst: d.clone(),
                };
                true
            }
            Instruction::Truncate {
                src: Constant(s),
                dst: d,
            } => {
                let src: StaticInit = (*s).into();
                let src: ast::Constant = src.as_int().into();
                *instruction = Instruction::Copy {
                    src: Constant(src),
                    dst: d.clone(),
                };
                true
            }

            Instruction::Unary {
                op,
                source: Constant(s),
                dst,
            } => {
                *instruction = Instruction::Copy {
                    dst: Value::Var(dst.clone()),
                    src: Value::Constant(s.with_unop(*op)),
                };
                true
            }
            Instruction::Binary {
                operator,
                source_1: Constant(s1),
                source_2: Constant(s2),
                dst,
            } => {
                let [s1, s2]: [StaticInit; 2] = [*s1, *s2].map(|x| x.into());
                let ty = s1.common_type(&s2);
                let src = match ty {
                    VarType::Int(_) => Value::Constant(ast::Constant::new_int(
                        operator.apply(s1.as_int(), s2.as_int()),
                    )),
                    VarType::Long(_) => Value::Constant(ast::Constant::new_long(
                        operator.apply(s1.as_long(), s2.as_long()),
                    )),
                };
                *instruction = Instruction::Copy {
                    src,
                    dst: dst.clone(),
                };
                true
            }
            Instruction::JumpIfZero {
                condition: Constant(c),
                target,
            } if c.is_zero() => {
                *instruction = Instruction::Jump {
                    target: target.clone(),
                };
                true
            }
            // need to replace the boxed slice with a vec for this
            Instruction::JumpIfNotZero {
                condition: Constant(_c),
                target: _t,
            } => false,

            _ => false,
        }
    }
    changed
}
