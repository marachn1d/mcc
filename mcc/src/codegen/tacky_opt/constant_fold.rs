use super::{Instruction, StaticInit, Value, VarType};

pub fn constant_fold(tacky: &mut Vec<Instruction>) -> bool {
    let mut changed = false;
    tacky.retain_mut(|instruction| fold_instruction(instruction, &mut changed));
    changed
}

fn fold_instruction(instruction: &mut Instruction, changed: &mut bool) -> bool {
    use Value::Constant;
    const KEEP: bool = true;
    const REMOVE: bool = false;

    // default to true, set false in catchall
    let mut should_change = true;

    let should_keep = match instruction {
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
            KEEP
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
            KEEP
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
            KEEP
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
            KEEP
        }
        Instruction::JumpIfZero {
            condition: Constant(c),
            target,
        } => {
            if c.is_zero() {
                *instruction = Instruction::Jump {
                    target: target.clone(),
                };
                KEEP
            } else {
                REMOVE
            }
        }
        Instruction::JumpIfNotZero {
            condition: Constant(c),
            target,
        } => {
            if c.is_zero() {
                REMOVE
            } else {
                *instruction = Instruction::Jump {
                    target: target.clone(),
                };
                KEEP
            }
        }

        _ => {
            should_change = false;
            KEEP
        }
    };
    *changed |= should_change;
    should_keep
}
