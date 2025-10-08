use asm::x86::{Binary, Pseudo, PseudoOp};

#[derive(Copy, Clone, Debug)]
pub struct OpRule<'a> {
    pub no_memory: bool,
    pub max_dword: bool,
    pub no_immediate: bool,
    pub op: &'a PseudoOp,
}

#[derive(Copy, Clone, Debug)]
pub struct OpRuleBuilder {
    pub no_memory: bool,
    pub max_dword: bool,
    pub no_immediate: bool,
}

#[derive(Copy, Clone, Debug)]
pub enum PairRule<'a> {
    Unary(OpRule<'a>),
    Binary {
        left: OpRule<'a>,
        right: OpRule<'a>,
        max_1_stack: bool,
    },
}

impl OpRuleBuilder {
    const fn none() -> Self {
        Self {
            no_memory: false,
            max_dword: false,
            no_immediate: false,
        }
    }

    const fn no_memory() -> Self {
        let mut new = Self::none();
        new.no_memory = true;
        new
    }

    const fn max_dword() -> Self {
        let mut new = Self::none();
        new.max_dword = true;
        new
    }

    const fn no_immediate() -> Self {
        let mut new = Self::none();
        new.no_immediate = true;
        new
    }

    const fn and(&self, other: Self) -> Self {
        Self {
            no_memory: self.no_memory | other.no_memory,
            max_dword: self.max_dword | other.max_dword,
            no_immediate: self.no_immediate | other.no_immediate,
        }
    }

    const fn build<'a>(self, op: &'a PseudoOp) -> OpRule<'a> {
        OpRule {
            no_memory: self.no_memory,
            no_immediate: self.no_immediate,
            max_dword: self.max_dword,
            op,
        }
    }

    const fn unary<'a>(self, op: &'a PseudoOp) -> PairRule<'a> {
        PairRule::Unary(self.build(op))
    }
}

impl<'a> OpRule<'a> {
    const fn with(self, other: Self) -> PairRule<'a> {
        PairRule::Binary {
            left: self,
            right: other,
            max_1_stack: false,
        }
    }

    pub const fn no_memory(&self) -> bool {
        self.no_memory
    }

    pub const fn max_dword(&self) -> bool {
        self.max_dword
    }

    pub const fn no_immediate(&self) -> bool {
        self.no_immediate
    }

    pub const fn operand(&self) -> &PseudoOp {
        self.op
    }
}

impl<'a> PairRule<'a> {
    pub const fn rule(instruction: &'a Pseudo) -> Option<Self> {
        const fn no_memory() -> OpRuleBuilder {
            OpRuleBuilder::no_memory()
        }

        const fn no_immediate() -> OpRuleBuilder {
            OpRuleBuilder::no_immediate()
        }

        const fn max_dword() -> OpRuleBuilder {
            OpRuleBuilder::max_dword()
        }

        const fn binary<'a>(
            left: &'a PseudoOp,
            left_rule: OpRuleBuilder,
            right: &'a PseudoOp,
            right_rule: OpRuleBuilder,
        ) -> PairRule<'a> {
            let left = left_rule.build(left);
            let right = right_rule.build(right);
            left.with(right)
        }

        match instruction {
            Pseudo::Mov {
                regs: (left, right),
                ..
            }
            | Pseudo::Binary {
                operator: Binary::ShiftLeft | Binary::ShiftRight,
                regs: (left, right),
                ..
            } => Some(binary(left, no_memory(), right, no_immediate()).max_1_stack()),
            Pseudo::Movsx {
                regs: (left, right),
                ..
            } => Some(binary(left, no_immediate(), right, no_immediate())),
            Pseudo::Idiv { divisor: op, .. }
            | Pseudo::Div { divisor: op, .. }
            | Pseudo::Unary { operand: op, .. } => Some(no_immediate().unary(op)),

            Pseudo::Binary {
                operator: Binary::And | Binary::Or | Binary::Xor,
                regs: (left, right),
                ..
            }
            | Pseudo::Cmp {
                regs: (left, right),
                ..
            }
            | Pseudo::Binary {
                operator: Binary::Add | Binary::Sub,
                regs: (left, right),
                ..
            } => Some(binary(left, max_dword(), right, no_immediate()).max_1_stack()),
            Pseudo::Binary {
                operator: Binary::Mult,
                regs: (left, right),
                ..
            } => Some(binary(left, max_dword(), right, no_memory()).max_1_stack()),

            Pseudo::Push(op) => Some(max_dword().unary(op)),

            Pseudo::MovZeroExtend(_) => todo!(),
            Pseudo::Call(_)
            | Pseudo::Ret
            | Pseudo::Cdq(_)
            | Pseudo::Jmp(_)
            | Pseudo::JmpCC { .. }
            | Pseudo::SetCC { .. }
            | Pseudo::Label(_) => None,
        }
    }

    const fn left(&self) -> OpRule {
        match self {
            PairRule::Unary(left) | PairRule::Binary { left, .. } => *left,
        }
    }

    const fn unary(rule: OpRule<'a>) -> Self {
        Self::Unary(rule)
    }

    const fn max_1_stack(&self) -> Self {
        if let PairRule::Binary {
            left,
            right,
            max_1_stack: false,
        } = self
        {
            Self::Binary {
                left: *left,
                right: *right,
                max_1_stack: true,
            }
        } else {
            *self
        }
    }
}

/*
impl RuleTable {
    const ADD_SUB_CMP: PairSet = pair_set(MemRule::One, MAX_DWORD, NO_IMM);
}

pub const RULES: RuleTable = RuleTable {
    mov: pair_set(MemRule::One, NO_MEM, NO_IMM),
    movsx: pair_set(MemRule::Two, NO_IMM, NO_IMM.or(NO_MEM)),
    add: RuleTable::ADD_SUB_CMP,
    sub: RuleTable::ADD_SUB_CMP,
    mul: pair_set(MemRule::One, MAX_DWORD, NO_MEM),
    cmp: RuleTable::ADD_SUB_CMP,
    bitwise: pair_set(MemRule::One, MAX_DWORD, NO_IMM),
    unop: NO_IMM,
    div: NO_IMM,
    shift: pair_set(MemRule::One, NO_MEM, NO_IMM),
    push: MAX_DWORD,
};
*/
