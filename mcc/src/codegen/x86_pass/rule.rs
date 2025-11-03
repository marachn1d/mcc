use asm::x86::{Binary, Op, Pseudo, PseudoOp};

#[derive(Copy, Clone, Debug)]
pub struct OpRule<'a> {
    pub no_memory: bool,
    pub max_dword: bool,
    pub no_immediate: bool,
    pub imm_or_cl: bool,
    pub op: &'a PseudoOp,
}

#[derive(Copy, Clone, Debug)]
pub struct OpRuleBuilder {
    pub no_memory: bool,
    pub max_dword: bool,
    pub no_immediate: bool,
    pub imm_or_cl: bool,
}

#[derive(Copy, Clone, Debug)]
pub enum PairRule<'a> {
    Unary(OpRule<'a>),
    Binary {
        left: OpRule<'a>,
        right: OpRule<'a>,
        max_1_stack: bool,
        r_is_dst: bool,
    },
}

impl OpRuleBuilder {
    const fn none() -> Self {
        Self {
            no_memory: false,
            max_dword: false,
            no_immediate: false,
            imm_or_cl: false,
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

    const fn imm_or_cl() -> Self {
        let mut new = Self::none();
        new.imm_or_cl = true;
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
            imm_or_cl: self.imm_or_cl | other.imm_or_cl,
        }
    }

    const fn build<'a>(self, op: &'a PseudoOp) -> OpRule<'a> {
        OpRule {
            no_memory: self.no_memory,
            no_immediate: self.no_immediate,
            max_dword: self.max_dword,
            op,
            imm_or_cl: self.imm_or_cl,
        }
    }

    const fn unary<'a>(self, op: &'a PseudoOp) -> PairRule<'a> {
        PairRule::Unary(self.build(op))
    }
}

impl<'a> OpRule<'a> {
    const fn with(self, other: Self, r_is_dst: bool) -> PairRule<'a> {
        PairRule::Binary {
            left: self,
            right: other,
            max_1_stack: false,
            r_is_dst,
        }
    }

    pub const fn needs_fix(&self) -> bool {
        if let PseudoOp::Normal(Op::Imm(i)) = self.operand() {
            self.no_immediate() || (self.max_dword() && i.above_dword())
        } else {
            self.no_memory()
        }
    }

    pub const fn needs_cl(&self) -> bool {
        if let PseudoOp::Normal(Op::Imm(_)) = self.operand() {
            false
        } else {
            self.imm_or_cl()
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

    pub const fn imm_or_cl(&self) -> bool {
        self.imm_or_cl
    }

    pub const fn operand(&self) -> &PseudoOp {
        self.op
    }
}

impl<'a> PairRule<'a> {
    pub const fn r_is_dst(&self) -> bool {
        matches!(self, Self::Binary { r_is_dst: true, .. })
    }

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

        const fn imm_or_cl() -> OpRuleBuilder {
            OpRuleBuilder::imm_or_cl()
        }

        const fn binary<'a>(
            left: &'a PseudoOp,
            left_rule: OpRuleBuilder,
            right: &'a PseudoOp,
            right_rule: OpRuleBuilder,
            l_is_dst: bool,
        ) -> PairRule<'a> {
            let left = left_rule.build(left);
            let right = right_rule.build(right);
            left.with(right, l_is_dst)
        }

        const fn binary_r_dst<'a>(
            left: &'a PseudoOp,
            left_rule: OpRuleBuilder,
            right: &'a PseudoOp,
            right_rule: OpRuleBuilder,
        ) -> PairRule<'a> {
            binary(left, left_rule, right, right_rule, true)
        }

        const fn binary_no_dst<'a>(
            left: &'a PseudoOp,
            left_rule: OpRuleBuilder,
            right: &'a PseudoOp,
            right_rule: OpRuleBuilder,
        ) -> PairRule<'a> {
            binary(left, left_rule, right, right_rule, false)
        }

        match instruction {
            Pseudo::Mov {
                regs: (left, right),
                ..
            } => Some(binary_r_dst(left, no_memory(), right, no_immediate()).with_max_1_stack()),
            Pseudo::Binary {
                operator: Binary::ShiftLeft | Binary::ShiftRight,
                regs: (left, right),
                ..
            } => Some(binary_r_dst(
                left,
                imm_or_cl(),
                right,
                no_immediate().and(no_memory()),
            )),
            Pseudo::Movsx {
                regs: (left, right),
                ..
                    // we only emit movsx when doing a casting operation, in which case the emitted
                    // tacky accounts for the new destination
            } => Some(binary_no_dst(left, no_immediate(), right, no_immediate().and(no_memory())).with_max_1_stack()),
            Pseudo::Cmp {
                regs: (left, right),
                ..
            } => Some(binary_no_dst(left, max_dword(), right, no_immediate()).with_max_1_stack()),
            Pseudo::Idiv { divisor: op, .. }
            | Pseudo::Div { divisor: op, .. }
            | Pseudo::Unary { operand: op, .. } => Some(no_immediate().unary(op)),

            Pseudo::Binary {
                operator: Binary::And | Binary::Or | Binary::Xor,
                regs: (left, right),
                ..
            }
            | Pseudo::Binary {
                operator: Binary::Add | Binary::Sub,
                regs: (left, right),
                ..
            } => Some(binary_r_dst(left, max_dword(), right, no_immediate()).with_max_1_stack()),
            Pseudo::Binary {
                operator: Binary::Mult,
                regs: (left, right),
                ..
            } => Some(binary_r_dst(left, max_dword(), right, no_memory()).with_max_1_stack()),

            Pseudo::Push(op) => Some(max_dword().unary(op)),

            Pseudo::MovZeroExtend((left, right)) => {
                Some(binary_r_dst(left, no_immediate(), right, no_immediate()))
            }
            Pseudo::Call(_)
            | Pseudo::Ret
            | Pseudo::Cdq(_)
            | Pseudo::Jmp(_)
            | Pseudo::JmpCC { .. }
            | Pseudo::SetCC { .. }
            | Pseudo::Label(_) => None,
        }
    }

    pub const fn left(&self) -> OpRule<'_> {
        match self {
            PairRule::Unary(left) | PairRule::Binary { left, .. } => *left,
        }
    }

    pub const fn right(&self) -> Option<OpRule<'_>> {
        match self {
            PairRule::Unary(_) => None,
            PairRule::Binary { right, .. } => Some(*right),
        }
    }

    pub const fn unary(rule: OpRule<'a>) -> Self {
        Self::Unary(rule)
    }

    pub const fn max_1_stack(&self) -> bool {
        if let Self::Binary { max_1_stack, .. } = self {
            *max_1_stack
        } else {
            false
        }
    }

    pub const fn with_max_1_stack(&self) -> Self {
        if let PairRule::Binary {
            left,
            right,
            max_1_stack: false,
            r_is_dst,
        } = self
        {
            Self::Binary {
                left: *left,
                right: *right,
                max_1_stack: true,
                r_is_dst: *r_is_dst,
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
