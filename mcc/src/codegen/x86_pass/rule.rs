use asm::x86::{Binary, X86};
use std::ops::BitOr;

#[derive(Copy, Clone, Debug)]
struct OpRule {
    no_memory: bool,
    max_dword: bool,
    no_immediate: bool,
}

#[derive(Copy, Clone, Debug)]
enum PairRule {
    Unary(OpRule),
    Binary {
        left: OpRule,
        right: OpRule,
        max_1_stack: bool,
    },
}

impl OpRule {
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

    const fn with(&self, other: Self) -> PairRule {
        PairRule::Binary {
            left: *self,
            right: other,
            max_1_stack: false,
        }
    }
    const fn unary(&self) -> PairRule {
        PairRule::Unary(*self)
    }
}

impl PairRule {
    const fn rule(instruction: &X86) -> Option<Self> {
        const fn no_memory() -> OpRule {
            OpRule::no_memory()
        }

        const fn no_immediate() -> OpRule {
            OpRule::no_immediate()
        }

        const fn max_dword() -> OpRule {
            OpRule::max_dword()
        }

        match instruction {
            asm::x86::BaseX86::Mov { .. }
            | asm::x86::BaseX86::Binary {
                operator: Binary::ShiftLeft | Binary::ShiftRight,
                ..
            } => Some(no_memory().with(no_immediate()).max_1_stack()),
            asm::x86::BaseX86::Movsx { .. } => {
                Some(no_immediate().with(no_immediate().and(no_memory())))
            }
            asm::x86::BaseX86::Idiv { .. }
            | asm::x86::BaseX86::Div { .. }
            | asm::x86::BaseX86::Unary { .. } => Some(no_immediate().unary()),

            asm::x86::BaseX86::Binary {
                operator: Binary::And | Binary::Or | Binary::Xor,
                ..
            }
            | asm::x86::BaseX86::Cmp { .. }
            | asm::x86::BaseX86::Binary {
                operator: Binary::Add | Binary::Sub,
                ..
            } => Some(max_dword().with(no_immediate()).max_1_stack()),
            asm::x86::BaseX86::Binary {
                operator: Binary::Mult,
                ..
            } => Some(max_dword().with(no_memory()).max_1_stack()),

            asm::x86::BaseX86::MovZeroExtend(_) | asm::x86::BaseX86::Push(_) => {
                Some(max_dword().unary())
            }
            asm::x86::BaseX86::Call(_)
            | asm::x86::BaseX86::Ret
            | asm::x86::BaseX86::Cdq(_)
            | asm::x86::BaseX86::Jmp(_)
            | asm::x86::BaseX86::JmpCC { .. }
            | asm::x86::BaseX86::SetCC { .. }
            | asm::x86::BaseX86::Label(_) => None,
        }
    }

    const fn left(&self) -> OpRule {
        match self {
            PairRule::Unary(left) | PairRule::Binary { left, .. } => *left,
        }
    }

    /// # Safety
    /// # PairRule must be binary
    const unsafe fn assume_right(&self) -> OpRule {
        if let PairRule::Binary { right, .. } = self {
            *right
        } else {
            unsafe { std::hint::unreachable_unchecked() }
        }
    }

    const fn unary(rule: OpRule) -> Self {
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
