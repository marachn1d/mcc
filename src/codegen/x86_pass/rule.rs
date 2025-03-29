use super::{Op, OpPair, PseudoOp};
use std::ops::BitOr;

#[derive(Debug)]
struct InvalidState {}

#[derive(Copy, Clone, Debug)]
pub struct PairSet(u8);

// lower 4 bits = left, upper 4 bits = right

const MAX_DWORD: RuleSet = RuleSet(OpRule::MaxDword as u8);

const NO_IMM: RuleSet = RuleSet(OpRule::NoImm as u8);

const NO_MEM: RuleSet = RuleSet(OpRule::NoMem as u8);

const NORULE: RuleSet = RuleSet(0);

pub const MOV: PairSet = pair_set(false, MAX_DWORD.or(NO_MEM), NORULE);
pub const CMP: PairSet = pair_set(false, MAX_DWORD, MAX_DWORD.or(NO_MEM));
pub const BIN_ADDSUB: PairSet = pair_set(true, MAX_DWORD, NO_MEM);
pub const MOVSX: PairSet = pair_set(false, NO_IMM, NO_MEM);
pub const BIN_BITSHIFT: PairSet = pair_set(false, NO_IMM, NO_MEM);
pub const PUSH: RuleSet = MAX_DWORD;
pub const IDIV: RuleSet = NO_IMM;

const fn pair_set(max_one: bool, l: RuleSet, r: RuleSet) -> PairSet {
    let set = (r.0 << 4) | l.0;
    if max_one {
        PairSet(set | PairRule::Max1Stack as u8)
    } else {
        PairSet(set & !(PairRule::Max1Stack as u8))
    }
}

impl PairSet {
    pub const fn max_1_stack(&self) -> bool {
        self.0 & PairRule::Max1Stack as u8 == 0b1000
    }

    pub const fn left(&self) -> RuleSet {
        RuleSet(self.0 & 0b1111)
    }

    pub const fn right(&self) -> RuleSet {
        RuleSet((self.0 >> 4) & 0b1111)
    }

    pub const fn new(max_one: bool, l: RuleSet, r: RuleSet) -> Self {
        let set = (r.0 << 4) | l.0;
        if max_one {
            Self(set | PairRule::Max1Stack as u8)
        } else {
            Self(set & !(PairRule::Max1Stack as u8))
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct RuleSet(u8);

impl RuleSet {
    pub const fn imm_allowed(&self, val: i64) -> bool {
        !(self.no_imm() || (self.max_dword() && val > i32::MAX as i64))
    }
    pub const fn nostack_and(r: ImmRule) -> Self {
        match r {
            ImmRule::MaxDword => Self(Self::NOSTACK & Self::MAXDWORD),
            ImmRule::NoImm => Self(Self::NOSTACK & Self::NOIMM),
        }
    }

    pub const fn new_max_dword() -> Self {
        Self(Self::MAXDWORD)
    }

    pub const fn imm_rule(&self) -> Option<ImmRule> {
        if self.no_imm() {
            Some(ImmRule::NoImm)
        } else if self.max_dword() {
            Some(ImmRule::MaxDword)
        } else {
            None
        }
    }

    pub const fn imm(r: ImmRule) -> Self {
        match r {
            ImmRule::MaxDword => Self(Self::MAXDWORD),
            ImmRule::NoImm => Self(Self::NOIMM),
        }
    }

    pub const NONE: u8 = OpRule::None as u8;
    pub const NOSTACK: u8 = OpRule::NoMem as u8;
    pub const MAXDWORD: u8 = OpRule::MaxDword as u8;

    pub const NOIMM: u8 = OpRule::NoImm as u8;

    pub const fn validate(self) -> RuleSet {
        if self.invalid_state() {
            panic!("invalid state")
        }
        self
    }

    pub const fn none(&self) -> bool {
        self.0 == 0
    }

    pub const fn no_stack(&self) -> bool {
        self.0 & 0b1 == 1
    }

    pub const fn max_dword(&self) -> bool {
        self.0 & 0b10 == 1
    }

    pub const fn no_imm(&self) -> bool {
        self.0 & 0b100 == 1
    }

    pub const fn invalid_state(&self) -> bool {
        self.0 & 0b110 == 0b110
    }

    pub const fn or(self, rhs: Self) -> RuleSet {
        Self(self.0 | rhs.0).validate()
    }

    pub const fn or_bit(self, rhs: OpRule) -> RuleSet {
        Self(self.0 | rhs as u8).validate()
    }
}

impl BitOr<OpRule> for RuleSet {
    type Output = RuleSet;

    fn bitor(self, rhs: OpRule) -> Self::Output {
        self.or_bit(rhs)
    }
}

impl BitOr<RuleSet> for OpRule {
    type Output = RuleSet;

    fn bitor(self, rhs: RuleSet) -> Self::Output {
        rhs.or_bit(self)
    }
}

impl BitOr<OpRule> for OpRule {
    type Output = RuleSet;

    fn bitor(self, rhs: Self) -> Self::Output {
        self.or(rhs)
    }
}

#[repr(u8)]
#[derive(Copy, Clone, Debug)]
pub enum OpRule {
    None = 0b000,
    NoMem = 0b001,
    MaxDword = 0b010,
    NoImm = 0b100,
}

pub enum ImmRule {
    MaxDword,
    NoImm,
}

impl OpRule {
    const fn or(self, rhs: Self) -> RuleSet {
        match (self, rhs) {
            (Self::NoImm, Self::MaxDword) | (Self::MaxDword, Self::NoImm) => {
                panic!("invalid state")
            }
            _ => RuleSet(self as u8 | rhs as u8),
        }
    }
}

#[repr(u8)]
#[derive(Copy, Clone, Debug)]
enum PairRule {
    None = 0b0000,
    Max1Stack = 0b1000,
}
