use std::ops::BitOr;

#[derive(Copy, Clone, Debug)]
pub struct PairSet(u8);

// lower 4 bits = left, upper 4 bits = right

const MAX_DWORD: RuleSet = RuleSet(OpRule::MaxDword as u8);

const NO_IMM: RuleSet = RuleSet(OpRule::NoImm as u8);

const NO_MEM: RuleSet = RuleSet(OpRule::NoMem as u8);

const _NORULE: RuleSet = RuleSet(0);

pub struct RuleTable {
    pub mov: PairSet,
    pub movsx: PairSet,
    pub add: PairSet,
    pub unop: RuleSet,
    pub sub: PairSet,
    pub shift: PairSet,
    pub bitwise: PairSet,
    pub div: RuleSet,
    pub push: RuleSet,
    pub cmp: PairSet,
    pub mul: PairSet,
}

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

// ideally we'd have some nice elegant matching stuff but it's pretty involved
#[derive(PartialEq, Eq)]
enum MemRule {
    One,
    Two,
}

const fn pair_set(mem: MemRule, l: RuleSet, r: RuleSet) -> PairSet {
    let set = (r.0 << 4) | l.0;
    match mem {
        MemRule::One => PairSet(set | MAX1STACK),
        MemRule::Two => PairSet(set & !(MAX1STACK)),
    }
}

impl PairSet {
    pub const fn max_1_stack(&self) -> bool {
        self.0 & MAX1STACK == MAX1STACK
    }

    pub const fn left(&self) -> RuleSet {
        RuleSet(self.0 & 0b0111)
    }

    pub const fn right(&self) -> RuleSet {
        RuleSet((self.0 >> 4) & 0b0111)
    }
}
use std::fmt::{self, Display, Formatter};
#[derive(Copy, Clone, Debug)]
pub struct RuleSet(u8);
impl Display for RuleSet {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let mut first = false;
        if self.max_dword() {
            first = true;
            write!(f, "MAX_DWORD")?;
        };
        if self.no_mem() {
            if !first {
                write!(f, " | ")?;
            }
            write!(f, "NO_MEM")?;
            first = true;
        };

        if self.no_imm() {
            if !first {
                write!(f, " | ")?;
            }

            write!(f, "NO_IMM")?;
        }

        Ok(())
    }
}

impl RuleSet {
    pub const fn imm_not_allowed(&self, val: i64) -> bool {
        self.no_imm() || (self.max_dword() && val > i32::MAX as i64)
    }

    pub const fn validate(self) -> RuleSet {
        if self.invalid_state() {
            panic!("invalid state")
        }
        self
    }

    pub const fn none(&self) -> bool {
        self.0 == 0
    }

    pub const fn no_mem(&self) -> bool {
        self.0 & 0b1 == 1
    }

    pub const fn max_dword(&self) -> bool {
        self.0 & 0b10 == 0b10
    }

    pub const fn no_imm(&self) -> bool {
        self.0 & 0b100 == 0b100
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
    NoMem = 0b001,
    MaxDword = 0b010,
    NoImm = 0b100,
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
const MAX1STACK: u8 = 0b1000;

#[cfg(test)]
mod test {
    #[test]
    fn test_none() {
        let rule = super::_NORULE;
        assert!(rule.none());
        assert!(!rule.no_mem());
        assert!(!rule.max_dword());
        assert!(!rule.no_imm());
    }

    #[test]
    fn test_pair() {
        let rule = super::RULES.mov;
        assert!(!rule.max_1_stack());
        assert!(!rule.left().none());
        assert!(rule.left().max_dword());
        assert!(rule.left().no_mem());
        assert!(!rule.left().no_imm());

        assert!(rule.right().none());
        assert!(!rule.right().no_mem());
        assert!(!rule.right().max_dword());
        assert!(!rule.right().no_imm());
    }
}
