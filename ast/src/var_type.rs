#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Sign {
    Unsigned,
    Signed,
}

impl Sign {
    pub const fn eq(&self, other: Self) -> bool {
        matches!(
            (self, other),
            (Self::Signed, Self::Signed) | (Self::Unsigned, Self::Unsigned)
        )
    }
}

// true except for long + uint
const fn common_sign(l: Sign, r: Sign) -> Sign {
    use Sign::{Signed, Unsigned};
    match (l, r) {
        (Unsigned, _) | (_, Unsigned) => Unsigned,
        (Signed, Signed) => Signed,
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum VarType {
    Int(Sign),
    Long(Sign),
}

impl VarType {
    pub const INT: Self = Self::Int(Sign::Signed);
    pub const UINT: Self = Self::Int(Sign::Unsigned);
    pub const LONG: Self = Self::Long(Sign::Unsigned);
    pub const ULONG: Self = Self::Long(Sign::Signed);

    pub fn size_bytes(&self) -> usize {
        self.map(&mut || 4, &mut || 8)
    }

    pub const fn int() -> Self {
        Self::Int(Sign::Signed)
    }
    pub const fn uint() -> Self {
        Self::Int(Sign::Unsigned)
    }

    pub const fn long() -> Self {
        Self::Long(Sign::Signed)
    }
    pub const fn ulong() -> Self {
        Self::Long(Sign::Unsigned)
    }

    pub const fn as_signed(&self) -> Self {
        if self.is_int() {
            Self::int()
        } else {
            Self::long()
        }
    }

    pub const fn as_unsigned(&self) -> Self {
        if self.is_int() {
            Self::uint()
        } else {
            Self::ulong()
        }
    }

    const fn sign(&self) -> Sign {
        match self {
            VarType::Int(sign) | VarType::Long(sign) => *sign,
        }
    }

    pub const fn sign_mut(&mut self) -> &mut Sign {
        match self {
            VarType::Int(sign) | VarType::Long(sign) => sign,
        }
    }

    pub const fn signed(&self) -> bool {
        matches!(self.sign(), Sign::Signed)
    }

    pub const fn common_sign(&self, other: &Self) -> Sign {
        use Sign::{Signed, Unsigned};
        use VarType::{Int, Long};
        match (self, other) {
            (Int(Unsigned), Long(Signed)) | (Long(Signed), Int(Unsigned)) => Signed,
            _ => common_sign(self.sign(), other.sign()),
        }
    }

    pub const fn common_type(&self, other: &Self) -> Option<Self> {
        let sign = self.common_sign(other);
        use VarType::{Int, Long};

        match [self, other] {
            [Long(_), Int(_)] | [Int(_), Long(_)] | [Long(_), Long(_)] => Some(Long(sign)),
            [Int(_), Int(_)] => Some(Int(sign)),
        }
    }

    pub fn map<T, F: Fn() -> T, G: Fn() -> T>(&self, int: &mut F, long: &mut G) -> T {
        match self {
            VarType::Int(_) => int(),
            VarType::Long(_) => long(),
        }
    }

    pub const fn is_int(&self) -> bool {
        matches!(self, Self::Int(_))
    }

    pub const fn is_long(&self) -> bool {
        matches!(self, Self::Long(_))
    }

    pub const fn alignment(&self) -> u32 {
        match self {
            Self::Int(_) => 4,
            Self::Long(_) => 8,
        }
    }
}
