#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum VarType {
    Int,
    Long,
}

impl VarType {
    pub const fn common_type(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (Self::Long, Self::Long) | (Self::Long, Self::Int) | (Self::Int, Self::Long) => {
                Some(Self::Long)
            }
            (Self::Int, Self::Int) => Some(Self::Int),
        }
    }

    pub const fn alignment(&self) -> u32 {
        match self {
            Self::Int => 4,
            Self::Long => 8,
        }
    }
}
