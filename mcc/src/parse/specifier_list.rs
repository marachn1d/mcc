use super::{Error, SpecifierList, StorageClass, Token, VarType};
use ast::Sign;

use util::TokenIter;

#[derive(Debug, Clone)]
pub struct SpeclistFsm {
    sc: Option<StorageClass>,
    ty: Option<TyBuilder>,
}

#[derive(Debug, Copy, Clone)]
pub enum TyBuilder {
    Sign(Sign),
    Int(Option<Sign>),
    // bool is seen_int, it's how we reject "long int int", for example
    Long(bool, Option<Sign>),
}

impl TyBuilder {
    const fn new_int() -> Self {
        Self::Int(None)
    }

    const fn new_long() -> Self {
        Self::Long(false, None)
    }

    const fn new_signed() -> Self {
        Self::Sign(Sign::Signed)
    }

    const fn new_unsigned() -> Self {
        Self::Sign(Sign::Unsigned)
    }

    const fn const_clone(&self) -> Self {
        match self {
            TyBuilder::Sign(s) => Self::Sign(*s),
            TyBuilder::Int(s) => Self::Int(*s),
            TyBuilder::Long(seen_int, s) => Self::Long(*seen_int, *s),
        }
    }

    const fn invalid_type<T>(&self, t: Token) -> Result<T, Error> {
        Err(Error::InvalidType(self.const_clone(), t))
    }

    const fn int(&mut self) -> Result<(), Error> {
        match self {
            Self::Sign(s) => *self = Self::Int(Some(*s)),
            Self::Int(_) => return self.invalid_type(Token::Int),
            Self::Long(seen_int, _) if *seen_int => return self.invalid_type(Token::Int),
            Self::Long(seen_int, _) => *seen_int = true,
        };
        Ok(())
    }
    const fn long(&mut self) -> Result<(), Error> {
        match self {
            Self::Sign(s) => *self = Self::Long(false, Some(*s)),
            Self::Int(s) => *self = Self::Long(true, *s),
            Self::Long(_, _) => return self.invalid_type(Token::Long),
        };
        Ok(())
    }

    fn unsigned(&mut self) -> Result<(), Error> {
        use Sign::Unsigned;
        if let Self::Int(s) | Self::Long(_, s) = self
            && s.is_none()
        {
            *s = Some(Unsigned);
            Ok(())
        } else {
            self.invalid_type(Token::Unsigned)
        }
    }
    const fn signed(&mut self) -> Result<(), Error> {
        use Sign::Signed;
        if let Self::Int(s) | Self::Long(_, s) = self
            && s.is_none()
        {
            *s = Some(Signed);
            Ok(())
        } else {
            self.invalid_type(Token::Signed)
        }
    }

    fn to_type(self) -> VarType {
        match self {
            TyBuilder::Sign(sign) => VarType::int_with_sign(sign),
            TyBuilder::Int(s) => s.map(VarType::int_with_sign).unwrap_or(VarType::int()),
            TyBuilder::Long(_, s) => s.map(VarType::long_with_sign).unwrap_or(VarType::long()),
        }
    }
}

impl SpeclistFsm {
    const fn new() -> Self {
        Self { sc: None, ty: None }
    }

    const fn const_clone(&self) -> Self {
        Self {
            sc: self.sc,
            ty: self.ty,
        }
    }

    pub fn done(self) -> Result<SpecifierList, Error> {
        if let Some(ty) = self.ty {
            Ok(SpecifierList {
                sc: self.sc,
                typ: ty.to_type(),
            })
        } else {
            Err(Error::Catchall("invalid specifier list"))
        }
    }

    pub fn type_specifier(self) -> Result<VarType, Error> {
        if self.sc.is_some() {
            Err(Error::NoStorageClass(self))
        } else {
            self.ty
                .map_or_else(|| self.invalid_specifiers(), |ty| Ok(ty.to_type()))
        }
    }

    const fn invalid_specifiers<T>(&self) -> Result<T, Error> {
        Err(Error::InvalidSpecifiers(self.const_clone()))
    }

    fn r#extern(&mut self) -> Result<(), Error> {
        match self.sc {
            Some(StorageClass::Static) => Err(Error::ConflictingLinkage),
            Some(StorageClass::Extern) => self.invalid_specifiers(),
            None => {
                self.sc = Some(StorageClass::Extern);
                Ok(())
            }
        }
    }

    fn r#static(&mut self) -> Result<(), Error> {
        match self.sc {
            Some(StorageClass::Static) => self.invalid_specifiers(),
            Some(StorageClass::Extern) => Err(Error::ConflictingLinkage),
            None => {
                self.sc = Some(StorageClass::Static);
                Ok(())
            }
        }
    }

    fn long(&mut self) -> Result<(), Error> {
        self.map_ty(TyBuilder::long, TyBuilder::new_long())
    }

    fn map_ty(
        &mut self,
        mut f: impl FnMut(&mut TyBuilder) -> Result<(), Error>,
        default: TyBuilder,
    ) -> Result<(), Error> {
        if let Some(ty) = &mut self.ty {
            f(ty)
        } else {
            self.ty = Some(default);
            Ok(())
        }
    }

    fn int(&mut self) -> Result<(), Error> {
        self.map_ty(TyBuilder::int, TyBuilder::new_int())
    }

    fn unsigned(&mut self) -> Result<(), Error> {
        self.map_ty(TyBuilder::unsigned, TyBuilder::new_unsigned())
    }

    fn signed(&mut self) -> Result<(), Error> {
        self.map_ty(TyBuilder::signed, TyBuilder::new_signed())
    }
}

fn get_specifier(tokens: &mut TokenIter, builder: &mut SpeclistFsm) -> Result<bool, Error> {
    match tokens.peek() {
        Some(Token::Int) => builder.int().map(|_| true),
        Some(Token::Long) => builder.long().map(|_| true),
        Some(Token::Static) => builder.r#static().map(|_| true),
        Some(Token::Extern) => builder.r#extern().map(|_| true),
        Some(Token::Signed) => builder.signed().map(|_| true),
        Some(Token::Unsigned) => builder.unsigned().map(|_| true),
        _ => Ok(false),
    }
}

pub fn get_specifiers(tokens: &mut TokenIter) -> Result<SpeclistFsm, Error> {
    let mut builder = SpeclistFsm::new();
    while get_specifier(tokens, &mut builder)? {
        tokens.next();
    }
    Ok(builder)
}
