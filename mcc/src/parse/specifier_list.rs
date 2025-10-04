use super::{Error, SpecifierList, StorageClass, Token, VarType};

use util::TokenIter;

#[derive(Debug, Clone)]
pub struct SpeclistFsm {
    sc: Option<StorageClass>,
    seen_int: bool,
    signed: Option<bool>,
    typ: Option<VarType>,
}

impl SpeclistFsm {
    const fn new() -> Self {
        Self {
            sc: None,
            typ: None,
            seen_int: false,
            signed: None,
        }
    }

    pub const fn done(self) -> Result<SpecifierList, Error> {
        if let Some(typ) = self.typ {
            Ok(SpecifierList { sc: self.sc, typ })
        } else if let Some(false) = self.signed {
            Ok(SpecifierList {
                sc: self.sc,
                typ: VarType::UINT,
            })
        } else {
            Err(Error::Catchall("invalid specifier list"))
        }
    }

    pub const fn type_specifier(self) -> Result<VarType, Error> {
        match self {
            Self { sc: Some(_), .. } => Err(Error::NoStorageClass),
            Self {
                sc: None,
                typ: None,
                ..
            } => Err(Error::InvalidSpecifiers),
            Self {
                sc: None,
                typ: Some(typ),
                ..
            } => Ok(typ),
        }
    }

    fn r#extern(&mut self) -> Result<(), Error> {
        match self.sc {
            Some(StorageClass::Static) => Err(Error::ConflictingLinkage),
            Some(StorageClass::Extern) => Err(Error::InvalidSpecifiers),
            None => {
                self.sc = Some(StorageClass::Extern);
                Ok(())
            }
        }
    }

    fn r#static(&mut self) -> Result<(), Error> {
        match self.sc {
            Some(StorageClass::Static) => Err(Error::InvalidSpecifiers),
            Some(StorageClass::Extern) => Err(Error::ConflictingLinkage),
            None => {
                self.sc = Some(StorageClass::Static);
                Ok(())
            }
        }
    }

    fn default_signed(&self) -> bool {
        matches!(self.signed, None | Some(true))
    }

    fn long(&mut self) -> Result<(), Error> {
        match self.typ {
            Some(VarType::Int(s)) => {
                self.typ = Some(VarType::Long(s));
                self.seen_int = true;
                Ok(())
            }
            Some(VarType::Long(_)) => self.invalid_type(),
            None => {
                self.typ = if self.default_signed() {
                    Some(VarType::long())
                } else {
                    Some(VarType::ulong())
                };
                Ok(())
            }
        }
    }

    fn int(&mut self) -> Result<(), Error> {
        match self.typ {
            None => {
                self.typ = if self.default_signed() {
                    Some(VarType::int())
                } else {
                    Some(VarType::uint())
                };
                self.seen_int = true;
                Ok(())
            }
            Some(VarType::Long(_)) if !self.seen_int => {
                self.seen_int = true;
                Ok(())
            }
            Some(VarType::Int(_) | VarType::Long(_)) => self.invalid_type(),
        }
    }

    fn unsigned(&mut self) -> Result<(), Error> {
        if self.signed.is_some() {
            Err(Error::InvalidSpecifiers)
        } else {
            self.signed = Some(false);
            if let Some(ty) = &mut self.typ {
                *ty = ty.as_unsigned()
            }

            Ok(())
        }
    }

    fn signed(&mut self) -> Result<(), Error> {
        if self.signed.is_some() {
            Err(Error::InvalidSpecifiers)
        } else {
            self.signed = Some(true);
            if let Some(ty) = &mut self.typ {
                *ty = ty.as_signed()
            }
            Ok(())
        }
    }

    fn invalid_type<T>(&self) -> Result<T, Error> {
        Err(Error::InvalidType(self.clone()))
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
