use super::{Error, SpecifierList, StorageClass, Token, TokenIter, VarType};

#[derive(Debug, Clone)]
pub struct SpeclistFsm {
    sc: Option<StorageClass>,
    int: bool,
    typ: Option<VarType>,
}

impl SpeclistFsm {
    const fn new() -> Self {
        Self {
            sc: None,
            typ: None,
            int: false,
        }
    }

    pub fn done(self) -> Result<SpecifierList, Error> {
        if let Some(typ) = self.typ {
            Ok(SpecifierList { sc: self.sc, typ })
        } else {
            Err(Error::Catchall("invalid specifier list"))
        }
    }

    pub fn type_specifier(self) -> Result<VarType, Error> {
        match self {
            Self { sc: Some(_), .. } => Err(Error::NoStorageClass),
            Self {
                sc: None,
                typ: None,
                int: _,
            } => Err(Error::InvalidSpecifiers),
            Self {
                sc: None,
                typ: Some(typ),
                int: _,
            } => Ok(typ),
        }
    }

    const fn r#extern(&mut self) -> Result<(), Error> {
        match self.sc {
            Some(StorageClass::Static) => Err(Error::ConflictingLinkage),
            Some(StorageClass::Extern) => Err(Error::InvalidSpecifiers),
            None => {
                self.sc = Some(StorageClass::Extern);
                Ok(())
            }
        }
    }

    const fn r#static(&mut self) -> Result<(), Error> {
        match self.sc {
            Some(StorageClass::Static) => Err(Error::InvalidSpecifiers),
            Some(StorageClass::Extern) => Err(Error::ConflictingLinkage),
            None => {
                self.sc = Some(StorageClass::Static);
                Ok(())
            }
        }
    }

    fn long(&mut self) -> Result<(), Error> {
        match self.typ {
            Some(VarType::Int) => {
                self.typ = Some(VarType::Long);
                self.int = true;
                Ok(())
            }
            Some(VarType::Long) => self.invalid_type(),
            None => {
                self.typ = Some(VarType::Long);
                Ok(())
            }
        }
    }

    fn int(&mut self) -> Result<(), Error> {
        match self.typ {
            None => {
                self.typ = Some(VarType::Int);
                self.int = true;
                Ok(())
            }
            Some(VarType::Long) if !self.int => Ok(()),
            Some(VarType::Int | VarType::Long) => self.invalid_type(),
        }
    }

    fn invalid_type<T>(&self) -> Result<T, Error> {
        Err(Error::InvalidType(self.clone()))
    }
}

pub fn get_specifiers(tokens: &mut TokenIter) -> Result<SpeclistFsm, Error> {
    let mut builder = SpeclistFsm::new();

    tokens.take_until(|token| match token {
        Token::Int => builder.int().map(|_| true),
        Token::Long => builder.long().map(|_| true),
        Token::Static => builder.r#static().map(|_| true),
        Token::Extern => builder.r#extern().map(|_| true),
        _ => Ok(false),
    })?;
    Ok(builder)
}
