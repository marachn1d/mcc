use super::{Error, SpecifierList, StorageClass, Token, TokenIter, VarType};

fn get_specifier(tokens: &mut TokenIter, builder: &mut SpeclistFsm) -> Result<bool, Error> {
    match tokens.peek() {
        Some(Token::Extern) => builder.r#extern().map(|_| true),
        Some(Token::Static) => builder.r#static().map(|_| true),
        _ => get_ty(tokens, &mut builder.var),
    }
}

fn get_ty(tokens: &mut TokenIter, builder: &mut TypeFsm) -> Result<bool, Error> {
    match tokens.peek() {
        Some(Token::Int) => builder.int().map(|_| true),
        Some(Token::Long) => builder.long().map(|_| true),
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

pub fn type_specifier(tokens: &mut TokenIter) -> Result<VarType, Error> {
    let mut builder = TypeFsm::new();

    while get_ty(tokens, &mut builder)? {
        tokens.next();
    }
    builder.done()
}

#[derive(Debug, Clone)]
pub struct TypeFsm {
    ty: Option<NumTy>,
    signed: Option<Sign>,
}

impl TypeFsm {
    const fn new() -> Self {
        Self {
            ty: None,
            signed: None,
        }
    }

    const fn int(&mut self) -> Result<(), Error> {
        match self.ty {
            None => self.ty = Some(NumTy::Int),
            Some(NumTy::Long) => self.ty = Some(NumTy::LongInt),
            _ => return Err(Error::InvalidSpecifiers),
        }
        Ok(())
    }

    const fn long(&mut self) -> Result<(), Error> {
        match self.ty {
            None => self.ty = Some(NumTy::Long),
            Some(NumTy::Int) => self.ty = Some(NumTy::LongInt),
            _ => return Err(Error::InvalidSpecifiers),
        }
        Ok(())
    }

    const fn unsigned(&mut self) -> Result<(), Error> {
        if self.signed.is_none() {
            self.signed = Some(Sign::Unsigned);
            Ok(())
        } else {
            Err(Error::InvalidSpecifiers)
        }
    }

    const fn signed(&mut self) -> Result<(), Error> {
        if self.signed.is_none() {
            self.signed = Some(Sign::Signed);
            Ok(())
        } else {
            Err(Error::InvalidSpecifiers)
        }
    }

    const fn done(&self) -> Result<VarType, Error> {
        let ty = match self.ty {
            Some(ty) => ty,
            None if self.signed.is_some() => NumTy::Int,
            _ => return Err(Error::InvalidSpecifiers),
        };
        Ok(match (ty, self.signed) {
            (NumTy::Int, None | Some(Sign::Signed)) => VarType::Int,
            (NumTy::Int, Some(Sign::Unsigned)) => VarType::UInt,
            (NumTy::Long | NumTy::LongInt, None | Some(Sign::Signed)) => VarType::Long,
            (NumTy::Long | NumTy::LongInt, Some(Sign::Unsigned)) => VarType::ULong,
        })
    }
}

#[derive(Debug, Clone)]
pub struct SpeclistFsm {
    sc: Option<StorageClass>,
    var: TypeFsm,
}

#[derive(Debug, Clone, Copy)]
enum NumTy {
    Int,
    Long,
    LongInt,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Sign {
    Signed,
    Unsigned,
}

impl SpeclistFsm {
    const fn new() -> Self {
        Self {
            sc: None,
            var: TypeFsm::new(),
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

    pub fn done(self) -> Result<SpecifierList, Error> {
        self.var
            .done()
            .map(|typ| SpecifierList { typ, sc: self.sc })
    }

    pub const fn var_type(&self) -> Result<VarType, Error> {
        self.var.done()
    }

    pub const fn type_specifier(self) -> Result<VarType, Error> {
        if self.sc.is_some() {
            Err(Error::NoStorageClass)
        } else {
            self.var_type()
        }
    }
}
