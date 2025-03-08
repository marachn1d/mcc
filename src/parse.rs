use super::lex::Identifier;

use super::lex::Keyword;
use super::slice_iter::TokenIter;

use super::Token;
use std::fmt::{self, Display, Formatter};

pub fn parse(tokens: Box<[Token]>) -> Result<Program, Error> {
    let mut tokens = TokenIter::new(tokens);
    let program = program(&mut tokens)?;
    if tokens.is_empty() {
        Ok(program)
    } else {
        Err(Error::ExtraStuff)
    }
}

fn program(tokens: &mut TokenIter) -> Result<Program, Error> {
    let mut functions = Vec::new();
    while !tokens.is_empty() {
        functions.push(declaration(tokens)?);
    }
    Ok(Program(functions.into()))
}

#[derive(Debug)]
pub struct Program(pub Box<[Declaration]>);

fn declaration(tokens: &mut TokenIter) -> Result<Declaration, Error> {
    let storage_class = specifiers(tokens)?;

    let name = tokens.consume_identifier()?;

    match tokens.consume_any()? {
        Token::Equals => {
            let exp = expression(tokens, None)?;
            tokens.consume(Token::Semicolon)?;
            Ok(Declaration::Var {
                name,
                init: Some(exp),
                storage_class,
            })
        }
        Token::OpenParen => {
            fndec_params_and_body(tokens, name, storage_class).map(Declaration::from)
        }
        Token::Semicolon => Ok(Declaration::Var {
            name,
            init: None,
            storage_class,
        }),
        _ => Err(Error::Catchall("expected initializer or semicolon")),
    }
}

fn specifiers(tokens: &mut TokenIter) -> Result<Option<StorageClass>, Error> {
    match tokens.as_slice() {
        [] => expected_eof(),

        [Token::Keyword(Keyword::Int), Token::Keyword(Keyword::Static), ..]
        | [Token::Keyword(Keyword::Static), Token::Keyword(Keyword::Int), ..] => {
            tokens.next();
            tokens.next();
            Ok(Some(StorageClass::Static))
        }

        [Token::Keyword(Keyword::Int), Token::Keyword(Keyword::Extern), ..]
        | [Token::Keyword(Keyword::Extern), Token::Keyword(Keyword::Int), ..] => {
            tokens.next();
            tokens.next();
            Ok(Some(StorageClass::Extern))
        }

        [Token::Keyword(Keyword::Static), Token::Keyword(Keyword::Extern), Token::Keyword(Keyword::Int), ..]
        | [Token::Keyword(Keyword::Extern), Token::Keyword(Keyword::Static), Token::Keyword(Keyword::Int), ..] => {
            Err(Error::ConflictingLinkage)
        }

        [Token::Keyword(Keyword::Int), ..] => {
            tokens.next();
            Ok(None)
        }
        _ => Err(Error::Expected(Keyword::Int.into())),
    }
}

fn fndec_params_and_body(
    tokens: &mut TokenIter,
    name: Identifier,
    storage_class: Option<StorageClass>,
) -> Result<FunctionDeclaration, Error> {
    let parameters = param_list(tokens)?;
    Ok(if tokens.next_if(|x| x == &Token::Semicolon).is_some() {
        FunctionDeclaration {
            name,
            params: parameters,
            body: None,
            storage_class,
        }
    } else {
        let body = Some(block(tokens)?);
        FunctionDeclaration {
            name,
            params: parameters,
            body,
            storage_class,
        }
    })
}

fn param_list(tokens: &mut TokenIter) -> Result<ParamList, Error> {
    match tokens.peek_any()? {
        Token::Keyword(Keyword::Void) => {
            tokens.next();

            tokens.consume(Token::CloseParen)?;
            Ok(ParamList::Void)
        }
        Token::Keyword(Keyword::Int) => {
            tokens.next();
            let mut ident = Vec::new();
            ident.push(tokens.consume_identifier()?);
            while param_continues(tokens)? {
                ident.push(tokens.consume_identifier()?);
            }

            Ok(ParamList::Int(ident.into()))
        }
        _ => Err(Error::Catchall("invalid param list")),
    }
}

fn param_continues(tokens: &mut TokenIter) -> Result<bool, Error> {
    match tokens.peek_any()? {
        Token::Comma => {
            tokens.next();
            tokens.consume(Keyword::Int)?;
            Ok(true)
        }
        Token::CloseParen => {
            tokens.next();
            Ok(false)
        }
        _ => Err(Error::Catchall("expected ',' or ')'.")),
    }
}

#[derive(Debug)]
pub struct Block(pub Box<[BlockItem]>);

fn block(tokens: &mut TokenIter) -> Result<Block, Error> {
    tokens.consume(Token::OpenBrace)?;
    let mut body = Vec::new();
    while let Some(item) = block_item(tokens)? {
        body.push(item);
    }
    tokens.consume(Token::CloseBrace)?;
    Ok(Block(body.into()))
}

fn block_item(tokens: &mut TokenIter) -> Result<Option<BlockItem>, Error> {
    match tokens.peek_any()? {
        Token::Keyword(Keyword::Int | Keyword::Static | Keyword::Extern) => {
            Ok(Some(BlockItem::D(declaration(tokens)?)))
        }
        Token::CloseBrace => Ok(None),
        _ => Ok(Some(BlockItem::S(statement(tokens)?))),
    }
}

fn var_declaration(
    tokens: &mut TokenIter,
    storage_class: Option<StorageClass>,
) -> Result<VariableDeclaration, Error> {
    tokens.consume(Keyword::Int)?;
    let name = tokens.consume_identifier()?;
    let init = match tokens.consume_any()? {
        Token::Equals => {
            let exp = expression(tokens, None)?;

            tokens.consume(Token::Semicolon)?;
            Ok(Some(exp))
        }
        Token::Semicolon => Ok(None),
        _ => Err(Error::Catchall("expected initializer or semicolon")),
    }?;
    Ok(VariableDeclaration {
        name,
        init,
        storage_class,
    })
}

#[derive(Debug)]
pub enum ParamList {
    Void,
    Int(Box<[Identifier]>),
}

impl ParamList {
    pub fn iter(&mut self) -> std::slice::IterMut<Identifier> {
        match self {
            Self::Int(inner) => inner.iter_mut(),
            Self::Void => [].iter_mut(),
        }
    }

    pub const fn len(&self) -> usize {
        match self {
            Self::Void => 0,
            Self::Int(inner) => inner.len(),
        }
    }

    pub const fn is_empty(&self) -> bool {
        match self {
            Self::Void => true,
            Self::Int(_) => false,
        }
    }
}

pub enum Param {
    Void,
    Int(Identifier),
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub name: Identifier,
    pub params: ParamList,
    pub body: Option<Block>,
    pub storage_class: Option<StorageClass>,
}

#[derive(Debug)]
pub struct VariableDeclaration {
    pub name: Identifier,
    pub init: Option<Expression>,
    pub storage_class: Option<StorageClass>,
}

#[derive(Debug, Copy, Clone)]
pub enum StorageClass {
    Static,
    Extern,
}

impl PartialEq for StorageClass {
    fn eq(&self, other: &Self) -> bool {
        matches!(
            (self, other),
            (Self::Static, Self::Static) | (Self::Extern, Self::Extern)
        )
    }
}

impl From<FunctionDeclaration> for Declaration {
    fn from(
        FunctionDeclaration {
            name,
            body,
            params,
            storage_class,
        }: FunctionDeclaration,
    ) -> Self {
        Self::Function {
            name,
            body,
            params,
            storage_class,
        }
    }
}
impl From<VariableDeclaration> for Declaration {
    fn from(dec: VariableDeclaration) -> Self {
        Self::Var {
            name: dec.name,
            init: dec.init,

            storage_class: dec.storage_class,
        }
    }
}

#[derive(Debug)]
pub struct Function {
    pub name: Identifier,
    pub body: Block,
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Function(\nname={},\nbody={:?}\n)", self.name, self.body)
    }
}

fn statement(tokens: &mut TokenIter) -> Result<Statement, Error> {
    Ok(match tokens.peek_any()? {
        Token::Keyword(Keyword::Return) => {
            tokens.next();
            let expression = expression(tokens, None)?;
            tokens.consume(Token::Semicolon)?;
            Statement::Ret(expression)
        }
        Token::Semicolon => {
            tokens.next();
            Statement::Null
        }
        Token::Keyword(Keyword::If) => {
            tokens.next();
            tokens.consume(Token::OpenParen)?;
            let condition = expression(tokens, None)?;
            tokens.consume(Token::CloseParen)?;
            let stmnt = statement(tokens)?;
            let r#else = if tokens.consume(Token::Keyword(Keyword::Else)).is_ok() {
                Some(Box::new(statement(tokens)?))
            } else {
                None
            };
            Statement::If {
                condition,
                then: Box::new(stmnt),
                r#else,
            }
        }
        Token::Keyword(Keyword::Switch) => {
            tokens.next();
            tokens.consume(Token::OpenParen)?;
            let val = expression(tokens, None)?;
            tokens.consume(Token::CloseParen)?;
            let body = Box::new(statement(tokens)?);
            Statement::Switch { val, body }
        }
        Token::Keyword(Keyword::Goto) => {
            tokens.next();
            let identifier = tokens.consume_identifier()?;
            tokens.consume(Token::Semicolon)?;
            Statement::Goto(identifier)
        }
        // LABEL
        Token::Identifier(_) if tokens.peek_peek().is_some_and(|x| x == &Token::Colon) => {
            let label = Label::Named(tokens.consume_identifier()?);
            tokens.next();
            let body = statement(tokens)?.into();
            Statement::Label { label, body }
        }
        Token::Keyword(Keyword::Default) => {
            tokens.next();
            tokens.consume(Token::Colon)?;
            let body = statement(tokens)?.into();
            Statement::Label {
                label: Label::Default,
                body,
            }
        }
        Token::OpenBrace => {
            let block = block(tokens)?;
            Statement::Compound(block)
        }
        Token::Keyword(Keyword::Break) => {
            tokens.next();
            tokens.consume(Token::Semicolon)?;

            Statement::Break
        }
        Token::Keyword(Keyword::Case) => {
            tokens.next();
            match tokens.next() {
                Some(Token::Constant(lex::Constant::Integer(num))) => {
                    tokens.consume(Token::Colon)?;
                    let body = statement(tokens)?.into();
                    let label = Label::Case(Expression::Int(num));
                    Ok(Statement::Label { label, body })
                }
                Some(_) => Err(Error::Catchall("expected integer constant")),
                None => Err(Error::UnexpectedEof),
            }?
        }
        Token::Keyword(Keyword::Continue) => {
            tokens.next();
            tokens.consume(Token::Semicolon)?;
            Statement::Continue
        }
        Token::Keyword(Keyword::While) => {
            tokens.next();
            tokens.consume(Token::OpenParen)?;
            let condition = expression(tokens, None)?;

            tokens.consume(Token::CloseParen)?;
            Statement::While {
                condition,
                body: Box::new(statement(tokens)?),
            }
        }
        Token::Keyword(Keyword::Do) => {
            tokens.next();
            let body = Box::new(statement(tokens)?);
            tokens.consume_arr([Keyword::While.into(), Token::OpenParen])?;
            let condition = expression(tokens, None)?;
            tokens.consume_arr([Token::CloseParen, Token::Semicolon])?;
            Statement::DoWhile { body, condition }
        }
        Token::Keyword(Keyword::For) => {
            tokens.next();
            tokens.consume(Token::OpenParen)?;
            let init = for_init(tokens)?;

            let condition = optional_expr(tokens, Token::Semicolon)?;

            let post = optional_expr(tokens, Token::CloseParen)?;

            let body = Box::new(statement(tokens)?);

            Statement::For {
                init,
                condition,
                post,
                body,
            }
        }
        _ => {
            let e = expression(tokens, None)?;
            tokens.consume(Token::Semicolon)?;
            Statement::Exp(e)
        }
    })
}

fn optional_expr(tokens: &mut TokenIter, delim: Token) -> Result<Option<Expression>, Error> {
    if tokens.consume(delim.clone()).is_ok() {
        Ok(None)
    } else {
        let expression = expression(tokens, None)?;
        tokens.consume(delim)?;
        Ok(Some(expression))
    }
}

// for debugging
#[allow(dead_code)]
fn print_return<T: fmt::Debug>(mut f: impl FnMut() -> T) -> T {
    let val = f();
    eprintln!("{:?}", val);
    val
}

fn for_init(tokens: &mut TokenIter) -> Result<Option<ForInit>, Error> {
    if let Ok(declaration) = var_declaration(tokens, None) {
        Ok(Some(ForInit::D(declaration)))
    } else {
        let res = optional_expr(tokens, Token::Semicolon)?.map(ForInit::E);
        Ok(res)
    }
}

#[derive(Debug)]
pub enum BlockItem {
    S(Statement),
    D(Declaration),
}

#[derive(Debug)]
pub struct DebugStatement {
    statement: Statement,
    line: usize,
}

#[derive(Debug)]
pub enum Statement {
    Ret(Expression),
    Exp(Expression),
    If {
        condition: Expression,
        then: Box<Statement>,
        r#else: Option<Box<Statement>>,
    },
    Break,
    Continue,
    While {
        condition: Expression,
        body: Box<Self>,
    },
    DoWhile {
        body: Box<Self>,
        condition: Expression,
    },
    For {
        init: Option<ForInit>,
        condition: Option<Expression>,
        post: Option<Expression>,
        body: Box<Self>,
    },
    Compound(Block),
    Label {
        label: Label,
        body: Box<Self>,
    },
    Goto(Identifier),
    Null,
    Switch {
        val: Expression,
        body: Box<Self>,
    },
}

#[derive(Debug)]
pub enum ForInit {
    D(VariableDeclaration),
    E(Expression),
}

#[derive(Debug, Clone)]
pub enum Label {
    Named(Identifier),
    Case(Expression),
    Default,
}
#[derive(Debug)]
pub enum Declaration {
    Function {
        name: Identifier,
        params: ParamList,
        body: Option<Block>,
        storage_class: Option<StorageClass>,
    },
    Var {
        name: Identifier,
        init: Option<Expression>,
        storage_class: Option<StorageClass>,
    },
}
/*
pub struct Declaration {
    pub name: Rc<Identifier>,
    pub init: Option<Expression>,
}
*/

fn expression(tokens: &mut TokenIter, min_precedence: Option<u8>) -> Result<Expression, Error> {
    let precedence = min_precedence.unwrap_or(0);

    let mut left = factor(tokens)?.into();

    while let Some(operator) = binary_operator(tokens, precedence) {
        match operator {
            BinaryOperator::Equals => {
                let right = expression(tokens, Some(operator.precedence()))?;
                left = Expression::Assignment((left, right).into());
            }
            BinaryOperator::Ternary => {
                let middle = expression(tokens, None)?;
                tokens.consume(Token::Colon)?;
                let right = expression(tokens, Some(operator.precedence()))?;
                left = Expression::Conditional {
                    condition: left.into(),
                    r#true: middle.into(),
                    r#false: right.into(),
                }
            }
            operator if operator.compound() => {
                let right = expression(tokens, Some(operator.precedence()))?;
                left = Expression::Binary(Binary {
                    left: Box::new(left),
                    right: Box::new(right),
                    operator,
                });
            }
            operator => {
                let right = expression(tokens, Some(operator.precedence() + 1))?;
                left = Expression::Binary(Binary {
                    left: Box::new(left),
                    right: Box::new(right),
                    operator,
                });
            }
        }
    }
    Ok(left)
}

fn binary_operator(tokens: &mut TokenIter, min_precedence: u8) -> Option<BinaryOperator> {
    let token = match tokens.peek()? {
        Token::Plus => Some(BinaryOperator::Add),
        Token::Minus => Some(BinaryOperator::Subtract),
        Token::Asterisk => Some(BinaryOperator::Multiply),
        Token::Slash => Some(BinaryOperator::Divide),
        Token::Percent => Some(BinaryOperator::Remainder),

        Token::LeftShift => Some(BinaryOperator::LeftShift),
        Token::RightShift => Some(BinaryOperator::RightShift),
        Token::Ampersand => Some(BinaryOperator::BitAnd),
        Token::Bar => Some(BinaryOperator::BitOr),
        Token::Caret => Some(BinaryOperator::Xor),

        Token::LogicalAnd => Some(BinaryOperator::LogAnd),
        Token::LogicalOr => Some(BinaryOperator::LogOr),

        Token::EqualTo => Some(BinaryOperator::EqualTo),
        Token::NotEqual => Some(BinaryOperator::NotEqual),
        Token::LessThan => Some(BinaryOperator::LessThan),
        Token::GreaterThan => Some(BinaryOperator::GreaterThan),
        Token::Leq => Some(BinaryOperator::Leq),
        Token::Geq => Some(BinaryOperator::Geq),

        Token::Equals => Some(BinaryOperator::Equals),
        Token::PlusEqual => Some(BinaryOperator::PlusEquals),
        Token::MinusEqual => Some(BinaryOperator::MinusEquals),
        Token::TimesEqual => Some(BinaryOperator::TimesEqual),
        Token::DivEqual => Some(BinaryOperator::DivEqual),
        Token::PercentEqual => Some(BinaryOperator::RemEqual),

        Token::BitAndEqual => Some(BinaryOperator::BitAndEqual),

        Token::BitOrEqual => Some(BinaryOperator::BitOrEqual),

        Token::BitXorEqual => Some(BinaryOperator::BitXorEqual),
        Token::LeftShiftEqual => Some(BinaryOperator::LeftShiftEqual),
        Token::RightShiftEqual => Some(BinaryOperator::RightShiftEqual),

        Token::QuestionMark => Some(BinaryOperator::Ternary),

        _ => None,
    }?;
    if token.precedence() >= min_precedence {
        tokens.next();
        Some(token)
    } else {
        None
    }
}
use super::lex;
fn factor(tokens: &mut TokenIter) -> Result<Factor, Error> {
    let factor = match tokens.consume_any()? {
        Token::Increment => {
            let exp = Box::new(factor(tokens)?.into());
            Ok(Factor::PrefixIncrement(exp))
        }
        Token::Decrement => {
            let exp = Box::new(factor(tokens)?.into());
            Ok(Factor::PrefixDecrement(exp))
        }
        Token::Constant(lex::Constant::Integer(c)) => Ok(Factor::Int(c)),

        t @ (Token::Minus | Token::Tilde | Token::Not) => {
            let operator = if t == Token::Minus {
                UnaryOperator::Negate
            } else if t == Token::Tilde {
                UnaryOperator::Complement
            } else {
                UnaryOperator::Not
            };
            let factor = Box::new(factor(tokens)?);
            Ok(Factor::Unary(Unary {
                exp: factor,
                op: operator,
            }))
        }
        Token::OpenParen => {
            let exp = Box::new(expression(tokens, None)?);
            tokens.consume(Token::CloseParen)?;
            Ok(Factor::Nested(exp))
        }
        Token::Identifier(ident) => {
            if tokens.peek() == Some(&Token::OpenParen) {
                tokens.next();
                let args = argument_list(tokens)?;
                Ok(Factor::FunctionCall { name: ident, args })
            } else {
                Ok(Factor::Var(ident))
            }
        }

        e => {
            eprintln!("errored in {e:?}");
            Err(Error::ExpectedExpression)
        }
    }?;
    match tokens.next_if(|x| x == &Token::Increment || x == &Token::Decrement) {
        Some(Token::Increment) => Ok(Factor::PostfixIncrement(Box::new(factor.into()))),
        Some(Token::Decrement) => Ok(Factor::PostfixDecrement(Box::new(factor.into()))),
        _ => Ok(factor),
    }
}

fn argument_list(tokens: &mut TokenIter) -> Result<Box<[Expression]>, Error> {
    if tokens.consume(Token::CloseParen).is_ok() {
        return Ok(Box::new([]));
    }

    let mut list = Vec::new();
    loop {
        list.push(expression(tokens, None)?);
        match tokens.consume_any()? {
            Token::Comma => {}
            Token::CloseParen => {
                break;
            }
            _ => return Err(Error::Catchall("idk")),
        }
    }

    Ok(list.into())
}

#[derive(Debug, Clone)]
pub enum Expression {
    Assignment(Box<(Self, Self)>),
    Binary(Binary),

    //factors
    PostfixIncrement(Box<Self>),
    PostfixDecrement(Box<Self>),
    PrefixIncrement(Box<Self>),
    PrefixDecrement(Box<Self>),

    Var(Identifier),
    Int(u64),
    Unary(Unary),
    Nested(Box<Self>),

    Conditional {
        condition: Box<Self>,
        r#true: Box<Self>,
        r#false: Box<Self>,
    },
    FunctionCall {
        name: Identifier,
        args: Box<[Self]>,
    },
}

impl Expression {
    pub const fn lvalue(&self) -> bool {
        match self {
            Self::Var(_) => true,
            Self::Nested(e) => e.lvalue(),
            _ => false,
        }
    }

    pub const fn number(&self) -> Option<u64> {
        match self {
            Self::Nested(e) => e.number(),
            Self::Int(val) => Some(*val),
            _ => None,
        }
    }
}

impl TryFrom<Expression> for Factor {
    type Error = ();

    fn try_from(expression: Expression) -> Result<Factor, ()> {
        match expression {
            Expression::Var(v) => Ok(Self::Var(v)),
            Expression::Int(i) => Ok(Self::Int(i)),
            Expression::Unary(u) => Ok(Self::Unary(u)),
            Expression::PrefixIncrement(p) => Ok(Self::PrefixIncrement(p)),
            Expression::PrefixDecrement(p) => Ok(Self::PrefixDecrement(p)),
            Expression::PostfixIncrement(p) => Ok(Self::PostfixIncrement(p)),
            Expression::PostfixDecrement(p) => Ok(Self::PostfixDecrement(p)),

            Expression::Nested(e) => Ok(Self::Nested(e)),

            _ => Err(()),
        }
    }
}

impl From<Factor> for Expression {
    fn from(factor: Factor) -> Self {
        match factor {
            Factor::Var(v) => Self::Var(v),
            Factor::Int(i) => Self::Int(i),
            Factor::Unary(u) => Self::Unary(u),
            Factor::PrefixIncrement(p) => Self::PrefixIncrement(p),
            Factor::PrefixDecrement(p) => Self::PrefixDecrement(p),
            Factor::PostfixIncrement(p) => Self::PostfixIncrement(p),
            Factor::PostfixDecrement(p) => Self::PostfixDecrement(p),

            Factor::Nested(e) => Self::Nested(e),

            Factor::FunctionCall { name, args } => Self::FunctionCall { name, args },
        }
    }
}

#[derive(Debug, Clone)]
pub enum Factor {
    Var(Identifier),
    Int(u64),
    Unary(Unary),
    PrefixIncrement(Box<Expression>),
    PostfixIncrement(Box<Expression>),
    PrefixDecrement(Box<Expression>),
    PostfixDecrement(Box<Expression>),
    Nested(Box<Expression>),
    FunctionCall {
        name: Identifier,
        args: Box<[Expression]>,
    },
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Fixness {
    Prefix,
    Postfix,
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub exp: Box<Factor>,
    pub op: UnaryOperator,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum UnaryOperator {
    Complement,
    Negate,
    Not,
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub operator: BinaryOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    BitAnd,
    BitOr,
    Xor,
    LeftShift,
    RightShift,
    LogAnd,
    LogOr,
    EqualTo,
    NotEqual,
    LessThan,
    GreaterThan,
    Leq,
    Geq,

    Equals,

    PlusEquals,
    MinusEquals,
    TimesEqual,
    DivEqual,
    RemEqual,
    BitAndEqual,
    BitOrEqual,
    BitXorEqual,
    LeftShiftEqual,
    RightShiftEqual,

    Ternary,
}

impl BinaryOperator {
    const fn precedence(&self) -> u8 {
        match self {
            Self::Equals
            | Self::PlusEquals
            | Self::MinusEquals
            | Self::TimesEqual
            | Self::DivEqual
            | Self::RemEqual
            | Self::BitAndEqual
            | Self::BitOrEqual
            | Self::BitXorEqual
            | Self::LeftShiftEqual
            | Self::RightShiftEqual => 1,
            Self::Ternary => 3,
            Self::LogOr => 5,
            Self::LogAnd => 10,
            Self::BitOr => 15,
            Self::Xor => 20,
            Self::BitAnd => 25,
            Self::EqualTo | Self::NotEqual => 30,
            Self::LessThan | Self::GreaterThan | Self::Leq | Self::Geq => 35,
            Self::LeftShift | Self::RightShift => 40,
            Self::Add | Self::Subtract => 45,
            Self::Multiply | Self::Divide | Self::Remainder => 50,
        }
    }

    pub const fn assignment_operator(&self) -> bool {
        matches!(self, Self::Equals) || self.compound()
    }

    pub const fn compound(&self) -> bool {
        matches!(
            self,
            Self::PlusEquals
                | Self::MinusEquals
                | Self::TimesEqual
                | Self::DivEqual
                | Self::RemEqual
                | Self::BitAndEqual
                | Self::BitOrEqual
                | Self::BitXorEqual
                | Self::LeftShiftEqual
                | Self::RightShiftEqual
        )
    }

    pub const fn relational(&self) -> bool {
        matches!(
            self,
            Self::EqualTo
                | Self::NotEqual
                | Self::LessThan
                | Self::GreaterThan
                | Self::Leq
                | Self::Geq
        )
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Program(\n{:?}\n)", self.0)
    }
}

impl Display for FunctionDeclaration {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "Function(\n{}({}){{{:?}}}\n)",
            self.name, self.params, self.body
        )
    }
}

impl Display for ParamList {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            ParamList::Void => write!(f, "void"),
            ParamList::Int(identifiers) => {
                for ident in identifiers {
                    write!(f, "int {ident}")?
                }
                Ok(())
            }
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Statement::Ret(ret) => write!(f, "Return(\n{:?}\n)", ret),
            Statement::Exp(e) => write!(f, "(\n{:?}\n)", e),
            Statement::Null => write!(f, "(\nnull\n)"),

            Statement::If {
                condition,
                then,
                r#else,
            } => write!(f, "if(\n{condition:?}\n){then}{else:?}"),
            Statement::Label { label, body } => write!(f, "LABEL\n{label}:{body}\n"),

            Statement::Goto(name) => write!(f, "goto\n{name}:\n"),

            Statement::Compound(Block(block)) => writeln!(f, "{{{block:?}}}"),
            _ => todo!(),
        }
    }
}

impl Display for Label {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Label::Default => write!(f, "default"),
            Label::Named(name) => write!(f, "{name}"),
            Label::Case(case) => write!(f, "case {case:?}"),
        }
    }
}

impl From<Constant> for Factor {
    fn from(c: Constant) -> Self {
        Self::Int(c.0)
    }
}

impl From<Unary> for Factor {
    fn from(u: Unary) -> Self {
        Self::Unary(u)
    }
}

#[derive(Debug)]
pub struct Constant(pub u64);

impl Display for Constant {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Constant({})", self.0)
    }
}

#[derive(Debug)]
pub struct DebugError {
    pub e: Error,
    pub line: usize,
}

const fn expected_eof<T>() -> Result<T, Error> {
    Err(Error::UnexpectedEof)
}

#[derive(Debug)]
pub enum Error {
    UnexpectedEof,
    Expected(Token),
    ExpectedIdentifier,
    ExpectedConstant,
    ExpectedExpression,
    ExpectedAnyKeyword,
    ExpectedKeyword(Keyword),
    Catchall(&'static str),
    ExtraStuff,
    DoubleDef,
    NoType,
    ConflictingLinkage,
}
