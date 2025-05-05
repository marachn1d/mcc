use ast::{Constant, DebugToken, Token};
use symtab::{Key, Store};
use util::SliceIter;

pub fn tokenize<'a>(bytes: &'a Store) -> Result<Box<[DebugToken<'a>]>, Error> {
    // we know they're ascii characters going in, so we're gonna use bytes so we can use the slice
    // pattern below :)
    let mut iter = SliceIter::new(bytes.as_ref());

    let mut tokens = Vec::new();
    let mut cur_line = 0;
    while let Some(token) = lex_slice(&mut iter, &mut cur_line)? {
        tokens.push(DebugToken {
            token,
            line: cur_line,
        });
    }
    Ok(tokens.into())
}

fn lex_slice<'a: 'b, 'b>(
    iter: &mut SliceIter<'a>,
    cur_line: &mut usize,
) -> Result<Option<Token<'a>>, Error> {
    match iter.as_bytes() {
        [b'<', b'<', b'=', ..] => {
            iter.next();
            iter.next();
            iter.next();
            Ok(Some(Token::LeftShiftEqual))
        }
        [b'>', b'>', b'=', ..] => {
            iter.next();
            iter.next();
            iter.next();
            Ok(Some(Token::RightShiftEqual))
        }
        [b'-', b'-', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::Decrement))
        }
        [b'<', b'<', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::LeftShift))
        }
        [b'&', b'&', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::LogicalAnd))
        }
        [b'|', b'|', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::LogicalOr))
        }
        [b'!', b'=', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::NotEqual))
        }
        [b'=', b'=', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::EqualTo))
        }
        [b'>', b'=', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::Geq))
        }
        [b'<', b'=', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::Leq))
        }
        [b'>', b'>', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::RightShift))
        }
        [b'+', b'=', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::PlusEqual))
        }
        [b'+', b'+', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::Increment))
        }
        [b'-', b'=', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::MinusEqual))
        }
        [b'*', b'=', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::TimesEqual))
        }
        [b'/', b'=', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::DivEqual))
        }
        [b'%', b'=', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::PercentEqual))
        }
        [b'&', b'=', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::BitAndEqual))
        }
        [b'|', b'=', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::BitOrEqual))
        }
        [b'^', b'=', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::BitXorEqual))
        }

        [a, ..] if !a.is_ascii() => error("Invalid Character (I Only Accept Ascii :[)"),
        [a, ..] if a.is_ascii_whitespace() => {
            if *a == b'\n' {
                *cur_line += 1;
            }
            iter.next();
            lex_slice(iter, cur_line)
        }
        [_, ..] => {
            let lit_start = iter.as_slice();
            // safe because we know there's 1 item there
            let a = unsafe { iter.next().unwrap_unchecked() };
            Ok(Some(match a {
                '(' => Token::OpenParen,
                ')' => Token::CloseParen,
                '{' => Token::OpenBrace,
                ';' => Token::Semicolon,
                '}' => Token::CloseBrace,
                '~' => Token::Tilde,
                '0'..='9' => {
                    let byte = AsciiDigit::from_char(a).unwrap();
                    Token::Const(constant_number(byte, iter)?)
                }
                '-' => Token::Minus,
                '+' => Token::Plus,
                '*' => Token::Asterisk,
                '/' => Token::Slash,
                '%' => Token::Percent,
                '&' => Token::Ampersand,
                '|' => Token::Bar,
                '^' => Token::Caret,
                '!' => Token::Not,
                '<' => Token::LessThan,
                '>' => Token::GreaterThan,
                '=' => Token::Equals,
                ',' => Token::Comma,
                '?' => Token::QuestionMark,
                ':' => Token::Colon,
                _ => literal(lit_start, iter)?,
            }))
        }
        [] => Ok(None),
    }
}

impl AsciiDigit {
    const fn from_char(int: char) -> Option<Self> {
        match int {
            '0' => Some(AsciiDigit::Zero),
            '1' => Some(AsciiDigit::One),
            '2' => Some(AsciiDigit::Two),
            '3' => Some(AsciiDigit::Three),
            '4' => Some(AsciiDigit::Four),
            '5' => Some(AsciiDigit::Five),
            '6' => Some(AsciiDigit::Six),
            '7' => Some(AsciiDigit::Seven),
            '8' => Some(AsciiDigit::Eight),
            '9' => Some(AsciiDigit::Nine),
            _ => None,
        }
    }
}

fn constant_number(start: AsciiDigit, iter: &mut SliceIter) -> Result<Constant, Error> {
    let mut bytes = vec![start];
    while let Some(constant) = next_if_number(iter) {
        bytes.push(constant);
    }

    match iter.peek() {
        Some('l') => {
            iter.next();
            Ok(Constant::Long(parse_long(&bytes)))
        }
        Some(x) if !word_character(x) => {
            let long = parse_long(&bytes);
            i32::try_from(long)
                .map(Constant::Int)
                .or(Ok(Constant::Long(long)))
        }
        _ => Err(Error::InvalidConstant),
    }
}

// assumes that lit_slice is 1 behind iter
fn literal<'a>(lit_slice: &'a str, iter: &mut SliceIter) -> Result<Token<'a>, Error> {
    let mut end = 1;
    while next_if_word(iter).is_some() {
        end += 1;
    }

    if iter.peek().is_some_and(|byte| !word_character(byte)) {
        Ok(match &lit_slice[0..end] {
            "int" => Token::Int,
            "return" => Token::Return,
            "void" => Token::Void,
            "if" => Token::If,
            "else" => Token::Else,
            "goto" => Token::Goto,
            "do" => Token::Do,
            "while" => Token::While,
            "for" => Token::For,
            "break" => Token::Break,
            "continue" => Token::Continue,
            "switch" => Token::Switch,
            "case" => Token::Case,
            "default" => Token::Default,
            "static" => Token::Static,
            "extern" => Token::Extern,
            "long" => Token::Long,
            _ => Token::Ident(Key::new(lit_slice)),
        })
    } else {
        Err(Error::InvalidLiteral)
    }
}

/*
fn identifier<'a>(str: &'a AsciiStr) -> Result<Token<'a>, Error> {
    if word_start(str[0]) {
        Ok(Token::Ident(str))
    } else {
        Err(Error::InvalidIdentifier)
    }
}
*/

const fn _word_boundary(byte: char) -> bool {
    !word_character(byte)
}

fn word_start(byte: char) -> bool {
    byte.is_alphabetic() || byte == '_'
}

const fn word_character(byte: char) -> bool {
    byte.is_ascii_alphanumeric() || byte == '_'
}

fn next_if_number(iter: &mut SliceIter) -> Option<AsciiDigit> {
    iter.next_if_map(AsciiDigit::from_char)
}

#[derive(Clone, Copy)]
enum AsciiDigit {
    Zero = 0,
    One = 1,
    Two = 2,
    Three = 3,
    Four = 4,
    Five = 5,
    Six = 6,
    Seven = 7,
    Eight = 8,
    Nine = 9,
}

fn parse_long(slice: &[AsciiDigit]) -> i64 {
    let mut cur = 0i64;
    for (place, digit) in slice.iter().map(|&x| i64::from(x as u8)).rev().enumerate() {
        cur += 10i64.pow(place as u32) * digit;
    }
    cur
}

fn next_if_word(iter: &mut SliceIter) -> Option<char> {
    iter.next_if(word_character)
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Invalid Constant")]
    InvalidConstant,
    #[error("Invalid Literal")]
    InvalidLiteral,
    #[error("Invalid Identifier")]
    InvalidIdentifier,
    #[error("Non Ascii Character")]
    NotAscii,
    #[error("{0}")]
    Other(String),
}

fn error<T>(message: &str) -> Result<T, Error> {
    Err(Error::Other(message.into()))
}
