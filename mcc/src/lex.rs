use ast::{Constant, DebugToken, Ident, Token};
use util::SliceIter;

pub fn tokenize(bytes: &[u8]) -> Result<Box<[DebugToken]>, Error> {
    let mut iter = SliceIter::new(bytes);

    let mut tokens = Vec::new();
    let mut cur_line = 0;
    let mut cur_char = 0;
    while let Some(token) = lex_slice(&mut iter, &mut cur_line, &mut cur_char)? {
        tokens.push(DebugToken {
            token,
            line: cur_line,
            char: cur_char,
        });
    }
    Ok(tokens.into())
}

fn lex_slice(
    iter: &mut SliceIter<u8>,
    cur_line: &mut usize,
    cur_char: &mut usize,
) -> Result<Option<Token>, Error> {
    match iter.as_slice() {
        [b'<', b'<', b'=', ..] => {
            for _ in 0..3 {
                iter.next();
            }
            *cur_char += 3;

            Ok(Some(Token::LeftShiftEqual))
        }
        [b'>', b'>', b'=', ..] => {
            for _ in 0..3 {
                iter.next();
            }
            *cur_char += 3;
            Ok(Some(Token::RightShiftEqual))
        }
        [b'-', b'-', ..] => {
            iter.next();
            iter.next();
            *cur_char += 2;
            Ok(Some(Token::Decrement))
        }
        [b'<', b'<', ..] => {
            iter.next();
            iter.next();
            *cur_char += 2;
            Ok(Some(Token::LeftShift))
        }
        [b'&', b'&', ..] => {
            iter.next();
            iter.next();
            *cur_char += 2;
            Ok(Some(Token::LogicalAnd))
        }
        [b'|', b'|', ..] => {
            iter.next();
            iter.next();
            *cur_char += 2;
            Ok(Some(Token::LogicalOr))
        }
        [b'!', b'=', ..] => {
            iter.next();
            iter.next();
            *cur_char += 2;
            Ok(Some(Token::NotEqual))
        }
        [b'=', b'=', ..] => {
            iter.next();
            iter.next();
            *cur_char += 2;
            Ok(Some(Token::EqualTo))
        }
        [b'>', b'=', ..] => {
            iter.next();
            iter.next();
            *cur_char += 2;
            Ok(Some(Token::Geq))
        }
        [b'<', b'=', ..] => {
            iter.next();
            iter.next();
            *cur_char += 2;
            Ok(Some(Token::Leq))
        }
        [b'>', b'>', ..] => {
            iter.next();
            iter.next();
            *cur_char += 2;
            Ok(Some(Token::RightShift))
        }
        [b'+', b'=', ..] => {
            iter.next();
            iter.next();
            *cur_char += 2;
            Ok(Some(Token::PlusEqual))
        }
        [b'+', b'+', ..] => {
            iter.next();
            iter.next();
            *cur_char += 2;
            Ok(Some(Token::Increment))
        }
        [b'-', b'=', ..] => {
            iter.next();
            iter.next();
            *cur_char += 2;
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
            lex_slice(iter, cur_line, cur_char)
        }
        [a, ..] => {
            iter.next();
            Ok(Some(match a {
                b'(' => Token::OpenParen,
                b')' => Token::CloseParen,
                b'{' => Token::OpenBrace,
                b';' => Token::Semicolon,
                b'}' => Token::CloseBrace,
                b'~' => Token::Tilde,
                b'0'..=b'9' => {
                    let byte = AsciiDigit::from_int(*a).unwrap();
                    Token::Const(constant_number(byte, iter)?)
                }
                b'-' => Token::Minus,
                b'+' => Token::Plus,
                b'*' => Token::Asterisk,
                b'/' => Token::Slash,
                b'%' => Token::Percent,
                b'&' => Token::Ampersand,
                b'|' => Token::Bar,
                b'^' => Token::Caret,
                b'!' => Token::Not,
                b'<' => Token::LessThan,
                b'>' => Token::GreaterThan,
                b'=' => Token::Equals,
                b',' => Token::Comma,
                b'?' => Token::QuestionMark,
                b':' => Token::Colon,
                a => literal(*a, iter)?,
            }))
        }
        [] => Ok(None),
    }
}

impl AsciiDigit {
    const fn from_int(int: u8) -> Option<Self> {
        match int {
            b'0' => Some(AsciiDigit::Zero),
            b'1' => Some(AsciiDigit::One),
            b'2' => Some(AsciiDigit::Two),
            b'3' => Some(AsciiDigit::Three),
            b'4' => Some(AsciiDigit::Four),
            b'5' => Some(AsciiDigit::Five),
            b'6' => Some(AsciiDigit::Six),
            b'7' => Some(AsciiDigit::Seven),
            b'8' => Some(AsciiDigit::Eight),
            b'9' => Some(AsciiDigit::Nine),
            _ => None,
        }
    }
}

fn constant_number(start: AsciiDigit, iter: &mut SliceIter<u8>) -> Result<Constant, Error> {
    let mut bytes = vec![start];
    while let Some(constant) = next_if_number(iter) {
        bytes.push(constant);
    }

    match iter.as_slice() {
        [b'l' | b'L', b'u' | b'U', ..] | [b'u' | b'U', b'l' | b'L', ..] => {
            iter.next();
            iter.next();
            Ok(Constant::new_ulong(parse_ulong(&bytes)).as_shrunk())
        }
        [x, ..] => match x {
            b'l' | b'L' => {
                iter.next();
                Ok(Constant::new_long(parse_long(&bytes)))
            }
            b'u' | b'U' => {
                iter.next();
                Ok(Constant::new_ulong(parse_ulong(&bytes)).as_shrunk())
            }
            x if !word_character(*x) => {
                let long = parse_long(&bytes);
                if let Ok(int) = i32::try_from(long) {
                    Ok(Constant::new_int(int))
                } else {
                    Ok(Constant::new_long(long))
                }
            }
            other => Err(Error::InvalidConstant(
                bytes
                    .iter()
                    .map(|x| *x as u8 as char)
                    .chain(std::iter::once(*other as char))
                    .collect(),
            )),
        },
        [] => Err(Error::UnexpectedEof),
    }
}

fn literal(byte: u8, iter: &mut SliceIter<u8>) -> Result<Token, Error> {
    let mut bytes = vec![byte];
    while let Some(character) = next_if_word(iter) {
        bytes.push(character);
    }
    if iter.peek().is_some_and(|byte| !word_character(byte)) {
        Ok(match bytes.as_slice() {
            b"int" => Token::Int,
            b"return" => Token::Return,
            b"void" => Token::Void,
            b"if" => Token::If,
            b"else" => Token::Else,
            b"goto" => Token::Goto,
            b"do" => Token::Do,
            b"while" => Token::While,
            b"for" => Token::For,
            b"break" => Token::Break,
            b"continue" => Token::Continue,
            b"switch" => Token::Switch,
            b"case" => Token::Case,
            b"default" => Token::Default,
            b"static" => Token::Static,
            b"extern" => Token::Extern,
            b"long" => Token::Long,
            b"unsigned" => Token::Unsigned,
            b"signed" => Token::Signed,
            _ => identifier(&bytes)?.into(),
        })
    } else {
        Err(Error::InvalidLiteral)
    }
}

fn identifier(bytes: &[u8]) -> Result<Ident, Error> {
    if word_start(bytes[0]) && bytes[1..].iter().all(|&x| word_character(x)) {
        Ok(unsafe { String::from_utf8_unchecked(bytes.to_vec()) })
    } else {
        Err(Error::InvalidIdentifier)
    }
}

const fn word_start(byte: u8) -> bool {
    match byte {
        b if b.is_ascii_alphabetic() => true,
        b'_' => true,
        _ => false,
    }
}

const fn word_character(byte: u8) -> bool {
    match byte {
        b if b.is_ascii_alphanumeric() => true,
        b'_' => true,
        _ => false,
    }
}

fn next_if_number(iter: &mut SliceIter<u8>) -> Option<AsciiDigit> {
    iter.next_if_map(AsciiDigit::from_int)
}

#[derive(Clone, Copy, Debug)]
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

fn parse_ulong(slice: &[AsciiDigit]) -> u64 {
    let mut cur = 0u64;
    for (place, digit) in slice.iter().map(|&x| u64::from(x as u8)).rev().enumerate() {
        cur += 10u64.pow(place as u32) * digit;
    }
    cur
}

fn parse_long(slice: &[AsciiDigit]) -> i64 {
    let mut cur = 0i64;
    for (place, digit) in slice.iter().map(|&x| i64::from(x as u8)).rev().enumerate() {
        cur += 10i64.pow(place as u32) * digit;
    }
    cur
}

fn next_if_word(iter: &mut SliceIter<u8>) -> Option<u8> {
    iter.next_if(word_character)
}

#[derive(Debug)]
pub enum Error {
    InvalidConstant(String),
    InvalidLiteral,
    InvalidIdentifier,
    NotAscii,
    Other(String),
    UnexpectedEof,
}

fn error<T>(message: &str) -> Result<T, Error> {
    Err(Error::Other(message.into()))
}
