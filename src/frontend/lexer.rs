use crate::common::context;
use crate::AnyError;
use std::iter::Peekable;
use std::str::Bytes;

#[derive(Clone, PartialEq, Debug)]
pub enum Token {
    Number(i64),
    Operator(Operator),
    Identifier(String),
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,
    OpenParenthesis,
    CloseParenthesis,
    // Comma,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Operator {
    Add,
    Substract,
    Ignore,
    Call,
    Get,
    Type,
    Assignment,
}

impl Token {
    #[allow(unused)]
    pub fn add() -> Self {
        Token::Operator(Operator::Add)
    }
}

pub type Tokens = Vec<Token>;

pub fn lex<S: AsRef<str>>(code_text: S) -> Result<Tokens, AnyError> {
    context("Lexer", try_lex(code_text))
}
fn try_lex<S: AsRef<str>>(code_text: S) -> Result<Tokens, AnyError> {
    let mut tokens = Tokens::new();
    let mut bytes = code_text.as_ref().bytes().peekable();
    while let Some(letter) = bytes.peek() {
        let letter = *letter;
        if let Some(digit) = parse_digit(letter) {
            let value = consume_number(digit, &mut bytes)?;
            tokens.push(Token::Number(value));
        } else if let Some(operator) = parse_operator(letter) {
            tokens.push(Token::Operator(operator));
            bytes.next();
        } else if let Some(token) = parse_grouping(letter) {
            tokens.push(token);
            bytes.next();
        } else if let Some(letter) = parse_letter(letter) {
            let name = consume_identifier(letter, &mut bytes)?;
            tokens.push(Token::Identifier(name));
        } else if is_space(letter) {
            bytes.next();
        } else {
            return Err(format!(
                "unsupported expression starting with byte {} ('{}')",
                letter, letter as char
            ))?;
        }
    }

    Ok(tokens)
}

pub fn parse_digit(letter: u8) -> Option<i64> {
    if is_digit(letter) {
        return Some((letter - b'0') as i64);
    } else {
        None
    }
}

fn is_digit(letter: u8) -> bool {
    letter >= b'0' && letter <= b'9'
}

fn is_space(letter: u8) -> bool {
    [b' ', b'\n', b'\t', b'\r'].contains(&letter)
}

pub fn parse_letter(letter: u8) -> Option<u8> {
    if letter >= b'a' && letter <= b'z' || letter >= b'A' && letter <= b'Z' || letter == b'_' {
        return Some(letter);
    } else {
        None
    }
}
pub fn parse_alphanum(letter: u8) -> Option<u8> {
    parse_letter(letter).or_else(|| if is_digit(letter) { Some(letter) } else { None })
}

pub fn parse_operator(letter: u8) -> Option<Operator> {
    match letter {
        b'+' => Some(Operator::Add),
        b'-' => Some(Operator::Substract),
        b';' => Some(Operator::Ignore),
        b'|' => Some(Operator::Call),
        b'#' => Some(Operator::Get),
        b':' => Some(Operator::Type),
        b'=' => Some(Operator::Assignment),
        _ => None,
    }
}
pub fn parse_grouping(letter: u8) -> Option<Token> {
    match letter {
        b'[' => Some(Token::OpenBracket),
        b']' => Some(Token::CloseBracket),
        b'{' => Some(Token::OpenBrace),
        b'}' => Some(Token::CloseBrace),
        b'(' => Some(Token::OpenParenthesis),
        b')' => Some(Token::CloseParenthesis),
        // b',' => Some(Token::Comma),
        _ => None,
    }
}

pub fn consume_number(first_digit: i64, iter: &mut Peekable<Bytes>) -> Result<i64, AnyError> {
    let mut accumulated: i64 = first_digit;
    loop {
        iter.next();
        if let Some(letter) = iter.peek() {
            if let Some(new_digit) = parse_digit(*letter) {
                accumulated = maybe_add_digit(accumulated, new_digit)?;
                continue;
            }
        }
        return Ok(accumulated);
    }
}

fn maybe_add_digit(mut accumulated: i64, new_digit: i64) -> Result<i64, AnyError> {
    const OVERFLOW_MESSAGE: &'static str = "Cant' fit number in 64 bits";
    return if i64::MAX / 10 < accumulated.abs() {
        Err(OVERFLOW_MESSAGE)?
    } else {
        accumulated *= 10;
        if i64::MAX - new_digit < accumulated {
            Err(OVERFLOW_MESSAGE)?
        } else {
            Ok(accumulated + new_digit)
        }
    };
}

pub fn consume_identifier(
    first_letter: u8,
    iter: &mut Peekable<Bytes>,
) -> Result<String, AnyError> {
    let mut accumulated = vec![first_letter];
    loop {
        iter.next();
        if let Some(letter) = iter.peek() {
            if let Some(new_letter) = parse_alphanum(*letter) {
                accumulated.push(new_letter);
                continue;
            }
        }
        return Ok(String::from_utf8(accumulated)?);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::lexer::Operator::Add;
    use Token::{CloseBracket, Identifier, Number, OpenBracket, Operator};

    #[test]
    fn test_overflow_positive() {
        lex("9223372036854775808").expect_err("should have failed");
    }

    #[test]
    fn test_several_digits() {
        assert_eq!(lex("57").unwrap(), vec![Number(57)])
    }
    #[test]
    fn test_operator() {
        assert_eq!(lex("+").unwrap(), vec![Operator(Add)])
    }

    #[test]
    fn test_adding_numbers() {
        assert_eq!(
            lex("5+7+12+34").unwrap(),
            vec![
                Number(5),
                Operator(Add),
                Number(7),
                Operator(Add),
                Number(12),
                Operator(Add),
                Number(34)
            ]
        );
    }

    #[test]
    fn test_identifier() {
        assert_eq!(
            lex("5asdf12+34").unwrap(),
            vec![
                Number(5),
                Identifier("asdf12".to_string()),
                Operator(Add),
                Number(34)
            ]
        );
    }
    #[test]
    fn test_list() {
        assert_eq!(
            lex("[5 6 7]").unwrap(),
            vec![OpenBracket, Number(5), Number(6), Number(7), CloseBracket,]
        );
    }
}
