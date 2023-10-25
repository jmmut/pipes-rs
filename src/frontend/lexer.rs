use crate::AnyError;
use std::iter::Peekable;
use std::str::Bytes;

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Token {
    Number(i64),
    Operator { operator: Operator },
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Operator {
    Add,
    Substract,
    Ignore,
}

impl Token {
    #[allow(unused)]
    pub fn add() -> Self {
        Token::Operator {
            operator: Operator::Add,
        }
    }
}

pub type Tokens = Vec<Token>;

pub fn lex<S: AsRef<str>>(code_text: S) -> Result<Tokens, AnyError> {
    let mut tokens = Tokens::new();
    let mut bytes = code_text.as_ref().bytes().peekable();
    while let Some(letter) = bytes.peek() {
        let letter = *letter;
        if let Some(digit) = parse_digit(letter) {
            let value = consume_number(digit, &mut bytes)?;
            tokens.push(Token::Number(value));
        } else if let Some(operator) = parse_operator(letter) {
            tokens.push(Token::Operator { operator });
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
    if letter >= b'0' && letter <= b'9' {
        return Some((letter - b'0') as i64);
    } else {
        None
    }
}

pub fn parse_operator(letter: u8) -> Option<Operator> {
    match letter {
        b'+' => Some(Operator::Add),
        b'-' => Some(Operator::Substract),
        b';' => Some(Operator::Ignore),
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_overflow_positive() {
        lex("9223372036854775808").expect_err("should have failed");
    }

    #[test]
    fn test_several_digits() {
        let tokens = lex("57").unwrap();
        assert_eq!(tokens, vec![Token::Number(57)])
    }
    #[test]
    fn test_operator() {
        let tokens = lex("+").unwrap();
        assert_eq!(tokens, vec![Token::add()])
    }

    #[test]
    fn test_adding_numbers() {
        let tokens = lex("5+7+12+34").unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Number(5),
                Token::add(),
                Token::Number(7),
                Token::add(),
                Token::Number(12),
                Token::add(),
                Token::Number(34)
            ]
        );
    }
}
