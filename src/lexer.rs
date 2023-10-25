use crate::AnyError;
use std::str::Bytes;

pub enum Token {
    Number(i64),
}

pub type Tokens = Vec<Token>;

pub fn lex<S: AsRef<str>>(code_text: S) -> Result<Tokens, AnyError> {
    let mut tokens = Tokens::new();
    let mut bytes = code_text.as_ref().bytes();
    while let Some(letter) = bytes.next() {
        if is_number(letter) {
            let value = consume_number(letter, &mut bytes)?;
            tokens.push(Token::Number(value));
        } else {
            return Err(format!(
                "unsupported expression starting with byte {} ('{}')",
                letter, letter as char
            ))?;
        }
    }

    Ok(tokens)
}

pub fn is_number(letter: u8) -> bool {
    letter >= b'0' && letter < b'9'
}

pub fn consume_number(letter: u8, _iter: &mut Bytes) -> Result<i64, AnyError> {
    Ok((letter - b'0') as i64)
}
