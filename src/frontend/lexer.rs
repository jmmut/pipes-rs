use crate::common::context;
use crate::AnyError;
use std::iter::Peekable;
use std::str::Bytes;

#[derive(Clone, PartialEq, Debug)]
pub enum Token {
    Number(i64),
    Operator(Operator),
    Identifier(String),
    String(Vec<u8>),
    Keyword(Keyword),
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
    Multiply,
    Divide,
    Modulo,
    Ignore,
    Call,
    Get,
    Type,
    Assignment,
    Overwrite,
    Concatenate,
    Comparison(Comparison),
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Comparison {
    Equals,
    LessThan,
    GreaterThan,
    LessThanEquals,
    GreaterThanEquals,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Keyword {
    Function,
    Loop,
    LoopOr,
    Times,
    Replace,
    Branch,
}
impl Keyword {
    pub fn name(&self) -> &'static str {
        match self {
            Keyword::Function => "function",
            Keyword::Loop => "loop",
            Keyword::LoopOr => "loop_or",
            Keyword::Times => "times",
            Keyword::Replace => "replace",
            Keyword::Branch => "branch",
        }
    }
}
mod keywords {
    use super::Keyword::*;
    pub const KEYWORDS: &[super::Keyword] = &[Function, Loop, LoopOr, Times, Replace, Branch];
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
        } else if let Some(token) = parse_grouping(letter) {
            tokens.push(token);
            bytes.next();
        } else if let Some(mut multichar_tokens) = consume_multichar_tokens(letter, &mut bytes) {
            tokens.append(&mut multichar_tokens);
        } else if let Some(operator) = parse_operator(letter) {
            tokens.push(Token::Operator(operator));
            bytes.next();
        } else if let Some(letter) = parse_letter_start(letter) {
            let name = consume_identifier(letter, &mut bytes)?;
            tokens.push(name);
        } else if let Some(string) = consume_string(letter, &mut bytes)? {
            tokens.push(Token::String(string));
            bytes.next();
        } else if let Some(char) = consume_char(letter, &mut bytes)? {
            tokens.push(Token::Number(char as i64));
            bytes.next();
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

pub fn parse_letter_start(letter: u8) -> Option<u8> {
    if letter >= b'a' && letter <= b'z'
        || letter >= b'A' && letter <= b'Z'
        || [b'_'].contains(&letter)
    {
        return Some(letter);
    } else {
        None
    }
}
pub fn parse_letter(letter: u8) -> Option<u8> {
    parse_letter_start(letter).or_else(|| {
        if [b'/'].contains(&letter) {
            Some(letter)
        } else {
            None
        }
    })
}
pub fn parse_alphanum(letter: u8) -> Option<u8> {
    parse_letter(letter).or_else(|| if is_digit(letter) { Some(letter) } else { None })
}

pub fn consume_multichar_tokens(letter: u8, iter: &mut Peekable<Bytes>) -> Option<Tokens> {
    use Comparison::*;
    use Operator::Comparison as OpComp;
    match letter {
        b'/' => {
            iter.next();
            // if_next_or(b'/', Operator::Concatenate, Operator::Add, iter)
            let next_letter = iter.peek();
            match next_letter {
                Some(b'/') => {
                    ignore_until_not_including(b'\n', iter);
                    iter.next();
                    Some(Vec::new())
                }
                Some(_) => None,
                None => None,
            }
        }
        b'+' => {
            iter.next();
            if_next_or(b'+', Operator::Concatenate, Operator::Add, iter)
        }
        b'=' => {
            iter.next();
            if_nexts_or(
                &[(b'?', OpComp(Equals)), (b'>', Operator::Overwrite)],
                Operator::Assignment,
                iter,
            )
        }
        b'<' => {
            iter.next();
            if_next_or(b'=', OpComp(LessThanEquals), OpComp(LessThan), iter)
        }
        b'>' => {
            iter.next();
            if_next_or(b'=', OpComp(GreaterThanEquals), OpComp(GreaterThan), iter)
        }
        b'|' => {
            iter.next();
            if_nexts_or(
                &[(b'*', Operator::Multiply), (b'/', Operator::Divide)],
                Operator::Call,
                iter,
            )
        }
        _ => None,
    }
}

fn if_nexts_or(
    nexts: &[(u8, Operator)],
    or: Operator,
    iter: &mut Peekable<Bytes>,
) -> Option<Tokens> {
    for (next, then) in nexts {
        if let Some(next_letter) = iter.peek() {
            if *next_letter == *next {
                iter.next();
                return Some(vec![Token::Operator(*then)]);
            }
        }
    }
    Some(vec![Token::Operator(or)])
}

fn if_next_or(
    next: u8,
    then: Operator,
    or: Operator,
    iter: &mut Peekable<Bytes>,
) -> Option<Tokens> {
    if let Some(next_letter) = iter.peek() {
        if *next_letter == next {
            iter.next();
            return Some(vec![Token::Operator(then)]);
        }
    }
    Some(vec![Token::Operator(or)])
}

pub fn ignore_until_not_including(end_letter: u8, iter: &mut Peekable<Bytes>) {
    loop {
        match iter.peek() {
            Some(letter) if *letter == end_letter => return,
            None => return,
            Some(_) => {
                iter.next();
            }
        }
    }
}

pub fn consume_escaped_until_not_including(
    end_letter: u8,
    iter: &mut Peekable<Bytes>,
) -> Result<Vec<u8>, AnyError> {
    let mut consumed = Vec::new();
    loop {
        match iter.peek() {
            Some(letter) => {
                if *letter == end_letter {
                    return Ok(consumed);
                } else if *letter == b'\\' {
                    iter.next();
                    match iter.peek() {
                        Some(b'"') => consumed.push(b'"'),
                        Some(b'n') => consumed.push(b'\n'),
                        Some(b'0') => consumed.push(b'\0'),
                        Some(b'\\') => consumed.push(b'\\'),
                        None => return Err("Incomplete escaped character")?,
                        Some(other) => {
                            return Err(format!(
                                "Unknown escaped character with code {} ({})",
                                *other, *other as char
                            )
                            .into())
                        }
                    }
                    iter.next();
                } else {
                    consumed.push(*letter);
                    iter.next();
                }
            }
            None => return Ok(consumed),
        }
    }
}

pub fn consume_string(quote: u8, iter: &mut Peekable<Bytes>) -> Result<Option<Vec<u8>>, AnyError> {
    if quote == b'"' {
        iter.next();
        let inner_string = consume_escaped_until_not_including(b'"', iter)?;
        if let Some(b'"') = iter.peek() {
            Ok(Some(inner_string))
        } else {
            Err("Unclosed double quote".into())
        }
    } else {
        Ok(None)
    }
}

pub fn consume_char(quote: u8, iter: &mut Peekable<Bytes>) -> Result<Option<u8>, AnyError> {
    if quote == b'\'' {
        iter.next();
        let letter = iter.peek();
        let character = match letter {
            Some(b'\\') => {
                iter.next();
                match iter.peek() {
                    Some(b'\'') => Some(b'\''),
                    Some(b'n') => Some(b'\n'),
                    Some(b'0') => Some(b'\0'),
                    Some(b'\\') => Some(b'\\'),
                    None => return Err("Incomplete escaped character")?,
                    Some(other) => {
                        return Err(format!(
                            "Unknown escaped character with code {} ({})",
                            *other, *other as char
                        )
                        .into());
                    }
                }
            }
            Some(regular_letter) => Some(*regular_letter),
            None => return Err("Unclosed single quote".into()),
        };
        iter.next();
        if let Some(b'\'') = iter.peek() {
            Ok(character)
        } else {
            Err("Unclosed single quote".into())
        }
    } else {
        Ok(None)
    }
}

pub fn parse_operator(letter: u8) -> Option<Operator> {
    match letter {
        b'-' => Some(Operator::Substract),
        b';' => Some(Operator::Ignore),
        b'#' => Some(Operator::Get),
        b':' => Some(Operator::Type),
        b'%' => Some(Operator::Modulo),
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

pub fn consume_identifier(first_letter: u8, iter: &mut Peekable<Bytes>) -> Result<Token, AnyError> {
    let mut accumulated = vec![first_letter];
    loop {
        iter.next();
        if let Some(letter) = iter.peek() {
            if let Some(new_letter) = parse_alphanum(*letter) {
                accumulated.push(new_letter);
                continue;
            }
        }
        return Ok(keyword_or_identifier(String::from_utf8(accumulated)?));
    }
}

pub fn keyword_or_identifier(identifier: String) -> Token {
    for k in keywords::KEYWORDS {
        if k.name() == identifier {
            return Token::Keyword(k.clone());
        }
    }
    Token::Identifier(identifier)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::lexer::Operator::{Add, Divide, Modulo, Multiply, Substract};
    use Token::{CloseBracket, Identifier, Number, OpenBracket, Operator};

    #[test]
    fn test_unkown_char() {
        lex("@").expect_err("should have failed");
    }
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
    fn test_arithmetic() {
        assert_eq!(
            lex("5 +7 |*12 |/ident -4 %2").unwrap(),
            vec![
                Number(5),
                Operator(Add),
                Number(7),
                Operator(Multiply),
                Number(12),
                Operator(Divide),
                Identifier("ident".to_string()),
                Operator(Substract),
                Number(4),
                Operator(Modulo),
                Number(2),
            ]
        );
    }

    #[test]
    fn test_identifier() {
        assert_eq!(
            lex("5as/df12+34").unwrap(),
            vec![
                Number(5),
                Identifier("as/df12".to_string()),
                Operator(Add),
                Number(34)
            ]
        );
        lex("/asdf").expect_err("should have failed");
    }
    #[test]
    fn test_list() {
        assert_eq!(
            lex("[5 6 7]").unwrap(),
            vec![OpenBracket, Number(5), Number(6), Number(7), CloseBracket,]
        );
    }

    #[test]
    fn test_comments() {
        assert_eq!(lex("5 // comment \n6").unwrap(), vec![Number(5), Number(6)]);
        lex("/").expect_err("should have failed");
        lex("6/3").expect_err("should have failed");
    }

    #[test]
    fn test_strings() {
        assert_eq!(
            lex("\"abc\"").unwrap(),
            vec![Token::String(vec![b'a', b'b', b'c'])]
        );
    }
    #[test]
    fn test_escaped_strings() {
        assert_eq!(
            lex(r#""\"\\\n\0""#).unwrap(),
            vec![Token::String(vec![b'"', b'\\', b'\n', 0])]
        );
        lex(r#""\"#).expect_err("unclosed string with escaped quote should fail");
        lex(r#""\a""#).expect_err("unknown escaped char should fail");
    }
    #[test]
    fn test_incomplete_string() {
        lex(r#"""#).expect_err("should have failed");
    }
    #[test]
    fn test_char() {
        assert_eq!(lex("'b'").unwrap(), vec![Token::Number('b' as i64)]);
        assert_eq!(
            lex(r#"'"' '\'' '\\' '\n' '\0'"#).unwrap(),
            vec![
                Token::Number('"' as i64),
                Token::Number('\'' as i64),
                Token::Number('\\' as i64),
                Token::Number('\n' as i64),
                Token::Number('\0' as i64),
            ]
        );

        lex(r"'\a'").expect_err("unknown escaped char should fail");
        lex(r"'\'").expect_err("unfinished escaped char should fail");
        lex(r"'\").expect_err("unclosed char should fail");
        lex(r"'").expect_err("unclosed char should fail");
    }
}
