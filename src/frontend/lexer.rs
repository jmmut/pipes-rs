use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::common::{context, err, err_loc, err_since, err_span};
use crate::frontend::location::{Location, SourceCode, Span};
use crate::AnyError;

#[derive(Clone, PartialEq, Debug)]
pub struct LocatedToken {
    pub token: Token,
    pub span: Span,
}
pub type LocatedTokens = Vec<LocatedToken>;

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

#[derive(Copy, Clone, PartialEq, Debug, EnumIter)]
pub enum Keyword {
    Function,
    Loop,
    LoopOr,
    Times,
    TimesOr,
    Replace,
    Map,
    Branch,
    Something,
    Inspect,
    Public,
    Cast,
}
impl Keyword {
    pub fn name(&self) -> &'static str {
        match self {
            Keyword::Function => "function",
            Keyword::Loop => "loop",
            Keyword::LoopOr => "loop_or",
            Keyword::Times => "times",
            Keyword::TimesOr => "times_or",
            Keyword::Replace => "replace",
            Keyword::Map => "map",
            Keyword::Branch => "branch",
            Keyword::Something => "something",
            Keyword::Inspect => "inspect",
            Keyword::Public => "public",
            Keyword::Cast => "cast",
        }
    }
}

#[derive(Debug, Clone)]
pub struct TokenizedSource {
    pub tokens: Vec<Token>,
    // pub tokens: Vec<LocatedToken>,
    pub source_code: SourceCode,
}
pub type Tokens = Vec<Token>;

pub fn lex<S: Into<SourceCode>>(code: S) -> Result<TokenizedSource, AnyError> {
    context("Lexer", try_lex(code.into()))
}
fn try_lex(mut code: SourceCode) -> Result<TokenizedSource, AnyError> {
    let mut tokens = Vec::<LocatedToken>::new();
    let mut previous_location = code.get_location();
    while !code.consumed() {
        let current_letter = code.peek().unwrap();
        if let Some(token) = try_consume_number(&mut code)? {
            tokens.push(token);
        } else if let Some(token) = try_consume_grouping(&mut code) {
            tokens.push(token);
        } else if let Some(mut multichars) = try_consume_multichar_tokens(&mut code) {
            tokens.append(&mut multichars);
        } else if let Some(token) = try_consume_operator(&mut code) {
            tokens.push(token);
        } else if let Some(token) = try_consume_identifier(&mut code) {
            tokens.push(token?);
        } else if let Some(token) = try_consume_string(&mut code) {
            tokens.push(token?);
        } else if let Some(token) = try_consume_char(&mut code) {
            tokens.push(token?);
        } else if try_consume_space(&mut code) {
        } else {
            return err_loc(
                format!(
                    "unsupported expression starting with byte {} ('{}')",
                    current_letter, current_letter as char,
                ),
                &code,
            );
        };
        if previous_location == code.get_location() {
            panic!(
                "infinite loop! when parsing{}",
                code.format_current_location()
            );
        }
        previous_location = code.get_location();
    }

    // if let Some(token) = tokens.last() {
    //     println!("debugging: {}", code.format_span(token.span));
    // }
    Ok(TokenizedSource {
        tokens: tokens.into_iter().map(|t| t.token).collect(),
        source_code: code,
    })
}

pub fn try_consume_number(code: &mut SourceCode) -> Result<Option<LocatedToken>, AnyError> {
    let initial_location = code.get_location();
    if let Some(digits) = code.consume_if(|letter, _| parse_digit(letter)) {
        let mut accumulated = 0;
        for digit in digits {
            accumulated = maybe_add_digit(accumulated, digit, code)?;
        }
        let token = code.span_token(Token::Number(accumulated), initial_location);
        Ok(Some(token))
    } else {
        Ok(None)
    }
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

fn maybe_add_digit(
    mut accumulated: i64,
    new_digit: i64,
    code: &mut SourceCode,
) -> Result<i64, AnyError> {
    const OVERFLOW_MESSAGE: &'static str = "Can't fit number in 64 bits";
    return if i64::MAX / 10 < accumulated.abs() {
        err_loc(OVERFLOW_MESSAGE, code)?
    } else {
        accumulated *= 10;
        if i64::MAX - new_digit < accumulated {
            err_loc(OVERFLOW_MESSAGE, code)?
        } else {
            Ok(accumulated + new_digit)
        }
    };
}

fn try_consume_grouping(code: &mut SourceCode) -> Option<LocatedToken> {
    let token = parse_grouping(code.peek()?)?;
    let located_token = code.located_token(token);
    code.next();
    Some(located_token)
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

fn try_consume_multichar_tokens(code: &mut SourceCode) -> Option<LocatedTokens> {
    let initial_location = code.get_location();
    if code.consume("//") {
        code.consume_while(|letter, _| letter != b'\n');
        code.next();
        Some(Vec::new())
    } else {
        let operators = &[
            ("++", Operator::Concatenate),
            ("=?", Operator::Comparison(Comparison::Equals)),
            ("=>", Operator::Overwrite),
            ("<=", Operator::Comparison(Comparison::LessThanEquals)),
            (">=", Operator::Comparison(Comparison::GreaterThanEquals)),
            ("|*", Operator::Multiply),
            ("|/", Operator::Divide),
        ];
        for (text, operator) in *operators {
            if code.consume(text) {
                let token = Token::Operator(operator);
                let located_token = code.span_token(token, initial_location);
                return Some(vec![located_token]);
            }
        }
        None
    }
}

fn try_consume_operator(code: &mut SourceCode) -> Option<LocatedToken> {
    let operator = parse_operator(code.peek()?)?;
    let token = code.located_token(Token::Operator(operator));
    code.next();
    Some(token)
}

pub fn parse_operator(letter: u8) -> Option<Operator> {
    match letter {
        b'+' => Some(Operator::Add),
        b'-' => Some(Operator::Substract),
        b'%' => Some(Operator::Modulo),
        b';' => Some(Operator::Ignore),
        b'|' => Some(Operator::Call),
        b'#' => Some(Operator::Get),
        b':' => Some(Operator::Type),
        b'=' => Some(Operator::Assignment),
        b'<' => Some(Operator::Comparison(Comparison::LessThan)),
        b'>' => Some(Operator::Comparison(Comparison::GreaterThan)),
        _ => None,
    }
}

fn try_consume_identifier(code: &mut SourceCode) -> Option<Result<LocatedToken, AnyError>> {
    let initial_location = code.get_location();
    let letters = code.consume_if(|letter, i| {
        if i == 0 {
            parse_letter_start(letter)
        } else {
            parse_alphanum(letter)
        }
    })?;
    let name = String::from_utf8(letters).map_err(|e| e.into());
    let token = name.map(|name| {
        let token = keyword_or_identifier(name);
        let located_token = code.span_token(token, initial_location);
        located_token
    });
    Some(token.into())
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

pub fn parse_alphanum(letter: u8) -> Option<u8> {
    parse_letter(letter).or_else(|| if is_digit(letter) { Some(letter) } else { None })
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

pub fn keyword_or_identifier(identifier: String) -> Token {
    for k in Keyword::iter() {
        if k.name() == identifier {
            return Token::Keyword(k.clone());
        }
    }
    Token::Identifier(identifier)
}

pub fn try_consume_string(iter: &mut SourceCode) -> Option<Result<LocatedToken, AnyError>> {
    let quote = iter.peek()?;
    let initial_location = iter.get_location();
    if quote == b'"' {
        iter.next();
        let inner_string = consume_escaped_until_not_including(b'"', iter);
        match inner_string {
            Ok(inner_string) => {
                if let Some(b'"') = iter.peek() {
                    let token = iter.span_token(Token::String(inner_string), initial_location);
                    iter.next();
                    Some(Ok(token))
                } else {
                    Some(err_span(
                        "Unclosed double quote",
                        iter,
                        iter.span_since(initial_location),
                    ))
                }
            }
            Err(e) => Some(Err(e)),
        }
    } else {
        None
    }
}

pub fn consume_escaped_until_not_including(
    end_letter: u8,
    iter: &mut SourceCode,
) -> Result<Vec<u8>, AnyError> {
    let mut consumed = Vec::new();
    loop {
        let inside = iter.get_location();
        match iter.peek() {
            Some(letter) => {
                if letter == end_letter {
                    return Ok(consumed);
                } else if letter == b'\\' {
                    iter.next();
                    match iter.peek() {
                        Some(b'"') => consumed.push(b'"'),
                        Some(b'n') => consumed.push(b'\n'),
                        Some(b'0') => consumed.push(b'\0'),
                        // Some(b'x') => // TODO: parse hexadecimal
                        Some(b'\\') => consumed.push(b'\\'),
                        None => return err_since("Incomplete escaped character", iter, inside)?,
                        Some(other) => {
                            return err_since(
                                format!(
                                    "Unknown escaped character with code {} ({})",
                                    other, other as char
                                ),
                                iter,
                                inside,
                            )
                        }
                    }
                    iter.next();
                } else {
                    consumed.push(letter);
                    iter.next();
                }
            }
            None => return Ok(consumed),
        }
    }
}

fn try_consume_char(code: &mut SourceCode) -> Option<Result<LocatedToken, AnyError>> {
    let quote = code.peek()?;
    let initial_location = code.get_location();
    Some(match consume_char(quote, code) {
        Ok(maybe_char) => {
            let token = Token::Number(maybe_char? as i64);
            let loc_token = code.span_token(token, initial_location);
            code.next();
            Ok(loc_token)
        }
        Err(e) => Err(e),
    })
}
pub fn consume_char(quote: u8, iter: &mut SourceCode) -> Result<Option<u8>, AnyError> {
    let start = iter.get_location();
    if quote == b'\'' {
        iter.next();
        let inside = iter.get_location();
        let letter = iter.peek();
        let character = match letter {
            Some(b'\\') => {
                iter.next();
                match iter.peek() {
                    Some(b'\'') => Some(b'\''),
                    Some(b'n') => Some(b'\n'),
                    Some(b'0') => Some(b'\0'),
                    Some(b'\\') => Some(b'\\'),
                    None => return err_since("Incomplete escaped character", iter, inside)?,
                    Some(other) => {
                        return err_since(
                            format!(
                                "Unknown escaped character with code {} ({})",
                                other, other as char
                            ),
                            iter,
                            inside,
                        );
                    }
                }
            }
            Some(regular_letter) => Some(regular_letter),
            None => return err_since("Unclosed single quote", iter, start),
        };
        iter.next();
        if let Some(b'\'') = iter.peek() {
            Ok(character)
        } else {
            err_since("Unclosed single quote", iter, start)
        }
    } else {
        Ok(None)
    }
}

fn try_consume_space(code: &mut SourceCode) -> bool {
    code.consume_while(|letter, _| is_space(letter))
}

fn is_space(letter: u8) -> bool {
    [b' ', b'\n', b'\t', b'\r'].contains(&letter)
}

#[cfg(test)]
mod tests {
    use crate::common::unwrap_display;
    use Token::{CloseBracket, Identifier, Number, OpenBracket, Operator};

    use crate::frontend::lexer::Operator::{Add, Divide, Modulo, Multiply, Substract};

    use super::*;

    fn get_tokens(text: &str) -> Tokens {
        unwrap_display(lex(text)).tokens
    }
    #[test]
    fn test_unkown_char() {
        lex("@").expect_err("should have failed");
    }
    #[test]
    fn test_overflow_positive() {
        lex("922337203685477580").unwrap();
        lex("9223372036854775808").expect_err("should have failed");
    }

    #[test]
    fn test_several_digits() {
        assert_eq!(get_tokens("57"), vec![Number(57)])
    }
    #[test]
    fn test_operator() {
        assert_eq!(get_tokens("+"), vec![Operator(Add)])
    }

    #[test]
    fn test_adding_numbers() {
        assert_eq!(
            get_tokens("5+7+12+34"),
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
            get_tokens("5 +7 |*12 |/ident -4 %2"),
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
            get_tokens("5as/df12+34"),
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
            get_tokens("[5 6 7]"),
            vec![OpenBracket, Number(5), Number(6), Number(7), CloseBracket,]
        );
    }

    #[test]
    fn test_comments() {
        assert_eq!(get_tokens("5 // comment \n6"), vec![Number(5), Number(6)]);
        lex("/").expect_err("should have failed");
        lex("6/3").expect_err("should have failed");
    }

    #[test]
    fn test_strings() {
        assert_eq!(
            get_tokens("\"abc\""),
            vec![Token::String(vec![b'a', b'b', b'c'])]
        );
    }
    #[test]
    fn test_escaped_strings() {
        assert_eq!(
            get_tokens(r#""\"\\\n\0""#),
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
        assert_eq!(get_tokens("'b'"), vec![Token::Number('b' as i64)]);
        assert_eq!(
            get_tokens(r#"'"' '\'' '\\' '\n' '\0'"#),
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
