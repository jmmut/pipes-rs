use crate::expression::{Expression, FALSE_STR, TRUE_STR};
use pipes_rs::common::{err_span, AnyError};
use pipes_rs::frontend::parser::reverse_iterative_parser::err_expected_span;
use pipes_rs::frontend::sources::lexer::{lex_with_eof, TokenizedSource};
use pipes_rs::frontend::sources::location::SourceCode;
use pipes_rs::frontend::sources::token::{LocatedToken, LocatedTokens, Token};

pub fn frontend(line: &str) -> Result<Expression, AnyError> {
    let tokenized_source = lex_with_eof(line)?;
    let expression = parse(tokenized_source)?;
    Ok(expression)
}

fn parse(tokenized_source: TokenizedSource) -> Result<Expression, AnyError> {
    let TokenizedSource {
        tokens,
        source_code,
    } = tokenized_source;
    let mut index: usize = 0;
    if tokens.len() == 0 {
        Ok(Expression::List(Vec::new()))
    } else {
        let expression = parse_tokens(&source_code, &tokens, &mut index)?;
        expect(Token::EndOfFile, &source_code, &tokens, &mut index)?;
        Ok(expression)
    }
}

fn parse_tokens(
    source: &SourceCode,
    tokens: &LocatedTokens,
    index: &mut usize,
) -> Result<Expression, AnyError> {
    let token = &tokens[*index];
    if let Token::Number(number) = token.token {
        *index += 1;
        Ok(Expression::Number(number))
    } else if token.token == Token::OpenParenthesis {
        let mut elements = Vec::new();
        *index += 1;
        loop {
            let LocatedToken { token, span } = &tokens[*index];
            if *token == Token::CloseParenthesis {
                *index += 1;
                return Ok(Expression::List(elements));
            } else if *token == Token::EndOfFile {
                return err_expected_span(
                    "another element or closing parenthesis",
                    token,
                    source,
                    *span,
                );
            } else {
                let element = parse_tokens(source, tokens, index)?;
                elements.push(element);
            }
        }
    } else if let Token::Identifier(name) = &token.token {
        *index += 1;
        if name == TRUE_STR {
            Ok(Expression::Bool(true))
        } else if name == FALSE_STR {
            Ok(Expression::Bool(false))
        } else {
            Ok(Expression::Symbol(name.to_string()))
        }
    } else if let Token::Keyword(name) = &token.token {
        *index += 1;
        Ok(Expression::Symbol(name.name().to_string()))
    } else if let Token::Operator(name) = &token.token {
        *index += 1;
        Ok(Expression::Symbol(name.to_string()))
    } else {
        return err_expected_span("an atom or list", &token.token, source, token.span);
    }
}

fn expect(
    expected: Token,
    source: &SourceCode,
    tokens: &LocatedTokens,
    index: &mut usize,
) -> Result<(), AnyError> {
    let token = &tokens[*index];
    if token.token == expected {
        *index += 1;
        Ok(())
    } else {
        err_span(
            format!("expected {}, but got {}", expected, token.token),
            source,
            token.span,
        )
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use pipes_rs::common::unwrap_display;

    pub fn l(elements: Vec<Expression>) -> Expression {
        Expression::List(elements)
    }
    pub fn e() -> Vec<Expression> {
        Vec::new()
    }
    pub fn n(number: i64) -> Expression {
        Expression::Number(number)
    }
    pub fn s(symbol: &str) -> Expression {
        Expression::Symbol(symbol.to_string())
    }
    #[test]
    fn empty() {
        frontend("").expect_err("should fail");
        // let expr = unwrap_display(frontend(""));
        // assert_eq!(expr, l(e()));
    }
    #[test]
    fn empty_parenthesis() {
        let expr = unwrap_display(frontend("()"));
        assert_eq!(expr, l(e()));
    }
    #[test]
    fn basic_number() {
        let expr = unwrap_display(frontend("5"));
        assert_eq!(expr, n(5));
    }
    #[test]
    fn incomplete() {
        frontend("(5").expect_err("should fail");
    }
    #[test]
    fn several_numbers() {
        let expr = unwrap_display(frontend("(5 6 7)"));
        assert_eq!(expr, l(vec![n(5), n(6), n(7)]));
    }
    #[test]
    fn nested() {
        let expr = unwrap_display(frontend("(() (6 (7)) 8)"));
        assert_eq!(expr, l(vec![l(e()), l(vec![n(6), l(vec![n(7)])]), n(8)]));
    }

    #[test]
    fn symbol() {
        let expr = unwrap_display(frontend("(+ (6 (a)) 8)"));
        assert_eq!(expr, l(vec![s("+"), l(vec![n(6), l(vec![s("a")])]), n(8)]));
    }
}
