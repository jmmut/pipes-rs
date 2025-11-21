use crate::expression::Expression;
use pipes_rs::common::{err_span, AnyError};
use pipes_rs::frontend::sources::lexer::{lex, TokenizedSource};
use pipes_rs::frontend::sources::location::SourceCode;
use pipes_rs::frontend::sources::token::{LocatedTokens, Token};

pub fn frontend(line: &str) -> Result<Expression, AnyError> {
    let tokenized_source = lex(line)?;
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
        Ok(Expression {
            elements: Vec::new(),
        })
    } else {
        let expression = parse_tokens(&source_code, &tokens, &mut index)?;
        Ok(expression)
    }
}

fn parse_tokens(
    source: &SourceCode,
    tokens: &LocatedTokens,
    index: &mut usize,
) -> Result<Expression, AnyError> {
    expect(Token::OpenParenthesis, source, tokens, index)?;
    expect(Token::CloseParenthesis, source, tokens, index)?;
    Ok(Expression {
        elements: Vec::new(),
    })
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
mod tests {
    use super::*;
    use pipes_rs::common::unwrap_display;

    fn l(elements: Vec<Expression>) -> Expression {
        Expression { elements }
    }
    fn e() -> Vec<Expression> {
        Vec::new()
    }
    #[test]
    fn empty() {
        let expr = unwrap_display(frontend(""));
        assert_eq!(expr, l(e()));
    }
    #[test]
    fn empty_parenthesis() {
        let expr = unwrap_display(frontend("()"));
        assert_eq!(expr, l(e()));
    }
}
