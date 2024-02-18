use crate::common::{context, err, err_span, AnyError};
use crate::frontend::expression::{Expression, ExpressionSpan};
use crate::frontend::lexer::TokenizedSource;
use crate::frontend::location::{SourceCode, Span};
use crate::frontend::program::Program;
use crate::frontend::token::{LocatedToken, LocatedTokens, Token};

pub fn parse_tokens(tokens: TokenizedSource) -> Result<Program, AnyError> {
    context("Multiparameter parser", Parser::parse_tokens(tokens))
}

struct Parser;

enum PartialExpression {
    Token(LocatedToken),
    Expression(ExpressionSpan),
}
type PartialExpressions = Vec<PartialExpression>;

impl Parser {
    fn parse_tokens(
        TokenizedSource {
            tokens,
            source_code,
        }: TokenizedSource,
    ) -> Result<Program, AnyError> {
        let grouped = Self::group_tokens(tokens, &source_code)?;
        Self::finish_construction(grouped, &source_code)
    }
    fn group_tokens(
        tokens: LocatedTokens,
        source_code: &SourceCode,
    ) -> Result<PartialExpressions, AnyError> {
        let mut pes = PartialExpressions::new();
        for LocatedToken { token, span } in tokens.into_iter().rev() {
            match token {
                Token::Number(n) => push_e(&mut pes, Expression::Value(n), span),
                Token::Operator(_)
                | Token::Identifier(_)
                | Token::String(_)
                | Token::Keyword(_)
                | Token::OpenBracket
                | Token::CloseBracket
                | Token::OpenBrace
                | Token::CloseBrace
                | Token::OpenParenthesis
                | Token::CloseParenthesis => push_t(&mut pes, token, span),
            }
        }
        Ok(pes)
    }
    fn finish_construction(
        mut partial_expressions: PartialExpressions,
        source: &SourceCode,
    ) -> Result<Program, AnyError> {
        if partial_expressions.len() == 0 {
            Ok(Program::new_raw(Expression::Nothing))
        } else if partial_expressions.len() == 1 {
            match partial_expressions.pop().unwrap() {
                PartialExpression::Token(t) => err_span(
                    "A top level file can not just be this token",
                    source,
                    t.span,
                ),
                PartialExpression::Expression(e) => Ok(Program::new(e)),
            }
        } else {
            err("unimplemented")
        }
    }
}

fn push_e(pes: &mut PartialExpressions, syntactic_type: Expression, span: Span) {
    pes.push(PartialExpression::Expression(ExpressionSpan {
        syntactic_type,
        span,
    }))
}
fn push_t(pes: &mut PartialExpressions, token: Token, span: Span) {
    pes.push(PartialExpression::Token(LocatedToken { token, span }))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::AnyError;
    use crate::frontend::expression::{Expression, ExpressionSpan};
    use crate::frontend::lexer::lex;
    use crate::frontend::program::Program;

    fn lex_and_parse(code: &str) -> Result<Program, AnyError> {
        let tokens = lex(code)?;
        let program = parse_tokens(tokens)?;
        Ok(program)
    }
    fn mock_program(expression: Expression) -> Program {
        Program::new(ExpressionSpan::new_spanless(expression))
    }
    #[test]
    fn test_basic() {
        let program = lex_and_parse("").unwrap();
        assert_eq!(program, mock_program(Expression::Nothing))
    }
    #[test]
    fn test_number() {
        let program = lex_and_parse("5").unwrap();
        assert_eq!(program, mock_program(Expression::Value(5)))
    }
    #[test]
    fn test_fails_on_incomplete_token() {
        lex_and_parse(":").expect_err("should fail");
    }
}
