use crate::common::{context, err, err_span, AnyError};
use crate::frontend::ast::expected;
use crate::frontend::expression::{Chain, Expression, ExpressionSpan, Operation};
use crate::frontend::lexer::TokenizedSource;
use crate::frontend::location::{SourceCode, Span, NO_SPAN};
use crate::frontend::program::Program;
use crate::frontend::token::{LocatedToken, LocatedTokens, Operator, OperatorSpan, Token};

pub fn parse_tokens(tokens: TokenizedSource) -> Result<Program, AnyError> {
    context("Multiparameter parser", Parser::parse_tokens(tokens))
}

struct Parser;

#[derive(Debug)]
enum PartialExpression {
    Token(LocatedToken),
    Expression(ExpressionSpan),
    Operation(Operation),
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
                Token::OpenBrace => construct_chain(&mut pes, span)?,
                Token::Operator(operator) => construct_operation(&mut pes, operator, span)?,
                Token::Identifier(name) => push_e(&mut pes, Expression::Identifier(name), span),
                Token::String(_)
                | Token::Keyword(_)
                | Token::OpenBracket
                | Token::CloseBracket
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
            return Ok(Program::new_raw(Expression::Nothing));
        } else if partial_expressions.len() > 1 {
            partial_expressions.insert(
                0,
                PartialExpression::Token(LocatedToken::spanless(Token::CloseBrace)),
            );
            construct_chain(&mut partial_expressions, NO_SPAN)?;
        }
        if partial_expressions.len() == 1 {
            match partial_expressions.pop().unwrap() {
                PartialExpression::Token(t) => err_span(
                    "A top level file can not just be this token",
                    source,
                    t.span,
                ),
                PartialExpression::Operation(o) => err_span(
                    "A top level file can not just be an operation",
                    source,
                    o.operator.span,
                ),
                PartialExpression::Expression(e) => Ok(Program::new(e)),
            }
        } else {
            err(expected("chain or expression", partial_expressions))
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

fn construct_chain(pes: &mut PartialExpressions, open_brace_span: Span) -> Result<(), AnyError> {
    let elem = pes.pop();
    if let Some(PartialExpression::Expression(initial)) = elem {
        let mut elem = pes.pop();
        let mut operations = Vec::new();
        while let Some(PartialExpression::Operation(operation)) = elem {
            operations.push(operation);
            elem = pes.pop();
        }
        if let Some(PartialExpression::Token(LocatedToken {
            token: Token::CloseBrace,
            span: close_span,
        })) = elem
        {
            push_e(
                pes,
                Expression::Chain(Chain{initial: Some(Box::new(initial)), operations}),
                open_brace_span.merge(&close_span),
            );
            Ok(())
        } else {
            err(expected("operator or closing brace '}'", elem))
        }
    } else if let Some(PartialExpression::Token(LocatedToken {
        token: Token::CloseBrace,
        span: close_span,
    })) = elem
    {
        push_e(
            pes,
            Expression::empty_chain(),
            open_brace_span.merge(&close_span),
        );
        Ok(())
    } else {
        err(expected("expression or closing brace '}'", elem))
    }
}

fn construct_operation(
    pes: &mut PartialExpressions,
    operator: Operator,
    op_span: Span,
) -> Result<(), AnyError> {
    let mut elem = pes.pop();
    let mut operands = Vec::new();
    while let Some(PartialExpression::Expression(expr)) = elem {
        elem = pes.pop();
        operands.push(expr);
    }
    if let Some(elem) = elem {
        pes.push(elem);
    }
    pes.push(PartialExpression::Operation(Operation {
        operator: OperatorSpan {
            operator,
            span: op_span,
        },
        operands,
    }));
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::{unwrap_display, AnyError};
    use crate::frontend::expression::{Chain, Expression, ExpressionSpan};
    use crate::frontend::lexer::lex;
    use crate::frontend::program::Program;
    use crate::frontend::token::{Operator, OperatorSpan};

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
    #[test]
    fn test_extra_brace() {
        lex_and_parse("5}").expect_err("should fail");
        lex_and_parse("{5}}").expect_err("should fail");
        lex_and_parse("{5").expect_err("should fail");
        lex_and_parse("{{5}").expect_err("should fail");
    }

    #[test]
    fn test_correct_braces() {
        lex_and_parse("5").expect("should work");
        lex_and_parse("{5}").expect("should work");
        lex_and_parse("{{5}}").expect("should work");
    }

    #[test]
    fn test_operators() {
        let program = unwrap_display(lex_and_parse("3 |func 4 5"));
        assert_eq!(
            program,
            mock_program(Expression::Chain(Chain {
                initial: Some(Box::new(ExpressionSpan::new_spanless(Expression::Value(3)))),
                operations: vec![Operation {
                    operator: OperatorSpan::spanless(Operator::Call),
                    operands: vec![
                        ExpressionSpan::new_spanless(Expression::Identifier("func".to_string())),
                        ExpressionSpan::new_spanless(Expression::Value(4)),
                        ExpressionSpan::new_spanless(Expression::Value(5)),
                    ],
                }],
            }))
        );
        // "{3 [|[func 4 5]]}"
    }
}
