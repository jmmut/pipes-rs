use crate::common::AnyError;
use crate::frontend::expression::{Expression, StaticList, Transformation};
use crate::frontend::iterative_parser::error_expected;
use crate::frontend::lexer::{lex, Operator, Token, Tokens};
use std::collections::VecDeque;
use std::fmt::Debug;

#[derive(Debug)]
enum PartialExpression {
    OpenBracket,
    Expression(Expression),
    Operation(Transformation),
    Operator(Operator),
}
pub fn ast_deserialize(s: &str) -> Result<Expression, AnyError> {
    let tokens = lex(s).unwrap();
    deserialize_tokens(tokens)
}
pub fn deserialize_tokens(tokens: Tokens) -> Result<Expression, AnyError> {
    let mut accumulated = Vec::new();
    for token in tokens {
        match token {
            Token::OpenBracket => accumulated.push(PartialExpression::OpenBracket),
            Token::CloseBracket => construct_list(&mut accumulated)?,
            Token::Operator(o) => accumulated.push(PartialExpression::Operator(o)),
            Token::Identifier(ident) => match ident.as_str() {
                "Chain" => construct_chain(&mut accumulated)?,
                "Op" => construct_operation(&mut accumulated)?,
                _ => accumulated.push(PartialExpression::Expression(Expression::Identifier(ident))),
            },
            Token::Number(n) => {
                accumulated.push(PartialExpression::Expression(Expression::Value(n)))
            }
            _ => error_expected("bracket, operator, identifier or value", token)?,
        };
    }
    finish_construction(&mut accumulated)
}

fn construct_chain(accumulated: &mut Vec<PartialExpression>) -> Result<(), AnyError> {
    let mut transformations = VecDeque::new();

    while let Some(pe) = accumulated.pop() {
        match pe {
            PartialExpression::Operation(t) => transformations.push_front(t),
            PartialExpression::Expression(e) => {
                accumulated.push(PartialExpression::Expression(Expression::Chain {
                    initial: Box::new(e),
                    transformations: transformations.into_iter().collect::<Vec<_>>(),
                }));
                return Ok(());
            }
            _ => return error_expected("expression or operation", pe),
        }
    }
    error_expected("expression", None::<()>)
}

fn construct_operation(accumulated: &mut Vec<PartialExpression>) -> Result<(), AnyError> {
    let elem = accumulated.pop();
    if let Some(PartialExpression::Expression(operand)) = elem {
        let elem = accumulated.pop();
        if let Some(PartialExpression::Operator(operator)) = elem {
            accumulated.push(PartialExpression::Operation(Transformation {
                operator,
                operand,
            }));
            Ok(())
        } else {
            error_expected("operator", elem)
        }
    } else {
        error_expected("operand", elem)
    }
}

fn construct_list(accumulated: &mut Vec<PartialExpression>) -> Result<(), AnyError> {
    let mut expressions = VecDeque::new();
    while let Some(pe) = accumulated.pop() {
        match pe {
            PartialExpression::OpenBracket => {
                accumulated.push(PartialExpression::Expression(Expression::StaticList(
                    StaticList {
                        elements: expressions.into_iter().collect::<Vec<_>>(),
                    },
                )));
                return Ok(());
            }
            PartialExpression::Expression(e) => expressions.push_front(e),
            _ => return error_expected("array start or expression", pe),
        }
    }
    error_expected("array start or expression", None::<()>)
}

fn finish_construction(accumulated: &mut Vec<PartialExpression>) -> Result<Expression, AnyError> {
    match accumulated.pop() {
        Some(PartialExpression::Expression(e)) => {
            if accumulated.is_empty() {
                return Ok(e);
            }
        }
        None => {}
        Some(a) => accumulated.push(a),
    }
    Err(format!("unfinished code: {:?}", accumulated).into())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::lex_and_parse;

    #[test]
    fn test_complex() {
        let ast = ast_deserialize("[5 +7 Op |parse_char Op Chain 8]").unwrap();
        let code = lex_and_parse("[ {5 +7 |parse_char}  8 ]").unwrap();
        assert_eq!(ast, code);
    }
}
