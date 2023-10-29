use crate::common::AnyError;
use crate::frontend::expression::{Expression, StaticList, Transformation, Type};
use crate::frontend::iterative_parser::error_expected;
use crate::frontend::lexer::{lex, Operator, Token, Tokens};
use std::collections::VecDeque;
use std::fmt::Debug;

#[derive(Debug)]
enum PartialExpression {
    OpenBracket,
    OpenParenthesis,
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
            Token::OpenParenthesis => accumulated.push(PartialExpression::OpenParenthesis),
            Token::CloseParenthesis => construct_complex_type(&mut accumulated)?,
            Token::Operator(o) => accumulated.push(PartialExpression::Operator(o)),
            Token::Identifier(ident) => match ident.as_str() {
                "Chain" => construct_chain(&mut accumulated)?,
                "Op" => construct_operation(&mut accumulated)?,
                // "Type" => construct_simple_type(&mut accumulated)?,
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
fn construct_simple_type(accumulated: &mut Vec<PartialExpression>) -> Result<(), AnyError> {
    let elem = accumulated.pop();
    if let Some(PartialExpression::Expression(Expression::Identifier(name))) = elem {
        let elem = accumulated.pop();
        if let Some(PartialExpression::Operator(Operator::Type)) = elem {
            accumulated.push(PartialExpression::Operation(Transformation {
                operator: Operator::Type,
                operand: Expression::Type(Type::simple(name)),
            }));
            Ok(())
        } else {
            error_expected("operator Type", elem)
        }
    } else {
        error_expected("operand identifier", elem)
    }
}

fn construct_complex_type(accumulated: &mut Vec<PartialExpression>) -> Result<(), AnyError> {
    let mut types = VecDeque::new();
    while let Some(expr_type) = accumulated.pop() {
        match expr_type {
            PartialExpression::Expression(Expression::Type(a_type)) => {
                types.push_front(a_type);
            }
            PartialExpression::Expression(Expression::Identifier(name)) => {
                types.push_front(Type::simple(name));
            }
            PartialExpression::OpenParenthesis => {
                break;
            }
            _ => return error_expected("type name or type expression", expr_type),
        }
    }
    let elem = accumulated.pop();
    if let Some(PartialExpression::Expression(Expression::Identifier(parent))) = elem {
        let a_type = Type::from(parent, types.into_iter().collect::<Vec<_>>());
        accumulated.push(PartialExpression::Expression(Expression::Type(a_type)));
        Ok(())
    } else {
        error_expected("array start or expression", elem)
    }
}

fn construct_list(accumulated: &mut Vec<PartialExpression>) -> Result<(), AnyError> {
    let mut expressions = VecDeque::new();
    let mut elem = accumulated.pop();
    while let Some(PartialExpression::Expression(e)) = elem {
        expressions.push_front(e);
        elem = accumulated.pop()
    }
    if let Some(PartialExpression::OpenBracket) = elem {
        accumulated.push(PartialExpression::Expression(Expression::StaticList(
            StaticList {
                elements: expressions.into_iter().collect::<Vec<_>>(),
            },
        )));
        Ok(())
    } else {
        error_expected("array start or expression", elem)
    }
}

fn finish_construction(accumulated: &mut Vec<PartialExpression>) -> Result<Expression, AnyError> {
    if accumulated.len() <= 1 {
        match accumulated.pop() {
            Some(PartialExpression::Expression(e)) => {
                return Ok(e);
            }
            None => return Ok(Expression::Nothing),
            Some(v) => {
                accumulated.push(v);
            }
        }
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

    #[test]
    fn test_types() {
        ast_deserialize("5 :i64() Op Chain").expect("should parse");
        ast_deserialize("[] :i64() Op Chain").expect("should parse");
        ast_deserialize("5 :tuple(i64()) Op Chain").expect("should parse");
        // ast_deserialize("5 :tuple[i64 i64] Type Chain").expect("should parse");
        // ast_deserialize("5 :tuple[:tuple[:i64 Type] Type :i64 Type] Type Chain").expect("should parse");
        // ast_deserialize("5 [[i64:]tuple: i64:]tuple: Chain").expect("should parse");
        ast_deserialize("[] :tuple(tuple(i64()) i64()) Op Chain").expect("should parse");
        // ast_deserialize("5 [[i64 n:]tuple ns: i64 outer:]tuple all: Chain").expect("should parse");
    }
}
