use crate::common::AnyError;
use crate::frontend::expression::{Expression, StaticList, Transformation};
use crate::frontend::lexer::{lex, Operator, Token, Tokens};
use std::collections::VecDeque;
use std::fmt::Debug;

pub struct Ast;

#[derive(Debug, PartialEq)]
pub enum VirtualToken {
    StartArray,
    StartChain,
    // Actual(Token),
    Operator(Operator),
    Expression(Expression),
}

impl Ast {
    pub fn deserialize(s: &str) -> Result<Expression, AnyError> {
        let tokens = lex(s).unwrap();
        Self::deserialize_tokens(tokens)
    }
    pub fn deserialize_tokens(tokens: Tokens) -> Result<Expression, AnyError> {
        let mut accumulated = Vec::new();
        for token in tokens {
            match token {
                Token::Number(n) => {
                    accumulated.push(VirtualToken::Expression(Expression::Value(n)))
                }
                Token::Operator(operator) => accumulated.push(VirtualToken::Operator(operator)),
                Token::Identifier(ident) => {
                    accumulated.push(VirtualToken::Expression(Expression::Identifier(ident)));
                }
                Token::OpenBrace => accumulated.push(VirtualToken::StartChain),
                Token::CloseBrace => Self::construct_flat_chain(&mut accumulated)?,
                Token::OpenBracket => accumulated.push(VirtualToken::StartArray),
                Token::CloseBracket => Self::construct_array(&mut accumulated)?,
            };
        }
        Self::finish_construction(&mut accumulated)
    }

    fn construct_flat_chain(accumulated: &mut Vec<VirtualToken>) -> Result<(), AnyError> {
        let mut transformations = VecDeque::new();

        let mut elem_expression = accumulated.pop();
        if let Some(VirtualToken::StartChain) = elem_expression {
            accumulated.push(VirtualToken::Expression(Expression::Nothing));
            Ok(())
        } else {
            while let Some(VirtualToken::Expression(operand)) = elem_expression {
                let elem_operator = accumulated.pop();
                match elem_operator {
                    None => {
                        return Err("unbalanced brace".into());
                    }
                    Some(VirtualToken::Operator(operator)) => {
                        transformations.push_front(Transformation { operator, operand });
                    }
                    Some(VirtualToken::StartChain) => {
                        accumulated.push(VirtualToken::Expression(Expression::Chain {
                            initial: Box::new(operand),
                            transformations: transformations.into_iter().collect::<Vec<_>>(),
                        }));
                        return Ok(());
                    }
                    _ => {
                        return error_expected("operator or chain start", elem_operator);
                    }
                }
                elem_expression = accumulated.pop();
            }
            error_expected(
                "expression or an opening chain (for Nothing) before closing a chain",
                elem_expression,
            )
        }
    }

    fn construct_array(accumulated: &mut Vec<VirtualToken>) -> Result<(), AnyError> {
        let mut expressions = VecDeque::new();
        let mut elem = accumulated.pop();
        while let Some(VirtualToken::Expression(e)) = elem {
            expressions.push_front(e);
            elem = accumulated.pop()
        }
        if let Some(VirtualToken::StartArray) = elem {
            accumulated.push(VirtualToken::Expression(Expression::StaticList(
                StaticList {
                    elements: expressions.into_iter().collect::<Vec<_>>(),
                },
            )));
            Ok(())
        } else {
            error_expected("array start", elem)
        }
    }

    fn finish_construction(accumulated: &mut Vec<VirtualToken>) -> Result<Expression, AnyError> {
        match accumulated.pop() {
            Some(VirtualToken::Expression(e)) => {
                if accumulated.is_empty() {
                    return Ok(e);
                }
            }
            None => {}
            Some(a) => accumulated.push(a),
        }

        Err(format!("unfinished code: {:?}", accumulated).into())
    }
}

pub fn error_expected<T: Debug>(expected: &str, actual: T) -> Result<(), AnyError> {
    Err(format!("expected {} but was {:?}", expected, actual).into())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::expression::Expression::{Chain, Identifier, Value};
    use crate::frontend::expression::{Expression, StaticList, Transformation, Transformations};
    use crate::frontend::lexer::Operator;

    #[test]
    fn test_value() {
        let ast = "5";
        let expected = Value(5);
        assert_eq!(Ast::deserialize(ast).unwrap(), expected);
    }
    #[test]
    fn test_chained_value() {
        let ast = "{5}";
        let expected = Chain {
            initial: Box::new(Value(5)),
            transformations: Transformations::new(),
        };
        assert_eq!(Ast::deserialize(ast).unwrap(), expected);
    }

    #[test]
    fn test_chain() {
        let ast = "{5 +7 +8}";
        let expected = Chain {
            initial: Box::new(Value(5)),
            transformations: vec![
                Transformation {
                    operator: Operator::Add,
                    operand: Value(7),
                },
                Transformation {
                    operator: Operator::Add,
                    operand: Value(8),
                },
            ],
        };
        assert_eq!(Ast::deserialize(ast).unwrap(), expected);
    }

    #[test]
    fn test_complex() {
        let ast = "[ {5 +7 |parse_char}  8 ]";
        let expected = Expression::StaticList(StaticList {
            elements: vec![
                Chain {
                    initial: Box::new(Value(5)),
                    transformations: vec![
                        Transformation {
                            operator: Operator::Add,
                            operand: Value(7),
                        },
                        Transformation {
                            operator: Operator::Call,
                            operand: Identifier("parse_char".to_string()),
                        },
                    ],
                },
                Value(8),
            ],
        });
        // let expected = "List(Chain(5, +7, |parse_char), 8)";
        assert_eq!(Ast::deserialize(ast).unwrap(), expected);
    }

    #[test]
    fn test_unfinished() {
        Ast::deserialize("5+").expect_err("should fail");
    }
}
