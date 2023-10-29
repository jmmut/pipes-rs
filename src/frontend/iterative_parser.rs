use crate::common::AnyError;
use crate::frontend::expression::{Expression, StaticList, Transformation};
use crate::frontend::lexer::{Operator, Token, Tokens};
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
    #[cfg(test)]
    pub fn deserialize(s: &str) -> Result<Expression, AnyError> {
        let tokens = crate::frontend::lexer::lex(s).unwrap();
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
                Token::CloseBrace => Self::push(&mut accumulated, Self::construct_flat_chain)?,
                Token::OpenBracket => accumulated.push(VirtualToken::StartArray),
                Token::CloseBracket => Self::push(&mut accumulated, Self::construct_array)?,
            };
        }
        Self::finish_construction(&mut accumulated)
    }

    fn push(
        accumulated: &mut Vec<VirtualToken>,
        f: fn(&mut Vec<VirtualToken>) -> Result<Expression, AnyError>,
    ) -> Result<(), AnyError> {
        let e = f(accumulated)?;
        accumulated.push(VirtualToken::Expression(e));
        Ok(())
    }

    fn construct_flat_chain(accumulated: &mut Vec<VirtualToken>) -> Result<Expression, AnyError> {
        let mut transformations = VecDeque::new();

        let mut elem_expression = accumulated.pop();
        if let Some(VirtualToken::StartChain) = elem_expression {
            Ok(Expression::Nothing)
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
                        return Ok(Expression::Chain {
                            initial: Box::new(operand),
                            transformations: transformations.into_iter().collect::<Vec<_>>(),
                        })
                    }
                    _ => {
                        return error_expected("operator or chain start", elem_operator)?;
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

    fn construct_array(accumulated: &mut Vec<VirtualToken>) -> Result<Expression, AnyError> {
        let mut expressions = VecDeque::new();
        let mut elem = accumulated.pop();
        while let Some(VirtualToken::Expression(e)) = elem {
            expressions.push_front(e);
            elem = accumulated.pop()
        }
        if let Some(VirtualToken::StartArray) = elem {
            Ok(Expression::StaticList(StaticList {
                elements: expressions.into_iter().collect::<Vec<_>>(),
            }))
        } else {
            error_expected("array start", elem)
        }
    }

    fn finish_construction(accumulated: &mut Vec<VirtualToken>) -> Result<Expression, AnyError> {
        if accumulated.len() <= 1 {
            match accumulated.pop() {
                Some(VirtualToken::Expression(e)) => {
                    return Ok(e);
                }

                None => Ok(Expression::Nothing),
                Some(v) => {
                    accumulated.push(v);
                    Err(format!("unfinished code: {:?}", accumulated).into())
                }
            }
        } else {
            accumulated.insert(0, VirtualToken::StartChain);
            let e = Self::construct_flat_chain(accumulated)?;
            if !accumulated.is_empty() {
                Err(format!("unfinished code: {:?}", accumulated).into())
            } else {
                Ok(e)
            }
        }
    }
}

pub fn error_expected<T: Debug, R>(expected: &str, actual: T) -> Result<R, AnyError> {
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
