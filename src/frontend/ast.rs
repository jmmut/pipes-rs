use crate::common::AnyError;
use crate::frontend::lexer::{lex, Operator, Token};
use crate::frontend::parser::{Expression, StaticList, Transformation};
use std::collections::VecDeque;

pub struct Ast;

#[derive(Debug, PartialEq)]
enum VirtualToken {
    StartArray,
    Actual(Token),
    // Operator(Operator),
    Expression(Expression),
    Transformation(Transformation),
}

impl Ast {
    pub fn deserialize(s: &str) -> Result<Expression, AnyError> {
        let tokens = lex(s).unwrap();
        let mut accumulated = Vec::new();
        for token in tokens {
            match token {
                Token::Number(n) => {
                    accumulated.push(VirtualToken::Expression(Expression::Value(n)))
                }
                Token::Operator(operator) => {
                    accumulated.push(VirtualToken::Actual(Token::Operator(operator)))
                }
                Token::Identifier(ident) => match &ident.as_str() {
                    &"Chain" => {
                        Self::construct_chain(&mut accumulated);
                    }
                    &identifier => accumulated.push(VirtualToken::Expression(
                        Expression::Identifier(identifier.to_string()),
                    )),
                },
                Token::OpenBracket => accumulated.push(VirtualToken::StartArray),
                Token::CloseBracket => {
                    Self::construct_array(&mut accumulated);
                }
                Token::CloseBrace => {
                    Self::construct_chain(&mut accumulated);
                }
                Token::Comma => {
                    Self::construct_transformation(&mut accumulated)?;
                }
                _ => (),
                // Token::Identifier(_) => {}
                // Token::OpenBracket => {}
                // Token::CloseBracket => {}
                // Token::OpenBrace => {}
                // Token::CloseBrace => {}
            };
        }
        if let Some(VirtualToken::Expression(e)) = accumulated.pop() {
            assert_eq!(accumulated, vec![]);
            return Ok(e);
        } else {
            panic!("incorrect AST: {:?}", accumulated);
        }
    }

    fn construct_chain(accumulated: &mut Vec<VirtualToken>) {
        let mut transformations = VecDeque::new();
        loop {
            match accumulated.pop() {
                Some(VirtualToken::Transformation(t)) => {
                    transformations.push_front(t);
                }
                Some(VirtualToken::Expression(initial)) => {
                    accumulated.push(VirtualToken::Expression(Expression::Chain {
                        initial: Box::new(initial),
                        transformations: transformations.into_iter().collect::<Vec<_>>(),
                    }));
                    return;
                }
                _ => unreachable!(),
            }
        }
    }

    fn construct_array(accumulated: &mut Vec<VirtualToken>) {
        let mut expressions = VecDeque::new();
        loop {
            match accumulated.pop() {
                Some(VirtualToken::Expression(e)) => {
                    expressions.push_front(e);
                }
                Some(VirtualToken::StartArray) => {
                    accumulated.push(VirtualToken::Expression(Expression::StaticList(
                        StaticList {
                            elements: expressions.into_iter().collect::<Vec<_>>(),
                        },
                    )));
                    return;
                }
                _ => unreachable!(),
            }
        }
    }
    fn construct_transformation(accumulated: &mut Vec<VirtualToken>) -> Result<(), AnyError> {
        let operand = if let Some(VirtualToken::Expression(o)) = accumulated.pop() {
            o
        } else {
            Err("expected operand")?
        };
        let operator = if let Some(VirtualToken::Actual(Token::Operator(o))) = accumulated.pop() {
            o
        } else {
            Err("expected operator")?
        };
        accumulated.push(VirtualToken::Transformation(Transformation {
            operator,
            operand,
        }));
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::lexer::Operator;
    use crate::frontend::parser::Expression::{Chain, Identifier, Value};
    use crate::frontend::parser::{Expression, StaticList, Transformation};

    #[test]
    fn test_value() {
        let ast = "5";
        let expected = Value(5);
        assert_eq!(Ast::deserialize(ast).unwrap(), expected);
    }

    #[test]
    fn test_chain() {
        let ast = "5 +7, +8, Chain";
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
        let ast = "[ 5 +7, | parse_char, Chain  8 ]";
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
        assert_eq!(Ast::deserialize(ast).unwrap(), expected);
    }
}
