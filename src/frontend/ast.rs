use crate::common::AnyError;
use crate::frontend::lexer::{lex, Operator, Token};
use crate::frontend::parser::{Expression, StaticList, Transformation};
use std::collections::VecDeque;

pub struct Ast;

#[derive(Debug, PartialEq)]
enum VirtualToken {
    StartArray,
    Actual(Token),
    Operator(Operator),
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
                Token::Operator(operator) => accumulated.push(VirtualToken::Operator(operator)),
                Token::Identifier(ident) => {
                    if ident == "Chain" {
                        Self::construct_chain(&mut accumulated)?;
                    } else {
                        accumulated.push(VirtualToken::Expression(Expression::Identifier(ident)));
                    }
                }
                Token::OpenBracket => accumulated.push(VirtualToken::StartArray),
                Token::CloseBracket => {
                    Self::construct_array(&mut accumulated)?;
                }
                Token::CloseBrace => {
                    Self::construct_chain(&mut accumulated)?;
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
            panic!("incorrect remaining AST: {:?}", accumulated);
        }
    }

    fn construct_chain(accumulated: &mut Vec<VirtualToken>) -> Result<(), AnyError> {
        let mut transformations = VecDeque::new();
        let mut elem = accumulated.pop();
        while let Some(VirtualToken::Transformation(t)) = elem {
            transformations.push_front(t);
            elem = accumulated.pop()
        }
        if let Some(VirtualToken::Expression(initial)) = elem {
            accumulated.push(VirtualToken::Expression(Expression::Chain {
                initial: Box::new(initial),
                transformations: transformations.into_iter().collect::<Vec<_>>(),
            }));
            Ok(())
        } else {
            Err("expected initial expression")?
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
            Err(format!("expected {:?}", Token::OpenBracket))?
        }
    }

    fn construct_transformation(accumulated: &mut Vec<VirtualToken>) -> Result<(), AnyError> {
        let operand = if let Some(VirtualToken::Expression(o)) = accumulated.pop() {
            o
        } else {
            Err("expected operand")?
        };
        let operator = if let Some(VirtualToken::Operator(o)) = accumulated.pop() {
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
        let ast = "[ 5 +7, |parse_char, }  8 ]";
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
