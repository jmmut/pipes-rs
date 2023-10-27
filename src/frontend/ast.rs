use crate::common::AnyError;
use crate::frontend::lexer::{lex, Operator, Token, Tokens};
use crate::frontend::parser::{Expression, StaticList, Transformation};
use std::collections::VecDeque;

pub struct Ast;

#[derive(Debug, PartialEq)]
enum VirtualToken {
    StartArray,
    StartChain,
    // Actual(Token),
    Operator(Operator),
    Expression(Expression),
    Transformation(Transformation),
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
                Token::Comma => Self::construct_transformation(&mut accumulated)?,
                Token::OpenBrace => accumulated.push(VirtualToken::StartChain),
                Token::CloseBrace => Self::construct_chain(&mut accumulated)?,
                Token::OpenBracket => accumulated.push(VirtualToken::StartArray),
                Token::CloseBracket => Self::construct_array(&mut accumulated)?,
            };
        }
        Self::finish_construction(&mut accumulated)
    }

    fn construct_transformation(accumulated: &mut Vec<VirtualToken>) -> Result<(), AnyError> {
        let elem = accumulated.pop();
        if let Some(VirtualToken::Expression(operand)) = elem {
            let elem = accumulated.pop();
            if let Some(VirtualToken::Operator(operator)) = elem {
                accumulated.push(VirtualToken::Transformation(Transformation {
                    operator,
                    operand,
                }));
                Ok(())
            } else {
                Err(format!("expected operator, but was '{:?}'", elem).into())
            }
        } else {
            Err(format!("expected operand, but was '{:?}'", elem).into())
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
            let elem = accumulated.pop();
            if let Some(VirtualToken::StartChain) = elem {
                accumulated.push(VirtualToken::Expression(Expression::Chain {
                    initial: Box::new(initial),
                    transformations: transformations.into_iter().collect::<Vec<_>>(),
                }));
                Ok(())
            } else {
                Err(format!("expected chain start, but was '{:?}'", elem).into())
            }
        } else {
            // TODO: construct transform
            Err(format!("expected initial expression, but was '{:?}'", elem).into())
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
            Err(format!("expected array start, but was {:?}", elem))?
        }
    }

    fn finish_construction(accumulated: &mut Vec<VirtualToken>) -> Result<Expression, AnyError> {
        if let Some(VirtualToken::Expression(e)) = accumulated.pop() {
            assert_eq!(
                *accumulated,
                Vec::new(),
                "incorrect remaining AST: {:?}",
                accumulated
            );

            return Ok(e);
            // Ok(Expression::Nothing)
        } else {
            Err(format!("incorrect remaining AST: {:?}", accumulated).into())
        }
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
        let ast = "{5 +7, +8,}";
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
        let ast = "[ {5 +7, |parse_char,}  8 ]";
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
