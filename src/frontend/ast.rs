use crate::common::AnyError;
use crate::frontend::lexer::Token::Operator;
use crate::frontend::lexer::{lex, Token};
use crate::frontend::parser::{Expression, Transformation};
use std::collections::VecDeque;

pub struct Ast;

#[derive(Debug)]
enum VirtualToken {
    OpenChain,
    Actual(Token),
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
                Token::Identifier(ident) => match ident.as_str() {
                    "Chain" => {
                        let e = VirtualToken::Expression(Self::construct_chain(&mut accumulated));
                        accumulated.push(e);
                    }
                    &_ => unimplemented!(),
                },
                Token::Operator(operator) => {
                    let operand = if let Some(VirtualToken::Expression(o)) = accumulated.pop() {
                        o
                    } else {
                        Err("expected operand")?
                    };
                    accumulated.push(VirtualToken::Transformation(Transformation {
                        operator,
                        operand,
                    }));
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
            assert_eq!(accumulated.len(), 0);
            return Ok(e);
        } else {
            panic!("incorrect AST: {:?}", accumulated);
        }
    }

    fn construct_chain(accumulated: &mut Vec<VirtualToken>) -> Expression {
        let mut transformations = VecDeque::new();
        loop {
            match accumulated.pop() {
                Some(VirtualToken::Transformation(t)) => {
                    transformations.push_front(t);
                }
                Some(VirtualToken::Expression(initial)) => {
                    return Expression::Chain {
                        initial: Box::new(initial),
                        transformations: transformations.into_iter().collect::<Vec<_>>(),
                    };
                }
                _ => unreachable!(),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::lexer::Operator;
    use crate::frontend::parser::Expression::{Chain, Value};
    use crate::frontend::parser::{Expression, StaticList, Transformation};

    #[test]
    fn test_value() {
        let ast = "5";
        let expected = Value(5);
        assert_eq!(Ast::deserialize(ast).unwrap(), expected);
    }

    #[test]
    fn test_chain() {
        let ast = "5 7 + 8 + Chain";
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
        let ast = "Array(Chain(5, TF(+, 7), TF(|, parse_char)), 8)";
        let ast = "ArrayStart 5 7 +(Chain(5, TF(+, 7), TF(|, parse_char)))";
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
                            operator: Operator::Add,
                            operand: Value(12),
                        },
                        Transformation {
                            operator: Operator::Add,
                            operand: Value(34),
                        },
                    ],
                },
                Value(8),
            ],
        });
        assert_eq!(Ast::deserialize(ast).unwrap(), expected);
    }
}
