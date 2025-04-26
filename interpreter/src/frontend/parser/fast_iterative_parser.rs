use std::fmt::Debug;

use crate::common::{AnyError, context};
use crate::frontend::expression::{Expression, Expressions, Transformation, Transformations, Type};
use crate::frontend::lexer::{Operator, Token, Tokens};

pub struct Parser {
    accumulated: Expressions,
    currently_parsing_stack: Vec<Parsing>,
}

#[derive(Debug, PartialEq)]
pub enum Parsing {
    AnyExpression,
    Value,
    Identifier,
    Chain,
    TransformationsOperator,
    TransformationsOperand(Operator),
    BracelessTransformations,
    List,
    TypeChildren,
    Eof,
}
#[derive(Debug, PartialEq)]
pub enum VirtualToken {
    StartArray,
    StartChain,
    StartType,
    // Actual(Token),
    Operator(Operator),
    Expression(Expression),
}

impl Parser {
    #[cfg(test)]
    pub fn parse(s: &str) -> Result<Expression, AnyError> {
        let tokens = crate::frontend::lexer::lex(s).unwrap();
        context("Parser", Self::parse_tokens(tokens))
    }

    pub fn parse_tokens(tokens: Tokens) -> Result<Expression, AnyError> {
        // let mut ast = Parser {
        //     accumulated: Vec::new(),
        // };
        let mut currently_parsing_stack = Vec::<Parsing>::new();
        currently_parsing_stack.push(Parsing::Eof);
        let mut currently_parsing = Parsing::AnyExpression;
        let mut transformations = Vec::new();
        let mut transformations_stack = Vec::new();
        let mut working_expressions = Vec::new();
        let mut working_expressions_stack = Vec::new();
        let error_expected = |message, token, currently_parsing| {
            error_expected_actual(
                &format!("{} (currently parsing: {:?})", message, currently_parsing),
                token,
            )
        };
        for token in tokens {
            match currently_parsing {
                Parsing::AnyExpression => {
                    match token {
                        Token::Number(n) => {
                            working_expressions.push(Expression::Value(n));
                            currently_parsing = Parsing::Value;
                        }
                        Token::Identifier(ident) => {
                            working_expressions.push(Expression::Identifier(ident));
                            currently_parsing = Parsing::Identifier;
                        }
                        Token::OpenBrace => {
                            currently_parsing = Parsing::Chain;
                            working_expressions_stack.push(working_expressions);
                            working_expressions = Vec::new();
                        }
                        Token::OpenBracket => {
                            currently_parsing = Parsing::List;
                            working_expressions_stack.push(working_expressions);
                            working_expressions = Vec::new();
                        }
                        _ => {
                            return error_expected(
                                "expression (value, identifier, chain or list)",
                                token,
                                currently_parsing,
                            )
                        }
                    };
                }
                Parsing::Identifier => match token {
                    Token::OpenParenthesis => currently_parsing = Parsing::TypeChildren,
                    Token::Number(_) => {}
                    Token::Operator(_) => {}
                    Token::Identifier(_) => {}
                    Token::OpenBracket => {}
                    Token::CloseBracket => {}
                    Token::OpenBrace => {}
                    Token::CloseBrace => {}
                    Token::CloseParenthesis => {}
                    _ => return error_expected("???", token, currently_parsing),
                },
                Parsing::Chain => match token {
                    Token::Number(n) => {
                        working_expressions.push(Expression::Value(n));
                        currently_parsing = Parsing::TransformationsOperator;
                        transformations_stack.push(transformations);
                        transformations = Vec::new();
                    }
                    Token::Identifier(name) => {
                        working_expressions.push(Expression::Identifier(name));
                        currently_parsing = Parsing::TransformationsOperator;
                        transformations_stack.push(transformations);
                        transformations = Vec::new();
                    }
                    Token::OpenBracket => {
                        currently_parsing_stack.push(currently_parsing);
                        currently_parsing = Parsing::List;
                    }
                    Token::OpenBrace => {
                        currently_parsing_stack.push(currently_parsing);
                        currently_parsing = Parsing::Chain;
                    }
                    _ => {
                        return error_expected(
                            "expression (value, identifier, chain or list)",
                            token,
                            currently_parsing,
                        )
                    }
                },
                Parsing::TransformationsOperator => match token {
                    Token::Operator(operator) => {
                        currently_parsing = Parsing::TransformationsOperand(operator);
                    }
                    Token::CloseBrace => {
                        let chain = Expression::chain(
                            Box::new(working_expressions.pop().unwrap()),
                            transformations,
                        );
                        currently_parsing = currently_parsing_stack.pop().unwrap();
                        transformations = transformations_stack.pop().unwrap();
                        working_expressions = working_expressions_stack.pop().unwrap();
                        match currently_parsing {
                            Parsing::TransformationsOperand(operator) => {
                                transformations.push(Transformation {
                                    operator,
                                    operand: chain,
                                });
                                currently_parsing = Parsing::TransformationsOperator;
                            }
                            _ => {
                                working_expressions.push(chain);
                            }
                        }
                    }
                    _ => return error_expected("operator or chain end", token, currently_parsing),
                },
                Parsing::TransformationsOperand(operator) => match token {
                    Token::Number(n) => {
                        transformations.push(Transformation {
                            operator,
                            operand: Expression::Value(n),
                        });
                        currently_parsing = Parsing::TransformationsOperator;
                    }
                    Token::Identifier(name) => {
                        transformations.push(Transformation {
                            operator,
                            operand: Expression::Identifier(name),
                        });
                        currently_parsing = Parsing::TransformationsOperator;
                    }
                    Token::OpenBrace => {
                        currently_parsing_stack.push(currently_parsing);
                        currently_parsing = Parsing::Chain;
                        working_expressions_stack.push(working_expressions);
                        working_expressions = Vec::new();
                    }
                    Token::OpenBracket => {
                        unimplemented!()
                    }
                    _ => {
                        return error_expected(
                            "expression (value, identifier, chain or list)",
                            token,
                            currently_parsing,
                        )
                    }
                },
                Parsing::List => match token {
                    Token::Number(n) => {
                        working_expressions.push(Expression::Value(n));
                    }
                    Token::Identifier(name) => {
                        working_expressions.push(Expression::Identifier(name));
                    }
                    Token::OpenBracket => {
                        working_expressions_stack.push(working_expressions);
                        working_expressions = Vec::new();
                    }
                    Token::CloseBracket => {
                        let mut previous_working_expressions =
                            working_expressions_stack.pop().unwrap();
                        previous_working_expressions.push(Expression::StaticList {
                            elements: working_expressions,
                        });
                        working_expressions = previous_working_expressions;
                    }
                    Token::OpenBrace => {
                        working_expressions_stack.push(working_expressions);
                        working_expressions = Vec::new();
                        currently_parsing_stack.push(currently_parsing);
                        currently_parsing = Parsing::Chain;
                    }
                    Token::CloseBrace => {}
                    Token::OpenParenthesis => {}
                    Token::CloseParenthesis => {}
                    _ => {
                        return error_expected(
                            "expression (value, identifier, chain or list)",
                            token,
                            currently_parsing,
                        )
                    }
                },
                Parsing::Eof => {
                    return error_expected("no more expressions", token, currently_parsing)
                }
                _ => unimplemented!("{:?}", currently_parsing),
            }
        }
        assert_eq!(working_expressions_stack, Vec::<Expressions>::new());
        assert_eq!(transformations_stack, Vec::<Transformations>::new());
        // currently_parsing_stack.push(currently_parsing);
        // assert_eq!(currently_parsing_stack, vec![Parsing::Eof]);
        working_expressions
            .pop()
            .ok_or("unable to parse expression".into())
    }
}

pub fn error_expected_actual<T: Debug, R>(expected: &str, actual: T) -> Result<R, AnyError> {
    err(format!("expected {} but was {:?}", expected, actual))
}

#[cfg(test)]
mod tests {
    use crate::frontend::ast::ast_deserialize;
    use crate::frontend::expression::Expression::Value;

    use super::*;

    #[test]
    fn test_value() {
        let ast = "5";
        let expected = Value(5);
        assert_eq!(Parser::parse(ast).unwrap(), expected);
    }

    #[test]
    fn test_chained_value() {
        let parsed = Parser::parse("{5}");
        let expected = ast_deserialize("5 Chain").unwrap();
        assert_eq!(parsed.unwrap(), expected);
    }

    #[test]
    fn test_chain() {
        let parsed = Parser::parse("{5 +7 +8}");
        let expected = ast_deserialize("5 +7 Op +8 Op Chain").unwrap();
        assert_eq!(parsed.unwrap(), expected);
        let parsed = Parser::parse("{5 +{3 + 4} +8}");
        let expected = ast_deserialize("5 + 3 + 4 Op Chain Op +8 Op Chain").unwrap();
        assert_eq!(parsed.unwrap(), expected);
    }

    #[test]
    fn test_list() {
        let parsed = Parser::parse("[7 8 9]");
        let expected = ast_deserialize("[7 8 9]").unwrap();
        assert_eq!(parsed.unwrap(), expected);
        let parsed = Parser::parse("[[4 5] 8 9]");
        let expected = ast_deserialize("[[4 5] 8 9]").unwrap();
        assert_eq!(parsed.unwrap(), expected);
    }

    #[test]
    fn test_complex() {
        let parsed = Parser::parse("[ {5 +7 |parse_char}  8 ]");
        let expected = ast_deserialize("[ 5 +7 Op |parse_char Op Chain 8 ]").unwrap();
        assert_eq!(parsed.unwrap(), expected);
    }

    #[ignore]
    #[test]
    fn test_unfinished() {
        Parser::parse("5+").expect_err("should fail");
        Parser::parse("{+5}").expect_err("should fail");
    }

    #[ignore]
    #[test]
    fn test_types() {
        let parsed = Parser::parse("5 :i64");
        let expected = ast_deserialize("5 :i64() Op Chain").unwrap();
        assert_eq!(parsed.unwrap(), expected);
    }
    #[ignore]
    #[test]
    fn test_complex_types() {
        let parsed = Parser::parse("5 :tuple(i64 i64)");
        let expected = ast_deserialize("5 :tuple(i64() i64()) Op Chain").unwrap();
        assert_eq!(parsed.unwrap(), expected);
    }
}
