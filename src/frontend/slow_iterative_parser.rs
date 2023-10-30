use std::collections::VecDeque;
use std::fmt::Debug;

use crate::common::{context, AnyError};
use crate::frontend::expression::{Expression, Transformation, Type};
use crate::frontend::lexer::{Operator, Token, Tokens};

pub struct Parser {
    accumulated: Vec<VirtualToken>,
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
        let mut ast = Parser {
            accumulated: Vec::new(),
        };
        for token in tokens {
            match token {
                Token::Number(n) => ast.push(Expression::Value(n))?,
                Token::Operator(operator) => ast.push_vt(VirtualToken::Operator(operator))?,
                Token::Identifier(ident) => ast.push(Expression::Identifier(ident))?,
                Token::OpenBrace => ast.push_vt(VirtualToken::StartChain)?,
                Token::CloseBrace => ast.push_f(Self::construct_flat_chain)?,
                Token::OpenBracket => ast.push_vt(VirtualToken::StartArray)?,
                Token::CloseBracket => ast.push_f(Self::construct_array)?,
                Token::OpenParenthesis => ast.push_vt(VirtualToken::StartType)?,
                Token::CloseParenthesis => ast.push_f(Self::construct_type)?,
                // _ => return error_expected("anything else", token),
            };
        }
        Self::finish_construction(&mut ast.accumulated)
    }

    fn push_f(
        &mut self,
        construct_expression: fn(&mut Vec<VirtualToken>) -> Result<Expression, AnyError>,
    ) -> Result<(), AnyError> {
        let expression = construct_expression(&mut self.accumulated)?;
        self.push(expression)
    }

    fn push(&mut self, expression: Expression) -> Result<(), AnyError> {
        self.push_vt(VirtualToken::Expression(expression))
    }
    fn push_vt(&mut self, virtual_token: VirtualToken) -> Result<(), AnyError> {
        self.accumulated.push(virtual_token);
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
                    Some(VirtualToken::Operator(operator)) => {
                        transformations
                            .push_front(Self::construct_transformation(operator, operand));
                    }
                    Some(VirtualToken::StartChain) => {
                        return Ok(Expression::Chain {
                            initial: Box::new(operand),
                            transformations: transformations.into_iter().collect::<Vec<_>>(),
                        })
                    }
                    None => {
                        return Err("unbalanced brace".into());
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
    fn construct_transformation(operator: Operator, operand: Expression) -> Transformation {
        if let Operator::Type = operator {
            if let Expression::Identifier(name) = operand {
                return Transformation {
                    operator,
                    operand: Expression::Type(Type::simple(name)),
                };
            }
        }
        Transformation { operator, operand }
    }
    fn construct_array(accumulated: &mut Vec<VirtualToken>) -> Result<Expression, AnyError> {
        let mut expressions = VecDeque::new();
        let mut elem = accumulated.pop();
        while let Some(VirtualToken::Expression(e)) = elem {
            expressions.push_front(e);
            elem = accumulated.pop()
        }
        if let Some(VirtualToken::StartArray) = elem {
            Ok(Expression::StaticList {
                elements: expressions.into_iter().collect::<Vec<_>>(),
            })
        } else {
            error_expected("array start or expression", elem)
        }
    }
    fn construct_type(accumulated: &mut Vec<VirtualToken>) -> Result<Expression, AnyError> {
        let mut types = VecDeque::new();
        let mut elem = accumulated.pop();
        while let Some(vt) = elem {
            match vt {
                VirtualToken::StartType => {
                    elem = accumulated.pop();
                    break;
                }
                VirtualToken::Expression(Expression::Type(a_type)) => {
                    types.push_front(a_type);
                }
                VirtualToken::Expression(Expression::Identifier(type_name)) => {
                    types.push_front(Type::simple(type_name));
                }
                _ => return error_expected("type start or type expression", vt),
            }
            elem = accumulated.pop();
        }

        if let Some(VirtualToken::Expression(Expression::Identifier(parent))) = elem {
            let a_type = Type::from(parent, types.into_iter().collect::<Vec<_>>());
            Ok(Expression::Type(a_type))
        } else {
            error_expected("array start or expression", elem)
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
            let error_message = format!("unfinished code: {:?}", accumulated);
            accumulated.insert(0, VirtualToken::StartChain);
            let e = Self::construct_flat_chain(accumulated)?;
            if !accumulated.is_empty() {
                Err(error_message.into())
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
    }

    #[test]
    fn test_complex() {
        let parsed = Parser::parse("[ {5 +7 |parse_char}  8 ]");
        let expected = ast_deserialize("[ 5 +7 Op |parse_char Op Chain 8 ]").unwrap();
        assert_eq!(parsed.unwrap(), expected);
    }

    #[test]
    fn test_unfinished() {
        Parser::parse("5+").expect_err("should fail");
        Parser::parse("{+5}").expect_err("should fail");
    }

    #[test]
    fn test_types() {
        let parsed = Parser::parse("5 :i64");
        let expected = ast_deserialize("5 :i64() Op Chain").unwrap();
        assert_eq!(parsed.unwrap(), expected);
    }
    #[test]
    fn test_complex_types() {
        let parsed = Parser::parse("5 :tuple(i64 i64)");
        let expected = ast_deserialize("5 :tuple(i64() i64()) Op Chain").unwrap();
        assert_eq!(parsed.unwrap(), expected);
    }

    // #[test]
    // fn test_function() {
    //     let parsed = Parser::parse("function {}");
    //     let expected = ast_deserialize("").unwrap();
    //     assert_eq!(parsed.unwrap(), expected);
    // }
}
