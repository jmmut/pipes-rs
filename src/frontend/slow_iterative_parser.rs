use std::collections::VecDeque;

use crate::common::{context, AnyError};
use crate::frontend::ast::{construct_function_from_chain, error_expected, PartialExpression};
use crate::frontend::expression::{Chain, Expression, Transformation, Type};
use crate::frontend::lexer::{Operator, Token, Tokens};

#[cfg(test)]
pub fn parse<S: AsRef<str>>(code_text: S) -> Result<Expression, AnyError> {
    let tokens = crate::frontend::lexer::lex(code_text)?;
    parse_tokens(tokens)
}

pub fn parse_tokens(tokens: Tokens) -> Result<Expression, AnyError> {
    context("Parser", Parser::parse_tokens(tokens))
}
pub struct Parser {
    accumulated: Vec<PartialExpression>,
}

impl Parser {
    fn parse_tokens(tokens: Tokens) -> Result<Expression, AnyError> {
        let mut ast = Parser {
            accumulated: Vec::new(),
        };
        for token in tokens {
            match token {
                Token::Number(n) => ast.push(Expression::Value(n)),
                Token::Operator(operator) => ast.push_pe(PartialExpression::Operator(operator)),
                Token::Identifier(ident) => ast.push(Expression::Identifier(ident)),
                Token::Keyword(keyword) => ast.push_pe(PartialExpression::Keyword(keyword)),
                Token::OpenBrace => ast.push_pe(PartialExpression::OpenBrace),
                Token::CloseBrace => ast.push_f(Self::construct_flat_chain)?,
                Token::OpenBracket => ast.push_pe(PartialExpression::OpenBracket),
                Token::CloseBracket => ast.push_f(Self::construct_array)?,
                Token::OpenParenthesis => ast.push_pe(PartialExpression::OpenParenthesis),
                Token::CloseParenthesis => ast.push_f(Self::construct_type)?,
                // _ => return error_expected("anything else", token),
            };
        }
        Self::finish_construction(&mut ast.accumulated)
    }

    fn push_f(
        &mut self,
        construct_expression: fn(&mut Vec<PartialExpression>) -> Result<Expression, AnyError>,
    ) -> Result<(), AnyError> {
        let expression = construct_expression(&mut self.accumulated)?;
        self.push(expression);
        Ok(())
    }

    fn push(&mut self, expression: Expression) {
        self.push_pe(PartialExpression::Expression(expression));
    }
    fn push_pe(&mut self, partial_expression: PartialExpression) {
        self.accumulated.push(partial_expression);
    }

    fn construct_flat_chain(
        accumulated: &mut Vec<PartialExpression>,
    ) -> Result<Expression, AnyError> {
        let mut transformations = VecDeque::new();

        let mut elem_expression = accumulated.pop();
        if let Some(PartialExpression::OpenBrace) = elem_expression {
            Self::attempt_construct_function(accumulated, Chain::empty())
        } else {
            while let Some(PartialExpression::Expression(operand)) = elem_expression {
                let elem_operator = accumulated.pop();
                match elem_operator {
                    Some(PartialExpression::Operator(operator)) => {
                        transformations
                            .push_front(Self::construct_transformation(operator, operand));
                    }
                    Some(PartialExpression::OpenBrace) => {
                        return Self::attempt_construct_function(
                            accumulated,
                            Chain::new(
                                Box::new(operand),
                                transformations.into_iter().collect::<Vec<_>>(),
                            ),
                        );
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
    pub fn construct_transformation(operator: Operator, operand: Expression) -> Transformation {
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
    fn attempt_construct_function(
        accumulated: &mut Vec<PartialExpression>,
        chain: Chain,
    ) -> Result<Expression, AnyError> {
        match construct_function_from_chain(accumulated, chain) {
            Ok(func) => Ok(Expression::Function(func)),
            Err((_error, chain)) => Ok(Expression::Chain(chain)),
        }
    }
    fn construct_array(accumulated: &mut Vec<PartialExpression>) -> Result<Expression, AnyError> {
        let mut expressions = VecDeque::new();
        let mut elem = accumulated.pop();
        while let Some(PartialExpression::Expression(e)) = elem {
            expressions.push_front(e);
            elem = accumulated.pop()
        }
        if let Some(PartialExpression::OpenBracket) = elem {
            Ok(Expression::StaticList {
                elements: expressions.into_iter().collect::<Vec<_>>(),
            })
        } else {
            error_expected("array start or expression", elem)
        }
    }
    fn construct_type(accumulated: &mut Vec<PartialExpression>) -> Result<Expression, AnyError> {
        let mut types = VecDeque::new();
        let mut elem = accumulated.pop();
        while let Some(pe) = elem {
            match pe {
                PartialExpression::OpenParenthesis => {
                    elem = accumulated.pop();
                    break;
                }
                PartialExpression::Expression(Expression::Type(a_type)) => {
                    types.push_front(a_type);
                }
                PartialExpression::Expression(Expression::Identifier(type_name)) => {
                    types.push_front(Type::simple(type_name));
                }
                _ => return error_expected("type start or type expression", pe),
            }
            elem = accumulated.pop();
        }

        if let Some(PartialExpression::Expression(Expression::Identifier(parent))) = elem {
            let a_type = Type::from_nameless(parent, types.into_iter().collect::<Vec<_>>());
            Ok(Expression::Type(a_type))
        } else {
            error_expected("array start or expression", elem)
        }
    }

    fn finish_construction(
        accumulated: &mut Vec<PartialExpression>,
    ) -> Result<Expression, AnyError> {
        if accumulated.len() <= 1 {
            match accumulated.pop() {
                Some(PartialExpression::Expression(e)) => {
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
            accumulated.insert(0, PartialExpression::OpenBrace);
            let e = Self::construct_flat_chain(accumulated)?;
            if !accumulated.is_empty() {
                Err(error_message.into())
            } else {
                Ok(e)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::frontend::ast::ast_deserialize;
    use crate::frontend::expression::Expression::Value;

    use super::*;

    #[test]
    fn test_nothing() {
        let ast = "{}";
        let expected = Expression::empty_chain();
        assert_eq!(parse(ast).unwrap(), expected);
    }
    #[test]
    fn test_value() {
        let ast = "5";
        let expected = Value(5);
        assert_eq!(parse(ast).unwrap(), expected);
    }
    #[test]
    fn test_chained_value() {
        let parsed = parse("{5}");
        let expected = ast_deserialize("5 Chain").unwrap();
        assert_eq!(parsed.unwrap(), expected);
    }

    #[test]
    fn test_chain() {
        let parsed = parse("{5 +7 +8}");
        let expected = ast_deserialize("5 +7 Op +8 Op Chain").unwrap();
        assert_eq!(parsed.unwrap(), expected);
    }

    #[test]
    fn test_complex() {
        let parsed = parse("[ {5 +7 |parse_char}  8 ]");
        let expected = ast_deserialize("[ 5 +7 Op |parse_char Op Chain 8 ]").unwrap();
        assert_eq!(parsed.unwrap(), expected);
    }

    #[test]
    fn test_unfinished() {
        parse("5+").expect_err("should fail");
        parse("{+5}").expect_err("should fail");
    }

    #[test]
    fn test_types() {
        let parsed = parse("5 :i64");
        let expected = ast_deserialize("5 :i64() Op Chain").unwrap();
        assert_eq!(parsed.unwrap(), expected);
    }
    #[test]
    fn test_complex_types() {
        let parsed = parse("5 :tuple(i64 i64)");
        let expected = ast_deserialize("5 :tuple(:i64() UT :i64() UT) Op Chain").unwrap();
        assert_eq!(parsed.unwrap(), expected);
    }
    #[test]
    fn test_assignment() {
        assert_eq_ast("function {} = noop", "function {} Fn = noop Op Chain");
    }

    #[test]
    #[ignore]
    fn test_branch() {
        assert_eq_ast(
            "5 |branch {7} {8}",
            "5 | branch 5 Chain 8 Chain Br Op Chain",
        );
    }

    mod function {
        use super::*;

        #[test]
        fn test_empty_function() {
            assert_eq_ast("function {}", "function {} Fn");
        }
        #[test]
        fn test_function_value() {
            assert_eq_ast("function {5}", "function 5 Chain Fn");
        }
        #[test]
        fn test_function_arg() {
            assert_eq_ast("function x {5}", "function x 5 Chain Fn");
        }
    }

    fn assert_eq_ast(code: &str, ast: &str) {
        let parsed = parse(code);
        let expected = ast_deserialize(ast).unwrap();
        assert_eq!(parsed.unwrap(), expected);
    }
}
