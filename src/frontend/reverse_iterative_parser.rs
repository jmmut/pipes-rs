use std::collections::VecDeque;
use std::fmt::Debug;

use crate::common::{context, AnyError};
use crate::frontend::ast::{construct_function_from_chain, PartialExpression};
use crate::frontend::expression::{
    Branch, Chain, Expression, Function, Transformation, Transformations, Type, TypedIdentifier,
    TypedIdentifiers, Types,
};
use crate::frontend::lexer::{Keyword, Operator, Token, Tokens};

#[cfg(test)]
pub fn parse<S: AsRef<str>>(code_text: S) -> Result<Expression, AnyError> {
    let tokens = crate::frontend::lexer::lex(code_text)?;
    parse_tokens(tokens)
}

pub fn parse_tokens(tokens: Tokens) -> Result<Expression, AnyError> {
    context("Reverse parser", Parser::parse_tokens(tokens))
}
pub struct Parser {
    accumulated: VecDeque<PartialExpression>,
}

impl Parser {
    fn parse_tokens(tokens: Tokens) -> Result<Expression, AnyError> {
        let mut ast = Parser {
            accumulated: VecDeque::new(),
        };
        for token in tokens.into_iter().rev() {
            match token {
                Token::Number(n) => ast.push(Expression::Value(n)),
                Token::Operator(operator) => {
                    let pe = construct_transformation(&mut ast.accumulated, operator)?;
                    ast.accumulated.push_front(pe);
                }
                Token::Identifier(ident) => ast.push(Expression::Identifier(ident)),
                Token::Keyword(keyword) => {
                    let pe = construct_keyword(&mut ast.accumulated, keyword)?;
                    ast.accumulated.push_front(pe);
                }
                Token::OpenBrace => ast.push_f(construct_chain)?,
                Token::CloseBrace => ast.push_pe(PartialExpression::CloseBrace),
                Token::OpenBracket => ast.push_f(construct_array)?,
                Token::CloseBracket => ast.push_pe(PartialExpression::CloseBracket),
                Token::OpenParenthesis => ast.push_f_pe(construct_children_types)?,
                Token::CloseParenthesis => ast.push_pe(PartialExpression::CloseParenthesis),
                // _ => return error_expected("anything else", token),
            };
        }
        finish_construction(&mut ast.accumulated)
    }

    fn push_f(
        &mut self,
        construct_expression: fn(&mut VecDeque<PartialExpression>) -> Result<Expression, AnyError>,
    ) -> Result<(), AnyError> {
        let expression = construct_expression(&mut self.accumulated)?;
        self.push(expression);
        Ok(())
    }
    fn push_f_pe(
        &mut self,
        construct_expression: fn(
            &mut VecDeque<PartialExpression>,
        ) -> Result<PartialExpression, AnyError>,
    ) -> Result<(), AnyError> {
        let expression = construct_expression(&mut self.accumulated)?;
        self.push_pe(expression);
        Ok(())
    }

    fn push(&mut self, expression: Expression) {
        self.push_pe(PartialExpression::Expression(expression));
    }
    fn push_pe(&mut self, partial_expression: PartialExpression) {
        self.accumulated.push_front(partial_expression);
    }
}

fn construct_transformation(
    accumulated: &mut VecDeque<PartialExpression>,
    operator: Operator,
) -> Result<PartialExpression, AnyError> {
    let elem_operand = accumulated.pop_front();
    if let Operator::Type = operator {
        if let Some(PartialExpression::Expression(Expression::Identifier(typename))) = elem_operand
        {
            let operand = get_type_maybe_pop_children(accumulated, typename);
            Ok(PartialExpression::Transformation(Transformation {
                operator,
                operand,
            }))
        } else {
            error_expected("type name after type operator ':'", elem_operand)
        }
    } else {
        if let Some(PartialExpression::Expression(operand)) = elem_operand {
            Ok(PartialExpression::Transformation(Transformation {
                operator,
                operand,
            }))
        } else {
            error_expected("operand after operator", elem_operand)
        }
    }
}

fn get_type_maybe_pop_children(
    accumulated: &mut VecDeque<PartialExpression>,
    typename: String,
) -> Expression {
    let maybe_children = accumulated.pop_front();
    match maybe_children {
        Some(PartialExpression::ChildrenTypes(children)) => {
            return Expression::Type(Type::children(typename, children))
        }
        Some(not_children) => accumulated.push_front(not_children),
        None => {}
    }
    Expression::Type(Type::simple(typename))
}

fn construct_keyword(
    accumulated: &mut VecDeque<PartialExpression>,
    keyword: Keyword,
) -> Result<PartialExpression, AnyError> {
    match keyword {
        Keyword::Function => construct_function(accumulated),
        Keyword::Branch => construct_branch(accumulated),
    }
}

fn construct_function(
    accumulated: &mut VecDeque<PartialExpression>,
) -> Result<PartialExpression, AnyError> {
    let mut elem = accumulated.pop_front();
    let parameter = if let Some(PartialExpression::ChildrenTypes(mut children)) = elem {
        elem = accumulated.pop_front();
        children.truncate(1);
        if let Some(type_) = children.pop() {
            type_
        } else {
            TypedIdentifier::nothing()
        }
    } else {
        TypedIdentifier::nothing()
    };

    if let Some(PartialExpression::Expression(Expression::Chain(body))) = elem {
        Ok(PartialExpression::Expression(Expression::Function(
            Function { parameter, body },
        )))
    } else {
        error_expected("chain for the function body", elem)
    }
}

fn construct_branch(
    accumulated: &mut VecDeque<PartialExpression>,
) -> Result<PartialExpression, AnyError> {
    let mut elem = accumulated.pop_front();
    if let Some(PartialExpression::Expression(Expression::Chain(yes))) = elem {
        let mut elem = accumulated.pop_front();
        if let Some(PartialExpression::Expression(Expression::Chain(no))) = elem {
            Ok(PartialExpression::Expression(Expression::Branch(Branch {
                yes,
                no,
            })))
        } else {
            error_expected("chain for the branch negative case", elem)
        }
    } else {
        error_expected("chain for the branch positive case", elem)
    }
}

fn construct_chain(accumulated: &mut VecDeque<PartialExpression>) -> Result<Expression, AnyError> {
    let elem_expression = accumulated.pop_front();
    match elem_expression {
        Some(PartialExpression::CloseBrace) => Ok(Expression::empty_chain()),
        Some(PartialExpression::Expression(initial)) => {
            construct_chain_transformations(accumulated, initial)
        }
        _ => error_expected("expression or closing brace", elem_expression),
    }
}

fn construct_chain_transformations(
    accumulated: &mut VecDeque<PartialExpression>,
    initial: Expression,
) -> Result<Expression, AnyError> {
    let mut transformations = Transformations::new();
    loop {
        let elem_operator = accumulated.pop_front();
        match elem_operator {
            Some(PartialExpression::CloseBrace) => {
                return Ok(Expression::chain(Box::new(initial), transformations))
            }
            Some(PartialExpression::Transformation(transformation)) => {
                transformations.push(transformation);
            }
            _ => error_expected("operator or closing brace", elem_operator)?,
        }
    }
}

fn construct_array(accumulated: &mut VecDeque<PartialExpression>) -> Result<Expression, AnyError> {
    let mut elements = Vec::new();
    let mut elem = accumulated.pop_front();
    while let Some(PartialExpression::Expression(e)) = elem {
        elements.push(e);
        elem = accumulated.pop_front()
    }
    if let Some(PartialExpression::CloseBracket) = elem {
        Ok(Expression::StaticList { elements })
    } else {
        error_expected("array end or expression", elem)
    }
}
fn construct_children_types(
    accumulated: &mut VecDeque<PartialExpression>,
) -> Result<PartialExpression, AnyError> {
    let mut types = TypedIdentifiers::new();
    let mut elem = accumulated.pop_front();
    let mut name_opt = None;
    loop {
        match elem {
            Some(PartialExpression::Expression(Expression::Identifier(name))) => {
                if let Some(previous_name) = name_opt {
                    types.push(TypedIdentifier::unknown_type(previous_name));
                }
                name_opt = Some(name)
            }
            Some(PartialExpression::Transformation(Transformation {
                operator: Operator::Type,
                operand: Expression::Type(type_),
            })) => {
                let name = if let Some(previous_name) = name_opt {
                    name_opt = None;
                    previous_name
                } else {
                    "".to_string()
                };
                types.push(TypedIdentifier { name, type_ });
            }
            Some(PartialExpression::CloseParenthesis) => {
                if let Some(previous_name) = name_opt {
                    types.push(TypedIdentifier::unknown_type(previous_name));
                }
                return Ok(PartialExpression::ChildrenTypes(types));
            }
            Some(_) | None => return error_expected("closing parenthesis or expression", elem),
        }
        elem = accumulated.pop_front();
    }
}

fn type_from_expression(expression: Expression) -> Result<Type, AnyError> {
    match expression {
        Expression::Type(a_type) => Ok(a_type),
        Expression::Identifier(type_name) => Ok(Type::simple(type_name)),
        _ => error_expected("type start or type expression", expression),
    }
}

fn finish_construction(
    accumulated: &mut VecDeque<PartialExpression>,
) -> Result<Expression, AnyError> {
    if accumulated.len() <= 1 {
        match accumulated.pop_front() {
            Some(PartialExpression::Expression(e)) => {
                return Ok(e);
            }
            None => Ok(Expression::Nothing),
            Some(v) => {
                accumulated.push_front(v);
                Err(format!("unfinished code: {:?}", accumulated).into())
            }
        }
    } else {
        let error_message = format!("unfinished code: {:?}", accumulated);
        accumulated.push_back(PartialExpression::CloseBrace);
        let e = construct_chain(accumulated)?;
        if !accumulated.is_empty() {
            Err(error_message.into())
        } else {
            Ok(e)
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
    fn test_assignment() {
        assert_eq_ast("function {} = noop", "function {} Fn = noop Op Chain");
    }

    #[test]
    fn test_branch() {
        assert_eq_ast(
            "5 |branch {7} {8}",
            "5 | branch 7 Chain 8 Chain Br Op Chain",
        );
    }
    mod types {
        use super::*;

        #[test]
        fn test_basic_type() {
            let parsed = parse("5 :i64");
            let expected = ast_deserialize("5 :i64() Op Chain").unwrap();
            assert_eq!(parsed.unwrap(), expected);
        }
        #[test]
        fn test_nameless_children_types() {
            let parsed = parse("5 :tuple(:i64 :i64)");
            let expected = ast_deserialize("5 :tuple(:i64() UT :i64() UT) Op Chain").unwrap();
            assert_eq!(parsed.unwrap(), expected);
        }
        #[test]
        fn test_typeless_children_types() {
            let parsed = parse("5 :tuple(x y)");
            let expected = ast_deserialize("5 :tuple(x NU y NU) Op Chain").unwrap();
            assert_eq!(parsed.unwrap(), expected);
        }
        #[test]
        fn test_children_types() {
            let parsed = parse("5 :tuple(x:i64 y:i64)");
            let expected = ast_deserialize("5 :tuple(x :i64() NT y :i64() NT) Op Chain").unwrap();
            assert_eq!(parsed.unwrap(), expected);
        }
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
            assert_eq_ast("function(x) {5}", "function x 5 Chain Fn");
        }
    }

    fn assert_eq_ast(code: &str, ast: &str) {
        let parsed = parse(code);
        let expected = ast_deserialize(ast).unwrap();
        assert_eq!(parsed.unwrap(), expected);
    }
}
