use std::collections::VecDeque;
use std::fmt::{Debug, Display};

use crate::common::{context, err, AnyError};
use crate::frontend::expression::{
    Branch, Chain, Composed, Expression, ExpressionSpan, Function, Loop, Operation, Type,
    TypedIdentifier,
};
use crate::frontend::lexer::{lex, TokenizedSource};
use crate::frontend::location::{SourceCode, Span};
use crate::frontend::parser::reverse_iterative_parser::PartialExpression;
use crate::frontend::program::Program;
use crate::frontend::token::{Keyword, LocatedToken, Operator, OperatorSpan, Token};

// pub struct PartialLocatedExpresion {
//     pe: PartialExpression,
//     pub location: Location,
// }
pub fn ast_deserialize(s: &str) -> Result<Program, AnyError> {
    let tokens = lex(s).unwrap();
    context("AST parser", deserialize_tokens(tokens))
}
pub fn ast_deserialize_source(s: &SourceCode) -> Result<Program, AnyError> {
    ast_deserialize(&s.text)
}
pub fn deserialize_tokens(tokens: TokenizedSource) -> Result<Program, AnyError> {
    let mut accumulated = Vec::new();
    for LocatedToken { token, span } in tokens.tokens {
        match token {
            Token::OpenBracket => accumulated.push(PartialExpression::OpenBracket(span)),
            Token::CloseBracket => construct_list(&mut accumulated)?,
            Token::OpenParenthesis => accumulated.push(PartialExpression::OpenParenthesis(span)),
            Token::CloseParenthesis => construct_type_with_children(&mut accumulated)?,
            Token::OpenBrace => accumulated.push(PartialExpression::OpenBrace(span)),
            Token::CloseBrace => construct_chain(&mut accumulated, span)?,
            Token::Operator(o) => accumulated.push(PartialExpression::Operator(o)),
            Token::Keyword(keyword) => accumulated.push(PartialExpression::Keyword(keyword)),
            Token::Identifier(ident) => match ident.as_str() {
                "Chain" => construct_chain(&mut accumulated, span)?,
                "Loop" => construct_loop(&mut accumulated)?,
                "Op" => construct_operation(&mut accumulated)?,
                "Fn" => construct_function(&mut accumulated)?,
                "Br" => construct_branch(&mut accumulated)?,
                "NT" => construct_named_type(&mut accumulated)?,
                "UT" => construct_unnamed_type(&mut accumulated)?,
                "NU" => construct_name_untyped(&mut accumulated)?,
                // "Type" => construct_simple_type(&mut accumulated)?,
                _ => accumulated.push(PartialExpression::expression_no_span(
                    Expression::Identifier(ident),
                )),
            },
            Token::Number(n) => {
                accumulated.push(PartialExpression::expression_no_span(Expression::Value(n)))
            }
            Token::String(string) => construct_string(string, &mut accumulated, span)?,
        };
    }
    finish_construction(&mut accumulated)
}

fn construct_list(accumulated: &mut Vec<PartialExpression>) -> Result<(), AnyError> {
    let mut expressions = VecDeque::new();
    let mut elem = accumulated.pop();
    while let Some(PartialExpression::Expression(e)) = elem {
        expressions.push_front(e);
        elem = accumulated.pop()
    }
    if let Some(PartialExpression::OpenBracket(_)) = elem {
        accumulated.push(PartialExpression::expression_no_span(
            Expression::StaticList {
                elements: expressions.into_iter().collect::<Vec<_>>(),
            },
        ));
        Ok(())
    } else {
        error_expected("array start or expression", elem.as_ref())
    }
}

fn construct_string(
    string: Vec<u8>,
    accumulated: &mut Vec<PartialExpression>,
    span: Span,
) -> Result<(), AnyError> {
    let pe = crate::frontend::parser::reverse_iterative_parser::construct_string(string, span);
    accumulated.push(pe);
    Ok(())
}

fn construct_type_with_children(accumulated: &mut Vec<PartialExpression>) -> Result<(), AnyError> {
    let mut types = VecDeque::new();
    let mut elem = accumulated.pop();
    while let Some(PartialExpression::TypedIdentifier(typed_identifier)) = elem {
        types.push_front(typed_identifier);
        elem = accumulated.pop();
    }
    if let Some(PartialExpression::OpenParenthesis(span)) = elem {
        elem = accumulated.pop();
        if let Some(PartialExpression::Expression(ExpressionSpan {
            syntactic_type: Expression::Identifier(parent),
            span,
        })) = elem
        {
            let a_type = Type::from(parent, types.into_iter().collect::<Vec<_>>());
            accumulated.push(PartialExpression::expression_no_span(Expression::Type(
                a_type,
            )));
            Ok(())
        } else {
            error_expected("parent type name before parenthesis", elem)
        }
    } else {
        error_expected("opening parenthesis before first child type", elem)
    }
}

fn construct_chain(accumulated: &mut Vec<PartialExpression>, span: Span) -> Result<(), AnyError> {
    let mut transformations = VecDeque::new();

    while let Some(pe) = accumulated.pop() {
        match pe {
            PartialExpression::Operation(t) => transformations.push_front(t),
            PartialExpression::Expression(e) => {
                accumulated.push(PartialExpression::expression_no_span(Expression::Chain(
                    Chain {
                        initial: Some(Box::new(e)),
                        operations: transformations.into_iter().collect::<Vec<_>>(),
                    },
                )));
                return Ok(());
            }
            PartialExpression::OpenBrace(span) => {
                return if transformations.is_empty() {
                    accumulated.push(PartialExpression::expression_no_span(
                        Expression::empty_chain(),
                    ));
                    Ok(())
                } else {
                    error_expected("expression or operation", Some(pe))
                }
            }
            _ => return error_expected("expression or operation", Some(pe)),
        }
    }
    error_expected("expression", None::<PartialExpression>)
}

fn construct_operation(accumulated: &mut Vec<PartialExpression>) -> Result<(), AnyError> {
    let elem = accumulated.pop();
    if let Some(PartialExpression::Expression(operand)) = elem {
        let elem = accumulated.pop();
        if let Some(PartialExpression::Operator(operator)) = elem {
            accumulated.push(PartialExpression::Operation(Operation {
                operator: OperatorSpan::spanless(operator),
                operands: vec![operand],
            }));
            Ok(())
        } else {
            error_expected("operator", elem)
        }
    } else {
        error_expected("operand", elem)
    }
}

fn construct_function(accumulated: &mut Vec<PartialExpression>) -> Result<(), AnyError> {
    let elem = accumulated.pop();
    return if let Some(PartialExpression::Expression(ExpressionSpan {
        syntactic_type: Expression::Chain(chain),
        span,
    })) = elem
    {
        match construct_function_from_chain(accumulated, chain) {
            Ok(function) => {
                accumulated.push(PartialExpression::expression_no_span(Expression::Function(
                    function,
                )));
                Ok(())
            }
            Err((error, _chain)) => Err(error),
        }
    } else {
        error_expected("function body (chain)", elem)
    };
}

pub fn construct_function_from_chain(
    accumulated: &mut Vec<PartialExpression>,
    body: Chain,
) -> Result<Function, (AnyError, Chain)> {
    let elem = accumulated.pop();
    match elem {
        Some(PartialExpression::Keyword(Keyword::Function)) => {
            let parameters = Vec::new();
            Ok(Function { parameters, body })
        }
        Some(PartialExpression::Expression(ExpressionSpan {
            syntactic_type: Expression::Identifier(param),
            span,
        })) => {
            let elem = accumulated.pop();
            match elem {
                Some(PartialExpression::Keyword(Keyword::Function)) => {
                    // TODO: accept typed parameter definition
                    let parameters = vec![TypedIdentifier::any(param)];
                    Ok(Function { parameters, body })
                }
                _ => {
                    let err = Err((anyerror_expected("'function'", elem.as_ref()), body));
                    if let Some(to_push) = elem {
                        accumulated.push(to_push);
                    }
                    err
                }
            }
        }
        _ => {
            let err = Err((
                anyerror_expected("'function' or parameter name", elem.as_ref()),
                body,
            ));
            if let Some(elem) = elem {
                accumulated.push(elem);
            }
            err
        }
    }
}

fn construct_loop(accumulated: &mut Vec<PartialExpression>) -> Result<(), AnyError> {
    let elem = accumulated.pop();
    return if let Some(PartialExpression::Expression(ExpressionSpan {
        syntactic_type: Expression::Chain(chain),
        span,
    })) = elem
    {
        match construct_loop_from_chain(accumulated, chain) {
            Ok(loop_) => {
                accumulated.push(PartialExpression::expression_no_span(Expression::Composed(
                    Composed::Loop(loop_),
                )));
                Ok(())
            }
            Err((error, _chain)) => Err(error),
        }
    } else {
        error_expected("loop body (chain)", elem)
    };
}

pub fn construct_loop_from_chain(
    accumulated: &mut Vec<PartialExpression>,
    body: Chain,
) -> Result<Loop, (AnyError, Chain)> {
    let elem = accumulated.pop();
    match elem {
        Some(PartialExpression::Keyword(Keyword::Loop)) => {
            let elem = TypedIdentifier {
                name: "".to_string(),
                type_: Type::nothing(),
            };
            Ok(Loop {
                iteration_elem: elem,
                body,
            })
        }
        Some(PartialExpression::Expression(ExpressionSpan {
            syntactic_type: Expression::Identifier(param),
            span,
        })) => {
            let elem = accumulated.pop();
            match elem {
                Some(PartialExpression::Keyword(Keyword::Loop)) => {
                    // TODO: accept typed parameter definition
                    let iteration_elem = TypedIdentifier::any(param);
                    Ok(Loop {
                        iteration_elem,
                        body,
                    })
                }
                _ => {
                    let err = Err((anyerror_expected("'loop'", elem.as_ref()), body));
                    if let Some(to_push) = elem {
                        accumulated.push(to_push);
                    }
                    err
                }
            }
        }
        _ => {
            let err = Err((
                anyerror_expected("'loop' or iteration element name", elem.as_ref()),
                body,
            ));
            if let Some(elem) = elem {
                accumulated.push(elem);
            }
            err
        }
    }
}

fn construct_branch(accumulated: &mut Vec<PartialExpression>) -> Result<(), AnyError> {
    let elem = accumulated.pop();
    if let Some(PartialExpression::Expression(ExpressionSpan {
        syntactic_type: Expression::Chain(no),
        span,
    })) = elem
    {
        let elem = accumulated.pop();
        if let Some(PartialExpression::Expression(ExpressionSpan {
            syntactic_type: Expression::Chain(yes),
            span,
        })) = elem
        {
            let elem = accumulated.pop();
            if let Some(PartialExpression::Keyword(Keyword::Branch)) = elem {
                accumulated.push(PartialExpression::expression_no_span(Expression::Composed(
                    Composed::Branch(Branch { yes, no }),
                )));
                Ok(())
            } else {
                error_expected("'branch' keyword", elem)
            }
        } else {
            error_expected("branch 'if-then' case (chain)", elem)
        }
    } else {
        error_expected("branch 'else' case (chain)", elem)
    }
}

fn construct_named_type(accumulated: &mut Vec<PartialExpression>) -> Result<(), AnyError> {
    let elem = accumulated.pop();
    if let Some(PartialExpression::Expression(ExpressionSpan {
        syntactic_type: Expression::Type(type_),
        span,
    })) = elem
    {
        let elem = accumulated.pop();
        if let Some(PartialExpression::Operator(Operator::Type)) = elem {
            let elem = accumulated.pop();
            if let Some(PartialExpression::Expression(ExpressionSpan {
                syntactic_type: Expression::Identifier(name),
                span,
            })) = elem
            {
                accumulated.push(PartialExpression::TypedIdentifier(TypedIdentifier {
                    name,
                    type_,
                }));
                Ok(())
            } else {
                error_expected("child type name before operator type ':'", elem)
            }
        } else {
            error_expected("operator type ':' after type name", elem)
        }
    } else {
        error_expected("type after type operator ':'", elem)
    }
}
fn construct_unnamed_type(accumulated: &mut Vec<PartialExpression>) -> Result<(), AnyError> {
    let elem = accumulated.pop();
    if let Some(PartialExpression::Expression(ExpressionSpan {
        syntactic_type: Expression::Type(type_),
        span,
    })) = elem
    {
        let elem = accumulated.pop();
        if let Some(PartialExpression::Operator(Operator::Type)) = elem {
            accumulated.push(PartialExpression::TypedIdentifier(
                TypedIdentifier::nameless(type_),
            ));
            Ok(())
        } else {
            error_expected("operator type ':' before type", elem)
        }
    } else {
        error_expected("type after type operator ':'", elem)
    }
}

fn construct_name_untyped(accumulated: &mut Vec<PartialExpression>) -> Result<(), AnyError> {
    let elem = accumulated.pop();
    if let Some(PartialExpression::Expression(ExpressionSpan {
        syntactic_type: Expression::Identifier(name),
        span,
    })) = elem
    {
        accumulated.push(PartialExpression::TypedIdentifier(TypedIdentifier::any(
            name,
        )));
        Ok(())
    } else {
        error_expected("child type name", elem)
    }
}

fn finish_construction(accumulated: &mut Vec<PartialExpression>) -> Result<Program, AnyError> {
    let main = finish_construction_expression(accumulated)?;
    Ok(Program::new(main))
}
fn finish_construction_expression(
    accumulated: &mut Vec<PartialExpression>,
) -> Result<ExpressionSpan, AnyError> {
    if accumulated.len() <= 1 {
        match accumulated.pop() {
            Some(PartialExpression::Expression(e)) => {
                return Ok(e);
            }
            None => return Ok(ExpressionSpan::new_spanless(Expression::Nothing)),
            Some(v) => {
                accumulated.push(v);
            }
        }
    }
    err(format!("unfinished code: {:?}", accumulated))
}

pub fn error_expected<T: Display, R, S: AsRef<str>>(
    expected: S,
    actual: Option<T>,
) -> Result<R, AnyError> {
    Err(anyerror_expected(expected, actual))
}
pub fn anyerror_expected<T: Display, S: AsRef<str>>(expected_: S, actual: Option<T>) -> AnyError {
    expected(expected_.as_ref(), actual).into()
}
pub fn expected<T: Display, S: AsRef<str>>(expected: S, actual: Option<T>) -> String {
    if let Some(actual) = actual.as_ref() {
        format!("expected {} but was '{}'", expected.as_ref(), actual)
    } else {
        format!("expected {} but was None", expected.as_ref())
    }
}
#[cfg(test)]
mod tests {
    use crate::frontend::lex_and_parse;

    use super::*;

    #[test]
    fn test_nothing() {
        let ast = ast_deserialize("{}").unwrap();
        assert_eq!(ast, Program::new_raw(Expression::empty_chain()));
        let ast = ast_deserialize("").unwrap();
        assert_eq!(ast, Program::new_raw(Expression::Nothing));
    }
    #[test]
    fn test_braced_value() {
        let ast = ast_deserialize("5 Chain").unwrap();
        let code = lex_and_parse("{5}").unwrap();
        assert_eq!(ast, code);
    }
    #[test]
    fn test_array() {
        ast_deserialize("[1 2 3]").expect("should parse");
        ast_deserialize("1 2 3]").expect_err("should NOT parse");
    }
    #[test]
    fn test_complex() {
        let ast = ast_deserialize("[5 +7 Op |print_char Op Chain 8]").unwrap();
        let code = lex_and_parse("[ {5 +7 |print_char}  8 ]").unwrap();
        assert_eq!(ast, code);
    }

    #[test]
    fn test_chain() {
        ast_deserialize("Chain").expect_err("should NOT parse");
        ast_deserialize("; Chain").expect_err("should NOT parse");
        ast_deserialize(";").expect_err("should NOT parse");
        ast_deserialize("{ 5 Chain").expect_err("should NOT parse");
        ast_deserialize("{ + 5 Op Chain").expect_err("should NOT parse");
        ast_deserialize("{ + 5 Op Chain").expect_err("should NOT parse");
    }
    #[test]
    fn test_types() {
        ast_deserialize("5 :i64() Op Chain").expect("should parse");
        ast_deserialize("[] :i64() Op Chain").expect("should parse");
        ast_deserialize("5 :tuple(:i64() UT) Op Chain").expect("should parse");
        ast_deserialize("5 :tuple(x :i64() NT) Op Chain").expect("should parse");
        ast_deserialize("5 :tuple(x NU) Op Chain").expect("should parse");
        // ast_deserialize("5 :tuple[i64 i64] Type Chain").expect("should parse");
        // ast_deserialize("5 :tuple[:tuple[:i64 Type] Type :i64 Type] Type Chain").expect("should parse");
        // ast_deserialize("5 [[i64:]tuple: i64:]tuple: Chain").expect("should parse");
        ast_deserialize("[] :tuple(:tuple(:i64() UT) UT y NU) Op Chain").expect("should parse");
        // ast_deserialize("5 [[i64 n:]tuple ns: i64 outer:]tuple all: Chain").expect("should parse");

        ast_deserialize("tuple x NU y NU)").expect_err("should NOT parse");
        ast_deserialize("(x NU y NU)").expect_err("should NOT parse");
        ast_deserialize("tuple(:i64() NT)").expect_err("should NOT parse");
        ast_deserialize("tuple(i64() NT)").expect_err("should NOT parse");
        ast_deserialize("tuple(x NT)").expect_err("should NOT parse");
        ast_deserialize("tuple(i64() UT)").expect_err("should NOT parse");
        ast_deserialize("tuple(: UT)").expect_err("should NOT parse");
        ast_deserialize("tuple(NU)").expect_err("should NOT parse");
    }

    #[test]
    fn test_function() {
        ast_deserialize("function { } Fn").expect("should parse");
        ast_deserialize("function 5 Chain Fn").expect("should parse");
        ast_deserialize("function 5+7 Op Chain Fn").expect("should parse");
        ast_deserialize("function x 5 Chain Fn").expect("should parse");

        ast_deserialize("function x y 5 Chain Fn").expect_err("should NOT parse");
        ast_deserialize("function x Fn").expect_err("should NOT parse");
    }
    #[test]
    fn test_branch() {
        ast_deserialize("branch 3 Chain 5 Chain Br").expect("should parse");

        ast_deserialize("3 Chain 5 Chain Br").expect_err("should NOT parse");
        ast_deserialize("branch 3 Chain Br").expect_err("should NOT parse");
        ast_deserialize("branch 3 Chain 5 Br").expect_err("should NOT parse");
    }

    #[test]
    fn test_string() {
        ast_deserialize(r#""asdf" #1 Op Chain"#).expect("should parse");
    }
}
