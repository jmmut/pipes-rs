use crate::common::unwrap_display;
use std::path::PathBuf;

use crate::frontend::ast::ast_deserialize;
use crate::frontend::expression::Expression::Value;
use crate::frontend::expression::{
    Chain, Expression, ExpressionSpan, Expressions, Operation, Operations,
};
use crate::frontend::token::Operator::Call;
use crate::frontend::token::{Operator, OperatorSpan};

use super::*;

fn chain(initial: Expression, operations: Operations) -> Expression {
    Expression::chain(Box::new(ExpressionSpan::new_spanless(initial)), operations)
}
fn op(operator: Operator, expressions: Vec<Expression>) -> Operation {
    Operation::several(
        OperatorSpan::spanless(operator),
        expressions
            .into_iter()
            .map(|e| ExpressionSpan::new_spanless(e))
            .collect(),
    )
}
fn ident(name: &str) -> Expression {
    Expression::Identifier(name.to_string())
}

fn mock_program(expression: Expression) -> Program {
    Program::new(ExpressionSpan::new_spanless(expression))
}

#[test]
fn test_nothing() {
    let expression = lex_and_parse("").unwrap();
    assert_eq!(expression, Program::new_raw(Expression::Nothing))
}

#[test]
fn test_nothing_braces() {
    let expression = lex_and_parse("{}").unwrap();
    assert_eq!(expression, Program::new_raw(Expression::empty_chain()));
    lex_and_parse("[{}]").expect("should parse (maybe doesn't evaluate)");
    lex_and_parse("[5 {}]").expect("should parse (maybe doesn't evaluate)");
    lex_and_parse("{[]}").expect("should parse (maybe doesn't evaluate)");
    lex_and_parse("{[5]}").expect("should parse (maybe doesn't evaluate)");
    // lex_and_parse("[]#{}").expect("should parse (maybe doesn't evaluate)");
}

#[test]
fn test_value() {
    let expression = lex_and_parse("57").unwrap();
    assert_eq!(expression, Program::new_raw(Expression::Value(57)));
}

#[test]
fn test_extra_brace() {
    lex_and_parse("5}").expect_err("should fail");
    lex_and_parse("{5}}").expect_err("should fail");
    lex_and_parse("{5").expect_err("should fail");
    lex_and_parse("{{5}").expect_err("should fail");
}

#[test]
fn test_correct_braces() {
    lex_and_parse("5").expect("should work");
    lex_and_parse("{5}").expect("should work");
    lex_and_parse("{{5}}").expect("should work");
}

#[test]
fn test_empty_chain() {
    let ast = "{}";
    let expected = Program::new_raw(Expression::empty_chain());
    assert_eq!(lex_and_parse(ast).unwrap(), expected);
}

#[test]
fn test_final_semicolon() {
    let parsed = lex_and_parse("4;");
    let expected = ast_deserialize("4 ;{} Op Chain").unwrap();
    assert_eq!(parsed.unwrap(), expected);
}
#[test]
fn test_final_semicolon_in_chain() {
    let parsed = lex_and_parse("4 + {{5;} ;3}");
    let expected = ast_deserialize("4 + 5 ;{} Op Chain ;3 Op Chain Op Chain").unwrap();
    assert_eq!(parsed.unwrap(), expected);
}
#[test]
fn test_final_semicolon_in_simple_chain() {
    let parsed = lex_and_parse("{5;}");
    let expected = ast_deserialize("5 ;{} Op Chain").unwrap();
    assert_eq!(parsed.unwrap(), expected);
}

#[test]
fn test_chained_value() {
    let parsed = lex_and_parse("{5}");
    let expected = ast_deserialize("5 Chain").unwrap();
    assert_eq!(parsed.unwrap(), expected);
}

#[test]
fn test_chain() {
    let parsed = lex_and_parse("{5 +7 +8}");
    let expected = ast_deserialize("5 +7 Op +8 Op Chain").unwrap();
    assert_eq!(parsed.unwrap(), expected);
}

#[test]
fn test_complex() {
    let parsed = lex_and_parse("[ {5 +7 |print_char}  8 ]");
    let expected = ast_deserialize("[ 5 +7 Op |print_char Op Chain 8 ]").unwrap();
    assert_eq!(parsed.unwrap(), expected);
}

#[test]
fn test_operators() {
    let code = "3 |print_char 4 5";
    let program = unwrap_display(lex_and_parse(code));
    assert_eq!(program.main().to_string(), code);
}

#[test]
fn test_unfinished() {
    lex_and_parse("5+").expect_err("should fail");
    lex_and_parse("{+5}").expect_err("should fail");
    lex_and_parse(";").expect_err("should fail");
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
        let parsed = lex_and_parse("5 :i64");
        let expected = ast_deserialize("5 :i64() Op Chain").unwrap();
        assert_eq!(parsed.unwrap(), expected);
    }
    #[test]
    fn test_nameless_children_types() {
        let parsed = lex_and_parse("5 :tuple(:i64 :i64)");
        let expected = ast_deserialize("5 :tuple(:i64() UT :i64() UT) Op Chain").unwrap();
        assert_eq!(parsed.unwrap(), expected);
    }
    #[test]
    fn test_typeless_children_types() {
        let parsed = lex_and_parse("5 :tuple(x y)");
        let expected = ast_deserialize("5 :tuple(x NU y NU) Op Chain").unwrap();
        assert_eq!(parsed.unwrap(), expected);
    }
    #[test]
    fn test_children_types() {
        let parsed = lex_and_parse("5 :tuple(x:i64 y:i64)");
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
        assert_eq_ast("function   {5}", "function 5 Chain Fn");
        assert_eq_ast("function() {5}", "function 5 Chain Fn");
    }
    #[test]
    fn test_function_arg() {
        assert_eq_ast("function(x) {5}", "function x 5 Chain Fn");
    }
}

#[test]
fn test_loop() {
    assert_eq_ast("browse(x) {5}", "browse x 5 Chain Loop");
}
fn assert_eq_ast(code: &str, ast: &str) {
    let parsed = lex_and_parse(code);
    let expected = ast_deserialize(ast).unwrap();
    assert_eq!(parsed.unwrap(), expected);
}

#[test]
fn test_import() {
    let main_path = PathBuf::from("./pipes_programs/demos/reusing_functions.pipes");
    let code = SourceCode::new(main_path.clone()).unwrap();
    let parsed = unwrap_display(lex_and_parse(code));
    assert_eq!(
        parsed
            .identifiers
            .contains_key("some_namespace/reusable_functions/increment"),
        true,
        "actual: {:?}",
        parsed.identifiers.keys()
    );
    assert_eq!(parsed.main_source.file, Some(main_path));
    assert_eq!(
        parsed.sources.keys().collect::<Vec<_>>(),
        vec!["some_namespace/reusable_functions.pipes"]
    )
}

#[test]
fn test_undefined_in_else() {
    lex_and_parse("[]|browse_or(e) {} {e}").expect_err("should fail");
    lex_and_parse("[]|times_or(e) {} {e}").expect_err("should fail");
    lex_and_parse("0|something(e) {} {e}").expect_err("should fail");
}

#[test]
fn test_no_empty_cast() {
    // this might change to allow backwards type inference
    lex_and_parse("[] |cast").expect_err("should fail");
}
