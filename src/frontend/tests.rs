use super::*;
use crate::frontend::ast::ast_deserialize;
use crate::frontend::expression::Expression;

#[test]
fn test_nothing() {
    let expression = lex_and_parse("").unwrap();
    assert_eq!(expression, Expression::Nothing)
}

#[test]
fn test_nothing_braces() {
    let expression = lex_and_parse("{}").unwrap();
    assert_eq!(expression, Expression::empty_chain());
    lex_and_parse("[{}]").expect("should parse (maybe doesn't evaluate)");
    lex_and_parse("[5 {}]").expect("should parse (maybe doesn't evaluate)");
    lex_and_parse("{[]}").expect("should parse (maybe doesn't evaluate)");
    lex_and_parse("{[5]}").expect("should parse (maybe doesn't evaluate)");
    // lex_and_parse("[]#{}").expect("should parse (maybe doesn't evaluate)");
}

#[test]
fn test_value() {
    let expression = lex_and_parse("57").unwrap();
    assert_eq!(expression, Expression::Value(57))
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
    let expected = Expression::empty_chain();
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
    let parsed = lex_and_parse("[ {5 +7 |parse_char}  8 ]");
    let expected = ast_deserialize("[ 5 +7 Op |parse_char Op Chain 8 ]").unwrap();
    assert_eq!(parsed.unwrap(), expected);
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
    assert_eq_ast("loop(x) {5}", "loop x 5 Chain Loop");
}
fn assert_eq_ast(code: &str, ast: &str) {
    let parsed = lex_and_parse(code);
    let expected = ast_deserialize(ast).unwrap();
    assert_eq!(parsed.unwrap(), expected);
}
