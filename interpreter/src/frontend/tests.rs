use std::path::PathBuf;

use crate::common::unwrap_display;
use crate::frontend::expression::{
    Branch, Browse, Chain, Composed, Expression, ExpressionSpan, Expressions, Operation,
    TypedIdentifier,
};
use crate::frontend::sources::token::Operator::{Add, Assignment, Call};
use crate::frontend::sources::token::{Operator, OperatorSpan};

use super::*;

pub fn val(value: i64) -> Expression {
    Expression::Value(value)
}
pub fn ident(name: &str) -> Expression {
    Expression::Identifier(name.to_string())
}
pub fn chain_init(initial: Expression, operations: &[Operation]) -> Expression {
    Expression::Chain(raw_chain_init(initial, operations))
}
pub fn chain(operations: &[Operation]) -> Expression {
    Expression::Chain(raw_chain(operations))
}
fn raw_chain_init(initial: Expression, operations: &[Operation]) -> Chain {
    Chain::new_initial(
        ExpressionSpan::new_spanless(initial),
        operations.into_iter().cloned().collect(),
    )
}
fn raw_chain(operations: &[Operation]) -> Chain {
    Chain::new(operations.into_iter().cloned().collect())
}
impl From<i64> for Chain {
    fn from(value: i64) -> Self {
        raw_chain_init(val(value), &[])
    }
}
impl From<Expression> for Chain {
    fn from(initial: Expression) -> Self {
        raw_chain_init(initial, &[])
    }
}

pub fn op(operator: Operator, operands: impl Into<Vec<Expression>>) -> Operation {
    Operation::several_no_sem_type(
        OperatorSpan::spanless(operator),
        to_expr_span_vec(&operands.into()),
    )
}
impl From<Expression> for Vec<Expression> {
    fn from(expr: Expression) -> Self {
        vec![expr]
    }
}
fn to_expr_span_vec(expressions: &[Expression]) -> Expressions {
    expressions
        .into_iter()
        .map(|e| ExpressionSpan::new_spanless(e.clone()))
        .collect()
}

fn type_(name: &str) -> Expression {
    Expression::Type(Type::simple(name))
}
fn nested_type(parent: &str, children_names_and_types: &[(&str, &str)]) -> Expression {
    Expression::Type(Type::children(
        parent,
        children_names_and_types
            .into_iter()
            .map(|name_and_type| (*name_and_type).into())
            .collect(),
    ))
}

impl From<(&str, &str)> for TypedIdentifier {
    fn from((name, type_name): (&str, &str)) -> Self {
        TypedIdentifier::new(name.to_string(), Type::simple(type_name.to_string()))
    }
}

fn list(expressions: &[Expression]) -> Expression {
    Expression::list(to_expr_span_vec(expressions))
}
fn func(parameters: &[(&str, &str)], body: impl Into<Chain>) -> Expression {
    Expression::function_any_return(
        parameters.into_iter().map(|p| (*p).into()).collect(),
        body.into(),
    )
}
fn func_ret(
    parameters: &[(&str, &str)],
    returned: (&str, &str),
    body: impl Into<Chain>,
) -> Expression {
    Expression::function(
        parameters.into_iter().map(|p| (*p).into()).collect(),
        returned.into(),
        body.into(),
    )
}

fn branch(yes: impl Into<Chain>, no: impl Into<Chain>) -> Expression {
    Expression::Composed(Composed::Branch(Branch {
        yes: yes.into(),
        no: no.into(),
    }))
}

fn assert_expr_eq(code: &str, expected: Expression) {
    let program = unwrap_display(lex_and_parse(code));
    let actual = program.main.take().0;
    assert_exprs_eq(actual, expected);
}

pub fn assert_exprs_eq(actual: Expression, expected: Expression) {
    assert_eq!(
        actual, expected,
        "\ndisplayed left:  {}\ndisplayed right: {}",
        actual, expected
    );
}

fn assert_code_eq(code: &str, expected_code: &str) {
    let expected_program = lex_and_parse(expected_code).unwrap();
    let expected = expected_program.take().0.syntactic_type;
    assert_expr_eq(code, expected);
}

#[test]
fn test_nothing() {
    unwrap_display(lex_and_parse(";"));
    unwrap_display(lex_and_parse("{;}"));
    assert_expr_eq("none", Expression::Nothing);
    // assert_expr_eq("none", chain_init(Expression::Nothing, &[]));
}

#[test]
fn test_nothing_braces() {
    assert_expr_eq("", Expression::empty_chain());
    assert_expr_eq("{}", Expression::empty_chain());
    unwrap_display(lex_and_parse("[{}]"));
    unwrap_display(lex_and_parse("[5 {}]"));
    unwrap_display(lex_and_parse("{[]}"));
    unwrap_display(lex_and_parse("{[5]}"));
    unwrap_display(lex_and_parse("[]#{}"));
}

#[test]
fn test_value() {
    assert_expr_eq("57", val(57));
    // assert_expr_eq("57", chain_init(val(57), &[]));
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
    unwrap_display(lex_and_parse("5"));
    unwrap_display(lex_and_parse("{5}"));
    unwrap_display(lex_and_parse("{{5}}"));
}

#[test]
fn test_final_semicolon() {
    assert_code_eq("4;", "{4;{}}")
}
#[test]
fn test_final_semicolon_in_chain() {
    assert_code_eq("4 + {{5;} ;3}", "4 + {{5;{}} ;3}");
}
#[test]
fn test_final_semicolon_in_simple_chain() {
    assert_code_eq("{5;}", "{5;{}}");
}

#[test]
fn test_chained_value() {
    assert_expr_eq("{5}", chain_init(val(5), &[]));
}

#[test]
fn test_operation_chain() {
    assert_expr_eq("{+5}", chain(&[op(Add, val(5))]));
}
#[test]
fn test_chain() {
    assert_expr_eq(
        "{5 +7 +8}",
        chain_init(val(5), &[op(Add, val(7)), op(Add, val(8))]),
    );
}

#[test]
fn test_complex() {
    assert_expr_eq(
        "[ {5 +7 |print_char}  8 ]",
        list(&[
            chain_init(val(5), &[op(Add, val(7)), op(Call, ident("print_char"))]),
            val(8),
        ]),
    );
}

#[test]
fn test_several_operands() {
    assert_expr_eq(
        "3 |print_char 4 5",
        chain_init(val(3), &[op(Call, &[ident("print_char"), val(4), val(5)])]),
    );
}

#[test]
fn test_unfinished() {
    lex_and_parse("5+").expect_err("should fail");
}

#[test]
fn test_assignment() {
    assert_expr_eq(
        "function {} = noop",
        chain_init(func(&[], Chain::empty()), &[op(Assignment, ident("noop"))]),
    );
}

#[test]
fn test_branch() {
    assert_expr_eq(
        "5 |branch {7} {8}",
        chain_init(val(5), &[op(Call, branch(7, 8))]),
    );
}

mod types {
    use super::*;

    #[test]
    fn test_basic_type() {
        assert_expr_eq(
            "5 :i64",
            chain_init(val(5), &[op(Operator::Type, type_("i64"))]),
        );
    }
    #[test]
    fn test_nameless_children_types() {
        assert_expr_eq(
            "5 :tuple(:i64 :i64)",
            chain_init(
                val(5),
                &[op(
                    Operator::Type,
                    nested_type("tuple", &[("", "i64"), ("", "i64")]),
                )],
            ),
        );
    }
    #[test]
    fn test_typeless_children_types() {
        assert_expr_eq(
            "5 :tuple(x y)",
            chain_init(
                val(5),
                &[op(
                    Operator::Type,
                    nested_type("tuple", &[("x", "any"), ("y", "any")]),
                )],
            ),
        );
    }
    #[test]
    fn test_children_types() {
        assert_expr_eq(
            "5 :tuple(x:i64 y:i64)",
            chain_init(
                val(5),
                &[op(
                    Operator::Type,
                    nested_type("tuple", &[("x", "i64"), ("y", "i64")]),
                )],
            ),
        );
    }
}

mod function {
    use super::*;

    #[test]
    fn test_empty_function() {
        assert_expr_eq("function {}", func(&[], Chain::empty()));
    }
    #[test]
    fn test_function_value() {
        assert_expr_eq("function {5}", func(&[], 5));
        assert_expr_eq("function() {5}", func(&[], 5));
    }
    #[test]
    fn test_function_arg() {
        assert_expr_eq("function(x) {5}", func(&[("x", "any")], 5));
    }
    #[test]
    fn test_function_return() {
        assert_expr_eq(
            "function(a :i64)(b :i64) {a}",
            func_ret(&[("a", "i64")], ("b", "i64"), ident("a")),
        );
    }
}

#[test]
fn test_loop() {
    assert_expr_eq(
        "browse(x) {5}",
        Expression::Composed(Composed::Browse(Browse {
            iteration_elem: ("x", "any").into(),
            body: 5.into(),
        })),
    );
}

#[test]
fn test_import() {
    let main_path = PathBuf::from("../pipes_programs/demos/reusing_functions.pipes");
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
    assert_eq!(
        parsed.sources.get_main().file,
        Some(main_path.canonicalize().unwrap())
    );
    assert_eq!(
        parsed.sources.keys().collect::<Vec<_>>(),
        vec!["some_namespace/reusable_functions.pipes"]
    )
}

#[test]
fn test_import_core() {
    let code = "[1 2 3 1 2 1] |array/count 1";
    unwrap_display(lex_and_parse(code));
}

#[test]
fn test_import_second_operand() {
    let code = "2 |function(a b) {a +b} nonexisting_var";
    lex_and_parse(code).expect_err("should fail but was");
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

#[test]
fn test_struct() {
    unwrap_display(lex_and_parse("public tuple(x :i64  y :i64) =Coord"));
    unwrap_display(lex_and_parse(
        "public tuple(x :i64  y :i64) =Coord ; function (c :Coord) { c }",
    ));
    unwrap_display(lex_and_parse(
        "public tuple(x :i64  y :i64) =Coord ; [3 5] |function (c :Coord) { c }",
    ));
}

#[test]
fn test_field() {
    assert_expr_eq(
        "[] .x",
        chain_init(list(&[]), &[op(Operator::Field, ident("x"))]),
    );
}

#[test]
fn test_public() {
    // this should be ok
    // public function() {} =some_func
    //
    // what should this do? store nothing in some_func?? otherwise we are breaking the no-precedence rule
    // {} |public function() {} =some_func
}
