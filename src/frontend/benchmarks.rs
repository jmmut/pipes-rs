use crate::common::AnyError;
use crate::frontend::expression::Expression;
use crate::frontend::lexer::{lex, Tokens};
use crate::frontend::{
    ast,
    parser::{recursive_parser, reverse_iterative_parser, slow_iterative_parser},
};
use std::time::{Duration, Instant};

// #[ignore]
#[test]
fn benchmark_deep() {
    let mut code = "1".to_string();
    let n = 135;
    for _ in 0..n {
        code += "+ {[2 {3";
    }
    for _ in 0..n {
        code += "}]#1}";
    }
    let mut code_ast = "1".to_string();
    for _ in 0..n {
        code_ast += "+ [2 3";
    }
    for _ in 0..n {
        code_ast += " Chain ] #1 Op Chain Op";
    }
    code_ast += " Chain";

    benchmark_rec_iter_rev_ast(code, code_ast);
}

#[ignore]
#[test]
fn benchmark_wide() {
    let mut code = "1".to_string();
    let n = 200000;
    for _ in 0..n {
        code += "+ {[2 3]#1}";
    }
    let mut code_ast = "1".to_string();
    for _ in 0..n {
        code_ast += "+ [2 3] #1 Op Chain Op";
    }
    code_ast += " Chain";

    benchmark_rec_iter_rev_ast(code, code_ast);
}

fn benchmark_rec_iter_rev_ast(code: String, code_ast: String) {
    // println!("{}", code);
    // println!("{}", code_ast);
    let tokens_1 = lex(code).unwrap();
    let tokens_2 = tokens_1.clone();
    let tokens_3 = tokens_1.clone();
    let tokens_4 = lex(code_ast).unwrap();

    fn parse(
        tokens: Tokens,
        parse_func: fn(Tokens) -> Result<Expression, AnyError>,
    ) -> (Result<Expression, AnyError>, Duration) {
        let start = Instant::now();
        let parsed = parse_func(tokens);
        let duration_recursive = Instant::now().duration_since(start);
        (parsed, duration_recursive)
    }

    let (parsed_rec, duration_recursive) = parse(tokens_1, recursive_parser::parse);
    let (parsed_iter, duration_iterative) = parse(tokens_2, slow_iterative_parser::parse_tokens);
    let (parsed_rev, duration_rev) = parse(tokens_3, reverse_iterative_parser::parse_tokens);
    let (parsed_ast, duration_ast) = parse(tokens_4, ast::deserialize_tokens);
    println!(
        "recursive: {}, iterative: {}, reverse: {}, ast: {}",
        duration_recursive.as_micros(),
        duration_iterative.as_micros(),
        duration_rev.as_micros(),
        duration_ast.as_micros(),
    );
    if parsed_rec.as_ref().unwrap() != parsed_iter.as_ref().unwrap()
        || parsed_iter.as_ref().unwrap() != parsed_rev.as_ref().unwrap()
        || parsed_rev.as_ref().unwrap() != parsed_ast.as_ref().unwrap()
    {
        let tree_rec = format!("{:#?}", parsed_rec.unwrap());
        let tree_iter = format!("{:#?}", parsed_iter.unwrap());
        let tree_rev = format!("{:#?}", parsed_rev.unwrap());
        let tree_ast = format!("{:#?}", parsed_ast.unwrap());
        assert_eq!(tree_rec, tree_iter);
        assert_eq!(tree_iter, tree_rev);
        assert_eq!(tree_rev, tree_ast);
    }
}
