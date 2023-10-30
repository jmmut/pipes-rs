use crate::frontend::expression::Expression;
use crate::frontend::slow_iterative_parser::Parser;
use crate::frontend::lexer::lex;
use crate::AnyError;

pub mod ast;
pub mod expression;
pub mod slow_iterative_parser;
pub mod lexer;

#[cfg(test)]
pub mod recursive_parser;
pub mod fast_iterative_parser;

pub fn lex_and_parse<S: AsRef<str>>(code_text: S) -> Result<Expression, AnyError> {
    let tokens = lex(code_text);
    // let expression = parse(tokens?);
    let expression = Parser::parse_tokens(tokens?);
    expression
}

#[cfg(test)]
mod tests {
    use super::*;
    use expression::Expression;

    #[test]
    fn test_nothing() {
        let expression = lex_and_parse("").unwrap();
        assert_eq!(expression, Expression::Nothing)
    }

    #[test]
    fn test_nothing_braces() {
        let expression = lex_and_parse("{}").unwrap();
        assert_eq!(expression, Expression::Nothing);
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
}

#[cfg(test)]
mod benchmarks {
    use crate::frontend::lexer::lex;
    use crate::frontend::recursive_parser::parse;
    use crate::frontend::{ast, slow_iterative_parser};
    use std::time::Instant;

    #[test]
    fn benchmark_deep() {
        let mut code = "1".to_string();
        let n = 200;
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

        benchmark_rec_iter_ast(code, code_ast);
    }

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

        benchmark_rec_iter_ast(code, code_ast);
    }

    fn benchmark_rec_iter_ast(code: String, code_ast: String) {
        // println!("{}", code);
        // println!("{}", code_ast);
        let tokens_1 = lex(code).unwrap();
        let tokens_2 = tokens_1.clone();
        let tokens_3 = lex(code_ast).unwrap();

        let start = Instant::now();
        let parsed_rec = parse(tokens_1);
        let duration_recursive = Instant::now().duration_since(start);

        let start = Instant::now();
        let parsed_iter = slow_iterative_parser::Parser::parse_tokens(tokens_2);
        let duration_iterative = Instant::now().duration_since(start);

        let start = Instant::now();
        let parsed_ast = ast::deserialize_tokens(tokens_3);
        let duration_ast = Instant::now().duration_since(start);

        println!(
            "recursive: {}, iterative: {},  ast: {}",
            duration_recursive.as_micros(),
            duration_iterative.as_micros(),
            duration_ast.as_micros(),
        );
        if parsed_rec.as_ref().unwrap() != parsed_iter.as_ref().unwrap()
            || parsed_iter.as_ref().unwrap() != parsed_ast.as_ref().unwrap()
        {
            let tree_rec = format!("{:#?}", parsed_rec.unwrap());
            let tree_iter = format!("{:#?}", parsed_iter.unwrap());
            let tree_ast = format!("{:#?}", parsed_ast.unwrap());
            assert_eq!(tree_rec, tree_iter);
            assert_eq!(tree_iter, tree_ast);
        }
    }
}
