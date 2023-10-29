use crate::frontend::lexer::lex;
use crate::frontend::recursive_parser::{parse, Expression};
use crate::AnyError;

pub mod iterative_parser;
pub mod lexer;
pub mod recursive_parser;

pub fn lex_and_parse<S: AsRef<str>>(code_text: S) -> Result<Expression, AnyError> {
    let tokens = lex(code_text);
    let expression = parse(tokens?);
    expression
}

#[cfg(test)]
mod tests {
    use super::*;
    use recursive_parser::Expression;

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
        lex_and_parse("[]#{}").expect("should parse (maybe doesn't evaluate)");
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
