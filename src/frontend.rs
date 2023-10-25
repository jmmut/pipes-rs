use crate::frontend::lexer::lex;
use crate::frontend::parser::{parse, Expression};
use crate::AnyError;

pub mod lexer;
pub mod parser;

pub fn lex_and_parse<S: AsRef<str>>(code_text: S) -> Result<Expression, AnyError> {
    let tokens = lex(code_text);
    let expression = parse(tokens?);
    expression
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::Expression;

    #[test]
    fn test_nothing() {
        let expression = lex_and_parse("").unwrap();
        assert_eq!(expression, Expression::Nothing)
    }

    #[test]
    fn test_value() {
        let expression = lex_and_parse("57").unwrap();
        assert_eq!(expression, Expression::Value(57))
    }
}