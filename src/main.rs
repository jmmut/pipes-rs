mod lexer;
mod parser;

use crate::lexer::lex;
use crate::parser::{parse, Expression};
use clap::Parser;

pub type AnyError = Box<dyn std::error::Error>;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    code_string: String,
}

fn main() -> Result<(), AnyError> {
    let result = interpret();
    match result {
        Ok(expression) => {
            println!("Result: {:?}", expression);
        }
        Err(e) => {
            println!("Error: {}", e);
        }
    }
    Ok(())
}
fn interpret() -> Result<Expression, AnyError> {
    let Args { code_string } = Args::parse();
    let expression = lex_and_parse(code_string)?;
    Ok(expression)
}

fn lex_and_parse<S: AsRef<str>>(code_text: S) -> Result<Expression, AnyError> {
    let tokens = lex(code_text);
    let expression = parse(&tokens?);
    expression
}

#[cfg(test)]
mod tests {
    use super::*;

    mod interpret {
        use super::*;

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
}
