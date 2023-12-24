use crate::frontend::lexer::lex;
use crate::frontend::location::SourceCode;
use crate::frontend::parser::parse_tokens;
use crate::frontend::program::Program;
use crate::AnyError;

pub mod ast;
pub mod expression;
pub mod lexer;
pub mod location;
mod parser;
pub mod program;

#[cfg(test)]
mod tests;

#[cfg(test)]
mod benchmarks;

pub fn lex_and_parse<S: Into<SourceCode>>(code_text: S) -> Result<Program, AnyError> {
    let tokens = lex(code_text);
    let expression = parse_tokens(tokens?);
    expression
}
