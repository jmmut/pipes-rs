use crate::frontend::expression::Expression;
use crate::frontend::lexer::lex;
use crate::frontend::location::SourceCode;
use crate::frontend::parser::parse_tokens;
use crate::AnyError;

pub mod ast;
pub mod expression;
pub mod lexer;
pub mod location;
mod parser;

#[cfg(test)]
mod tests;

#[cfg(test)]
mod benchmarks;

pub fn lex_and_parse<S: AsRef<str>>(code_text: S) -> Result<Expression, AnyError> {
    let tokens = lex(code_text);
    let expression = parse_tokens(tokens?);
    expression
}

pub fn lex_and_parse_source(code_text: &SourceCode) -> Result<Expression, AnyError> {
    lex_and_parse(&code_text.text)
}
