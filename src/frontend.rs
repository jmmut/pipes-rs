use crate::frontend::expression::Expression;
use crate::frontend::lexer::lex;
use crate::frontend::location::SourceCode;
use crate::frontend::parser::parse_tokens;
use crate::frontend::parser::reverse_iterative_parser::{parse_tokens_cached, Parser};
use crate::frontend::program::Program;
use crate::AnyError;
use std::collections::{HashMap, HashSet};

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

pub fn lex_and_parse_with_identifiers<S: Into<SourceCode>>(
    code_text: S,
    identifiers: HashSet<String>,
) -> Result<Program, AnyError> {
    let tokens = lex(code_text);
    let ast = Parser::new_with_available(None, identifiers, None);
    let expression = parse_tokens_cached(tokens?.tokens, ast);
    expression
}
