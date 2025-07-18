use sources::lexer::lex;
use sources::location::SourceCode;
use std::collections::HashSet;
use std::path::PathBuf;

use crate::common::unwrap_display;
use crate::frontend::expression::Type;
use crate::frontend::parser::reverse_iterative_parser::{parse_tokens_cached, Parser};
use crate::frontend::parser::root::get_project_root;
use crate::frontend::parser::{nodes_to_expression, parse_tokens, reverse_iterative_parser};
use crate::frontend::program::Program;
use crate::frontend::sources::lexer::lex_with_eof;
use crate::AnyError;

pub mod expression;
pub mod parser;
pub mod program;

pub mod sources;
#[cfg(test)]
pub mod tests;

//#[cfg(test)]
//mod benchmarks;

pub fn lex_and_parse<S: Into<SourceCode>>(code_text: S) -> Result<Program, AnyError> {
    let expression = if true {
        let tokens = lex(code_text);
        parse_tokens(tokens?)
    } else {
        let tokens = lex_with_eof(code_text);
        nodes_to_expression::parse_tokens(tokens?)
    };
    expression
}

pub fn lex_and_parse_with_identifiers<S: Into<SourceCode>>(
    code_text: S,
    identifiers: HashSet<String>,
) -> Result<Program, AnyError> {
    let tokens = lex(code_text)?;
    let root = get_project_root(&None, &Some(PathBuf::from("./")));
    let ast = Parser::new_with_available(tokens.source_code, identifiers, root.ok());
    let expression = parse_tokens_cached(tokens.tokens, ast);
    expression
}

pub fn parse_type(code: &str) -> Type {
    let tokens = unwrap_display(lex(code));
    unwrap_display(reverse_iterative_parser::parse_type(tokens))
}
