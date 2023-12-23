use crate::common::AnyError;
use crate::frontend::parser::reverse_iterative_parser::Parser;

pub fn import(parser: Parser) -> Result<Parser, AnyError> {
    if parser.unresolved_identifiers.is_empty() {
        Ok(parser)
    } else {
        unimplemented!()
    }
}
