use wasm_bindgen::prelude::*;

use crate::common::AnyError;
use crate::evaluate::{GenericValue, Runtime};
use crate::frontend::lex_and_parse;
use crate::frontend::location::SourceCode;
use crate::middleend::typing::{add_types, check_types, put_types};

pub mod common;
pub mod evaluate;
pub mod frontend;
pub mod middleend;

#[wasm_bindgen]
pub fn interpret_in_web(code: &str) -> Box<[JsValue]> {
    match interpret_in_web_fallible(code) {
        Ok((result, output)) => result_and_message_to_jsvalue(result, output),
        Err(err) => Box::new([JsValue::from(-1), JsValue::from(err.to_string())]),
    }
}

fn interpret_in_web_fallible(code: &str) -> Result<(GenericValue, String), AnyError> {
    let source = SourceCode::new_fileless(code.to_string());
    let mut program = lex_and_parse(source)?;

    put_types(&mut program)?;

    let mut print_output = Vec::<u8>::new();
    let read_input: &[u8] = &[];
    let result = Runtime::evaluate(program, read_input, &mut print_output)?;
    Ok((result, String::from_utf8(print_output)?))
}

fn result_and_message_to_jsvalue(result: GenericValue, print_output: String) -> Box<[JsValue; 2]> {
    Box::new([JsValue::from(result), JsValue::from(print_output)])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_interpret_in_web() {
        let (result, output) = interpret_in_web_fallible("3+5 +'0' |print_char").unwrap();
        assert_eq!(result, '8' as i64);
        assert_eq!(output, "8");
    }
}
