use wasm_bindgen::prelude::*;

use crate::common::AnyError;
use crate::evaluate::Runtime;
use crate::frontend::lex_and_parse;
use crate::frontend::location::SourceCode;

pub mod common;
pub mod evaluate;
pub mod frontend;
pub mod typing;

#[wasm_bindgen]
pub fn interpret_in_web(code: &str) -> Box<[JsValue]> {
    match interpret_in_web_fallible(code) {
        Ok(result_and_output) => result_and_output,
        Err(err) => Box::new([JsValue::from(-1), JsValue::from(err.to_string())]),
    }
}

fn interpret_in_web_fallible(code: &str) -> Result<Box<[JsValue]>, AnyError> {
    let source = SourceCode::new_fileless(code.to_string());
    let expression = lex_and_parse(source)?;

    let mut print_output = Vec::<u8>::new();
    let read_input: &[u8] = &[];
    let result = Runtime::evaluate(expression, read_input, &mut print_output)?;
    Ok(Box::new([
        JsValue::from(result),
        JsValue::from(String::from_utf8(print_output)?),
    ]))
}
