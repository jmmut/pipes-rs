
pub mod common;
pub mod evaluate;
pub mod frontend;


use wasm_bindgen::prelude::*;
use crate::common::AnyError;
use crate::evaluate::{Runtime, NOTHING};

use crate::frontend::lex_and_parse_source;
use crate::frontend::location::SourceCode;

#[wasm_bindgen]
pub fn interpret_in_web(code: &str) -> JsValue {
    match interpret_in_web_fallible(code) {
        Ok(js_string) => js_string,
        Err(err) => JsValue::from_str(&err.to_string()),
    }
}

fn interpret_in_web_fallible(code: &str) -> Result<JsValue, AnyError> {
    let source = SourceCode::new_fileless(code.to_string());
    let expression = lex_and_parse_source(&source)?;

    let mut print_output = Vec::<u8>::new();
    let read_input :&[u8] = &[];
    let result = Runtime::evaluate(expression, read_input, &mut print_output)?;
    let formatted_result = if result != NOTHING {
        format!("{}\nResult: {}", String::from_utf8(print_output)?, result)
    } else {
        String::from_utf8(print_output)?
    };
    Ok(JsValue::from_str(&formatted_result))
}
