use clap::Parser;

use crate::evaluate::evaluate;
use crate::frontend::lex_and_parse;

mod evaluate;
mod frontend;
mod lexer;
mod parser;

pub type AnyError = Box<dyn std::error::Error>;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    code_string: String,
}

fn main() -> Result<(), AnyError> {
    let result = interpret();
    match result {
        Ok(()) => {}
        Err(e) => {
            println!("Error: {}", e);
        }
    }
    Ok(())
}
fn interpret() -> Result<(), AnyError> {
    let Args { code_string } = Args::parse();
    let expression = lex_and_parse(code_string)?;
    println!("Expression: {:?}", expression);
    let result = evaluate(expression)?;
    println!("{}", result);
    Ok(())
}
