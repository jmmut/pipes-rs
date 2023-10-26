use clap::Parser;

use crate::evaluate::Runtime;
use crate::frontend::lex_and_parse;

mod evaluate;
mod frontend;

pub type AnyError = Box<dyn std::error::Error>;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    code_string: String,

    #[arg(short, long)]
    prettify: bool,
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
    let Args {
        code_string,
        prettify,
    } = Args::parse();
    let expression = lex_and_parse(code_string)?;
    if prettify {
        println!("Expression: {:#?}", expression);
    } else {
        println!("Expression: {:?}", expression);
    }
    let result = Runtime::evaluate(expression)?;
    println!("{}", result);
    Ok(())
}
