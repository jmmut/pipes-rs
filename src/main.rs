use crate::common::AnyError;
use clap::Parser;

use crate::evaluate::Runtime;
use crate::frontend::ast::Ast;
use crate::frontend::lex_and_parse;

mod common;
mod evaluate;
mod frontend;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    code_string: String,

    #[arg(short, long)]
    prettify: bool,

    /// alternative syntax, like `[ {5 +7, |print_char,} 8 ]`
    #[arg(short, long)]
    alternative: bool,
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
        alternative,
    } = Args::parse();

    let expression = if alternative {
        Ast::deserialize(&code_string)?
    } else {
        lex_and_parse(code_string)?
    };

    if prettify {
        println!("Expression: {:#?}", expression);
    } else {
        println!("Expression: {:?}", expression);
    }

    let result = Runtime::evaluate(expression)?;
    println!("{}", result);
    Ok(())
}
