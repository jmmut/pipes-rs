use crate::common::AnyError;
use clap::Parser;
use std::path::PathBuf;

use crate::evaluate::Runtime;
use crate::frontend::ast::Ast;
use crate::frontend::lex_and_parse;

mod common;
mod evaluate;
mod frontend;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// inline pipes code to be interpreted. Either this or the input file must be provided
    code_string: Option<String>,

    /// input file with pipes code to interpret. Either this or the code string must be provided
    #[arg(short, long)]
    input_file: Option<PathBuf>,

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
        input_file,
        prettify,
        alternative,
    } = Args::parse();
    let code_string = read_input(code_string, input_file)?;
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

fn read_input(
    code_string: Option<String>,
    input_file: Option<PathBuf>,
) -> Result<String, AnyError> {
    if code_string.is_some() && input_file.is_some() {
        Err("Only the code string or the input file should be provided")?
    } else if let Some(code) = code_string {
        Ok(code)
    } else if let Some(file) = input_file {
        Ok(std::fs::read_to_string(file)?)
    } else {
        Err("Either the code string or the input file should be provided")?
    }
}
