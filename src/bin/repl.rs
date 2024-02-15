use clap::Parser;
use pipes_rs::common::AnyError;
use pipes_rs::evaluate::{Runtime, NOTHING};
use pipes_rs::frontend::lex_and_parse;
use pipes_rs::frontend::location::SourceCode;
use pipes_rs::middleend::typing::check_types;
use std::io::{Read, Write};
use std::path::PathBuf;
use std::thread::sleep;
use std::time::Duration;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Input file with pipes code to interpret. Either this or the code string must be provided
    input_file: Option<PathBuf>,

    /// Inline pipes code to be interpreted. Either this or the input file must be provided
    #[arg(short, long)]
    evaluate_string: Option<String>,

    /// Only parse and check types, but don't interpret
    #[arg(short, long)]
    check: bool,

    /// Don't do a platform-dependent terminal clear, and just print in a new page
    #[arg(long)]
    no_clear: bool,
}

fn main() -> Result<(), AnyError> {
    let args = Args::parse();
    loop {
        let result = interpret(&args, std::io::stdin(), std::io::stdout());
        match result {
            Ok(()) => {}
            Err(e) => {
                println!("Error: {}", e);
            }
        }
        sleep(Duration::from_millis(100))
    }
}

fn interpret<R: Read, W: Write>(args: &Args, read_src: R, print_dst: W) -> Result<(), AnyError> {
    let Args {
        input_file,
        evaluate_string,
        check,
        no_clear,
    } = args;

    if *no_clear {
        // clear screen and go to line 1 column 1 https://stackoverflow.com/a/34837038/2375586
        print!("{esc}[2J{esc}[1;1H", esc = 27 as char);
    } else {
        // reset terminal to avoid clearing by scrolling, but loses previous output
        print!("{esc}c", esc = 27 as char);
    }
    let code_string =
        SourceCode::new_from_string_or_file(evaluate_string.clone(), input_file.clone())?;
    let program = lex_and_parse(code_string)?;

    check_types(&program)?;

    if !check {
        let result = Runtime::evaluate(program, read_src, print_dst)?;
        if result != NOTHING {
            println!("{}", result);
        }
    }
    Ok(())
}
