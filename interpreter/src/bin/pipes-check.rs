use std::io::{Read, Write};
use std::path::PathBuf;
use std::thread::sleep;
use std::time::Duration;

use clap::Parser;

use pipes_rs::backend::evaluate::NOTHING;
use pipes_rs::backend::Runtime;
use pipes_rs::common::AnyError;
use pipes_rs::frontend::lex_and_parse;
use pipes_rs::frontend::sources::location::SourceCode;
use pipes_rs::middleend::comptime::rewrite;
use pipes_rs::middleend::typing::put_types;

/// Continuously (~10 times per second) try to interpret (or just typecheck) a pipes program.
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Input file with pipes code to interpret. Either this or --evaluate-string must be provided
    input_file: Option<PathBuf>,

    /// Inline pipes code to be interpreted. Either this or the input file must be provided
    #[arg(short, long)]
    evaluate_string: Option<String>,

    /// Only parse and check types, but don't interpret
    #[arg(short, long)]
    check_only: bool,

    /// Wait this number of milliseconds before interpreting (or just typechecking) again
    #[arg(short, long, default_value = "100")]
    period_millis: u64,

    /// Don't do a platform-dependent terminal clear, and just print in a new page
    #[arg(long)]
    no_clear: bool,
}

fn main() {
    let args = Args::parse();
    loop {
        if let Err(e) = interpret(&args, std::io::stdin(), std::io::stdout()) {
            clear_terminal(args.no_clear);
            println!("Error: {}", e);
        }
        sleep(Duration::from_millis(args.period_millis))
    }
}

fn interpret<R: Read, W: Write>(args: &Args, read_src: R, print_dst: W) -> Result<(), AnyError> {
    let Args {
        input_file,
        evaluate_string,
        check_only: check,
        no_clear,
        ..
    } = args;

    let code_string =
        SourceCode::new_from_string_or_file(evaluate_string.clone(), input_file.clone())?;
    let mut program = lex_and_parse(code_string)?;

    put_types(&mut program)?;
    let program = rewrite(program)?;

    if !check {
        let result = Runtime::evaluate(program, read_src, print_dst)?;
        clear_terminal(*no_clear);
        if result != NOTHING {
            println!("{}", result);
        }
    }
    Ok(())
}

fn clear_terminal(no_clear: bool) {
    if no_clear {
        // clear screen and go to line 1 column 1 https://stackoverflow.com/a/34837038/2375586
        print!("{esc}[2J{esc}[1;1H", esc = 27 as char);
    } else {
        // reset terminal to avoid clearing by scrolling, but loses previous output
        print!("{esc}c", esc = 27 as char);
    }
}
