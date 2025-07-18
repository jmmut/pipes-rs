use std::io::{Read, Write};
use std::path::PathBuf;

use clap::Parser;

use pipes_rs::backend::evaluate::NOTHING;
use pipes_rs::backend::Runtime;
use pipes_rs::common::AnyError;
use pipes_rs::frontend::lex_and_parse;
use pipes_rs::frontend::sources::location::SourceCode;
use pipes_rs::middleend::comptime::rewrite;
use pipes_rs::middleend::typing::put_types;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Input file with pipes code to interpret. Either this or the code string must be provided
    input_file: Option<PathBuf>,

    /// Optional initial value that the script will receive
    initial_value: Option<i64>,

    /// Inline pipes code to be interpreted. Either this or the input file must be provided
    #[arg(short, long)]
    evaluate_string: Option<String>,

    /// Only parse and check types, but don't interpret
    #[arg(short, long)]
    check: bool,

    /// Print the AST
    #[arg(short, long)]
    debug_ast: bool,

    /// Print the prettified AST
    #[arg(short, long)]
    prettify: bool,
}

fn main() {
    if let Err(e) = interpret(Args::parse(), std::io::stdin(), std::io::stdout()) {
        println!("Error: {}", e);
    }
}

fn interpret<R: Read, W: Write>(args: Args, read_src: R, print_dst: W) -> Result<(), AnyError> {
    reset_sigpipe();
    let Args {
        evaluate_string,
        check,
        input_file,
        initial_value,
        prettify,
        debug_ast,
    } = args;
    let code_string = SourceCode::new_from_string_or_file(evaluate_string, input_file)?;
    let program = lex_and_parse(code_string)?;

    let mut program = rewrite(program)?;
    put_types(&mut program)?;

    // two ifs so that --debug-ast and --prettify only prints once, prettified
    if debug_ast || prettify {
        if prettify {
            println!("Expression: {}", program.main());
        } else {
            println!("Expression: {:#?}", program.main());
        }
    }

    if !check {
        let result = Runtime::evaluate_with_initial(program, initial_value, read_src, print_dst)?;
        if result != NOTHING {
            println!("{}", result);
        }
    }
    Ok(())
}

/// Don't print an error message if our output could not be written to a (closed) pipe.
/// This can happen if you do `cargo run | head`. The behaviour should be to die without printing
/// any error. see https://stackoverflow.com/a/65760807/2375586
#[cfg(unix)]
fn reset_sigpipe() {
    unsafe {
        libc::signal(libc::SIGPIPE, libc::SIG_DFL);
    }
}

#[cfg(not(unix))]
fn reset_sigpipe() {
    // no-op
}

#[cfg(test)]
mod tests {
    use pipes_rs::common::unwrap_display;

    use super::*;

    #[test]
    fn test_read_file() {
        let mut print_dst = Vec::<u8>::new();
        let args = Args {
            evaluate_string: None,
            check: false,
            input_file: Some(PathBuf::from("../pipes_programs/demos/hello_world.pipes")),
            initial_value: None,
            debug_ast: false,
            prettify: false,
        };
        unwrap_display(interpret(args, std::io::stdin(), &mut print_dst));
        assert_eq!(
            String::from_utf8(print_dst).unwrap().as_str(),
            "Hello World!\n"
        );
    }
}
