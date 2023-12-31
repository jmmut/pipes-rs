use clap::Parser;
use pipes_rs::common::AnyError;
use std::io::{Read, Write};
use std::path::PathBuf;

use pipes_rs::evaluate::{Runtime, NOTHING};
use pipes_rs::frontend::ast::ast_deserialize_source;
use pipes_rs::frontend::lex_and_parse_source;
use pipes_rs::frontend::location::SourceCode;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Input file with pipes code to interpret. Either this or the code string must be provided
    input_file: Option<PathBuf>,

    /// Inline pipes code to be interpreted. Either this or the input file must be provided
    #[arg(short, long)]
    code_string: Option<String>,

    /// AST syntax, like `[ 5 +7 Op |print_char Op Chain 8 ]`
    #[arg(short, long)]
    ast: bool,

    /// Print the AST
    #[arg(short, long)]
    debug_ast: bool,

    /// Print the prettified AST
    #[arg(short, long)]
    prettify: bool,
}

fn main() -> Result<(), AnyError> {
    let result = interpret(Args::parse(), std::io::stdin(), std::io::stdout());
    match result {
        Ok(()) => {}
        Err(e) => {
            println!("Error: {}", e);
        }
    }
    Ok(())
}
fn interpret<R: Read, W: Write>(args: Args, read_src: R, print_dst: W) -> Result<(), AnyError> {
    let Args {
        code_string,
        input_file,
        prettify,
        ast,
        debug_ast,
    } = args;
    let code_string = SourceCode::new_from_string_or_file(code_string, input_file)?;
    let expression = if ast {
        ast_deserialize_source(&code_string)?
    } else {
        lex_and_parse_source(&code_string)?
    };

    // two ifs so that --debug-ast and --prettify only prints once, prettified
    if debug_ast || prettify {
        if prettify {
            println!("Expression: {:#?}", expression);
        } else {
            println!("Expression: {:?}", expression);
        }
    }

    let result = Runtime::evaluate(expression, read_src, print_dst)?;
    if result != NOTHING {
        println!("{}", result);
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_read_file() {
        let mut print_dst = Vec::<u8>::new();
        let args = Args {
            code_string: None,
            input_file: Some(PathBuf::from("pipes_programs/demos/hello_world.pipes")),
            ast: false,
            debug_ast: false,
            prettify: true,
        };
        interpret(args, std::io::stdin(), &mut print_dst).unwrap();
        assert_eq!(
            String::from_utf8(print_dst).unwrap().as_str(),
            "Hello World!\n"
        );
    }
}
