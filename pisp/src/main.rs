mod backend;
mod expression;
mod frontend;

use crate::backend::eval;
use crate::frontend::frontend;
use pipes_rs::common::AnyError;
use std::io::Write;

enum ReplResult {
    Result,
    Exit,
}

pub fn main() {
    loop {
        let line = prompt();
        let result = try_line(line);
        if let Err(err) = result {
            println!("ERROR: {}", err)
        } else if let Ok(ReplResult::Exit) = result {
            break;
        }
    }
}
fn try_line(line: Result<Option<String>, AnyError>) -> Result<ReplResult, AnyError> {
    match line? {
        Some(line) => {
            if line == "quit" || line == "exit" {
                return Ok(ReplResult::Exit);
            }
            // println!("bytes: '{:?}'", line.as_bytes());
            let expression = frontend(&line)?;
            let result = eval(&expression)?;
            println!("{}", result);
            Ok(ReplResult::Result)
        }
        None => {
            println!();
            Ok(ReplResult::Exit)
        }
    }
}

fn prompt() -> Result<Option<String>, AnyError> {
    let mut line = "".to_string();
    print!("> ");
    std::io::stdout().flush()?;
    std::io::stdin().read_line(&mut line)?;
    if line.len() == 0 {
        // this is what happens if you do CTRL-D
        Ok(None)
    } else {
        if line.ends_with("\n") {
            line.pop();
        }
        Ok(Some(line))
    }
}
