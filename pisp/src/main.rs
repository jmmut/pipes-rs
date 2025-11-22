mod backend;
mod expression;
mod frontend;

use crate::backend::{eval, Environment};
use crate::frontend::frontend;
use pipes_rs::common::AnyError;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

const HISTORY_FILE: &'static str = "pisp-history.txt";

enum ReplResult {
    Result,
    Exit,
}

pub fn main() -> Result<(), AnyError> {
    let mut rl = DefaultEditor::new()?;
    let _ = rl.load_history(HISTORY_FILE);
    let mut env = Environment::new();
    loop {
        let line = prompt(&mut rl);
        let result = try_line(line, &mut env);
        if let Err(err) = result {
            println!("ERROR: {}", err)
        } else if let Ok(ReplResult::Exit) = result {
            break;
        }
    }
    rl.save_history(HISTORY_FILE)?;
    Ok(())
}
fn try_line(
    line: Result<Option<String>, AnyError>,
    env: &mut Environment,
) -> Result<ReplResult, AnyError> {
    match line? {
        Some(line) => {
            if line == "quit" || line == "exit" {
                return Ok(ReplResult::Exit);
            }
            // println!("bytes: '{:?}'", line.as_bytes());
            let expression = frontend(&line)?;
            let result = env.eval(&expression)?;
            println!("{}", result);
            Ok(ReplResult::Result)
        }
        None => Ok(ReplResult::Exit),
    }
}

fn prompt(rl: &mut DefaultEditor) -> Result<Option<String>, AnyError> {
    let readline = rl.readline("> ");
    match readline {
        Ok(line) => {
            rl.add_history_entry(line.as_str())?;
            Ok(Some(line))
        }
        Err(ReadlineError::Interrupted) => Ok(None),
        Err(ReadlineError::Eof) => Ok(None),
        Err(err) => Err(err.into()),
    }
}
