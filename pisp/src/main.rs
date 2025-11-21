mod expression;
mod frontend;

use crate::frontend::frontend;
use pipes_rs::common::AnyError;
use std::io::Write;

pub fn main() {
    loop {
        let line = prompt();
        match line {
            Ok(Some(line)) => {
                if line == "quit" || line == "exit" {
                    break;
                }
                // println!("bytes: '{:?}'", line.as_bytes());
                let expression = frontend(&line);
                match expression {
                    Ok(expression) => {
                        println!("{}", expression);
                    }
                    Err(err) => {
                        println!("ERROR: {}", err)
                    }
                }
            }
            Ok(None) => {
                println!();
                break;
            }
            Err(err) => {
                println!("ERROR: {}", err)
            }
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
