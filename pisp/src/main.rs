mod expression;
mod frontend;

use pipes_rs::common::AnyError;
use std::io::Write;
use crate::frontend::frontend;

pub fn main() {
    loop {
        let line = prompt();
        match line {
            Ok(Some(line)) => {
                if line == "quit" || line == "exit" {
                    break;
                }
                println!("bytes: '{:?}'", line.as_bytes());
                let expression = frontend(&line);
                println!("{}", expression);
            }
            Ok(None) => {
                println!();
                break;
            }
            Err(e) => {
                println!("ERROR: {}", e)
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
