use pipes_rs::common::AnyError;
use std::io::Write;

pub fn main() {
    loop {
        let line = prompt();
        match line {
            Ok(line) => {
                println!("{}", line);
                if line == "quit" || line == "exit" {
                    break;
                }
            }
            Err(e) => {
                println!("ERROR: {}", e)
            }
        }
    }
}

fn prompt() -> Result<String, AnyError> {
    let mut line = "".to_string();
    print!("> ");
    std::io::stdout().flush()?;
    std::io::stdin().read_line(&mut line)?;
    if line.ends_with("\n") {
        line.pop();
    }
    Ok(line)
}
