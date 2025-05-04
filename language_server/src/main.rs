use crate::message_buffer::MessageBuffer;
use crate::response::response;
use clap::Parser;
use pipes_rs::common::AnyError;
use std::fs::File;
use std::io::Read;
use std::io::Write;

mod analyzer;
mod message_buffer;
mod request;
mod response;

/// Command line arguments
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Optional path to a log file
    #[arg()]
    log_file: Option<String>,
}

fn main() {
    if let Err(e) = try_main() {
        println!("Error: {}", e)
    }
}

fn try_main() -> Result<(), AnyError> {
    let args = Args::parse();
    let mut log: Option<File> = None;
    if let Some(log_path) = args.log_file {
        match File::create(&log_path) {
            Ok(file) => log = Some(file),
            Err(e) => {
                eprintln!("Failed to open log file: {}", e);
                std::process::exit(1);
            }
        }
    }

    let mut message_buffer = MessageBuffer::new();
    let stdin = std::io::stdin();
    let mut handle = stdin.lock();
    let mut buf = [0; 1]; // read one byte at a time

    while handle.read(&mut buf).unwrap_or(0) == 1 {
        let c = buf[0];

        if let Some(log_file) = log.as_mut() {
            let _ = write!(log_file, "{}", c as char);
        }

        message_buffer.handle_char(c)?;

        if message_buffer.is_message_complete() {
            if let Some(log_file) = log.as_mut() {
                let _ = writeln!(
                    log_file,
                    "\n\n###### request body:\n{}",
                    message_buffer.body()?
                );
            }

            match response(&message_buffer.body()?) {
                Ok(answer) => {
                    if let Some(log_file) = log.as_mut() {
                        let _ = writeln!(
                            log_file,
                            "\n\n###### answer:\n{:?}\n\n###### waiting for next request:",
                            answer
                        );
                    }
                    if let Some(answer) = answer {
                        print!("{}", answer);
                        std::io::stdout().flush().unwrap();
                    }
                }
                Err(e) => {
                    if let Some(log_file) = log.as_mut() {
                        let _ = writeln!(log_file, "\n\n###### exception: {}", e);
                    }
                }
            }

            message_buffer.reset();
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::request::parse;
    use crate::response::choose_response;
    use pipes_rs::common::unwrap_display;

    #[test]
    fn test_json_response_balance() {
        let input = r#"{"jsonrpc":"2.0","id":"2","method":"textDocument/hover","params":{"textDocument":{"uri":"file://../pipes_programs/demos/hello_world.pipes"},"position":{"line":1,"character":2}}}"#;
        let request = parse(input).unwrap();
        let response_body = unwrap_display(choose_response(&request));
        let json = &response_body.unwrap_or("".to_string());

        assert_eq!(json.chars().filter(|&c| c == '"').count() % 2, 0);
        assert_eq!(
            json.chars().filter(|&c| c == '{').count(),
            json.chars().filter(|&c| c == '}').count()
        );
    }
}
