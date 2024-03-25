use std::fs::File;
use std::io::{BufRead, BufReader, Read, Write};
use std::os::fd::FromRawFd;
use std::os::raw;
use crate::backend::Runtime;

impl<R: Read, W: Write> Runtime<R, W> {
    pub fn debugger_prompt(&mut self, arguments: &[i64]) {
        let special_file_descriptor = arguments[1];
        loop {
            let mut command = String::new();
            // println!("extra_inputs before: {:?}", self.extra_inputs);
            let result = if special_file_descriptor != 0 {
                eprint!("(debugger from fd {}) ", special_file_descriptor);
                let special_file_descriptor = special_file_descriptor as raw::c_int;
                if let Some(reader) = self.extra_inputs.get_mut(&special_file_descriptor) {
                    reader.read_line(&mut command)
                } else {
                    let mut reader = BufReader::new(unsafe { File::from_raw_fd(special_file_descriptor) });
                    let result = reader.read_line(&mut command);
                    self.extra_inputs.insert(special_file_descriptor, reader);
                    result
                }
                // let reader = self.extra_inputs.entry(special_file_descriptor).or_insert(
                //     BufReader::new(unsafe { File::from_raw_fd(special_file_descriptor) })
                // );
                // reader.read_line(&mut command)
            } else {
                eprint!("(debugger) ");
                std::io::stdin().read_line(&mut command)
            };
            // println!("extra_inputs after: {:?}", self.extra_inputs);
            match result {
                Ok(n) => {
                    command.pop(); // remove \n
                    println!("breakpoint: {n} bytes read, command: {}", command);
                    if ["quit", "exit", "continue"].iter().any(|c| *c == command) {
                        eprintln!("leaving debugger");
                        break;
                    }
                },
                Err(error) => println!("breakpoint error: {error}"),
            }
        }
    }
}
