use crate::backend::evaluate::NOTHING;
use crate::backend::Runtime;
use crate::common::AnyError;
use crate::frontend::expression::{Expression, ExpressionSpan};
use crate::frontend::lex_and_parse_with_identifiers;
use crate::frontend::program::Identifiers;
use crate::middleend::intrinsics::builtin_types;
use std::io::{Read, Write};

impl<R: Read, W: Write, W2: Write> Runtime<R, W, W2> {
    #[cfg(not(unix))]
    pub fn debugger_prompt(&mut self, _arguments: &[i64]) -> Result<(), AnyError> {
        // debugger unsupported in wasm
        Ok(())
    }
    #[cfg(unix)]
    pub fn debugger_prompt(&mut self, arguments: &[i64]) -> Result<(), AnyError> {
        use std::fs::File;
        use std::io::{BufRead, BufReader};
        use std::os::fd::FromRawFd;
        use std::os::raw;
        let special_file_descriptor = arguments[1];
        loop {
            let mut command = String::new();
            // println!("extra_inputs before: {:?}", self.extra_inputs);
            let result: Result<usize, AnyError> = if special_file_descriptor != 0 {
                write!(
                    self.print_err_mut(),
                    "(debugger from fd {}) ",
                    special_file_descriptor
                )?;
                let special_file_descriptor = special_file_descriptor as raw::c_int;
                if let Some(reader) = self.extra_inputs.get_mut(&special_file_descriptor) {
                    reader.read_line(&mut command)
                } else {
                    let mut reader =
                        BufReader::new(unsafe { File::from_raw_fd(special_file_descriptor) });
                    let result = reader.read_line(&mut command);
                    self.extra_inputs.insert(special_file_descriptor, reader);
                    result
                }
                .map_err(|e| e.into())
            } else {
                let mut buffer = [0; 1];
                let mut second_buffer = Vec::new();
                write!(self.print_err_mut(), "(debugger) ")?;
                let mut bytes_read = self.read_input.as_mut().unwrap().read_exact(&mut buffer);
                while let Ok(_n) = bytes_read {
                    second_buffer.push(buffer[0]);
                    if buffer[0] == b'\n' {
                        break;
                    }
                    bytes_read = self.read_input.as_mut().unwrap().read_exact(&mut buffer);
                }
                match String::from_utf8(second_buffer) {
                    Ok(str) => {
                        command = str;
                        Ok(command.len())
                    }
                    Err(e) => Err(e.to_string().into()),
                }
            };
            // println!("extra_inputs after: {:?}", self.extra_inputs);
            match result {
                Ok(_n) => {
                    command.pop(); // remove \n
                    if ["quit", "exit", "continue"].iter().any(|c| *c == command) {
                        write!(self.print_err_mut(), "leaving debugger\n")?;
                        break;
                    } else {
                        self.evaluate_command(arguments, command)?;
                    }
                }
                Err(error) => write!(self.print_err_mut(), "breakpoint error: {error}\n")?,
            }
        }
        Ok(())
    }

    fn evaluate_command(&mut self, arguments: &[i64], command: String) -> Result<(), AnyError> {
        let identifiers = self.mock_identifiers();
        match lex_and_parse_with_identifiers(command, identifiers) {
            Ok(program) => {
                match self.evaluate_recursive_with_initial(
                    arguments[0],
                    &builtin_types::UNKNOWN,
                    program.main(),
                ) {
                    Ok(output) => {
                        if output == NOTHING {
                            write!(self.print_err_mut(), "result: none\n")?;
                        } else {
                            write!(self.print_err_mut(), "result: {}\n", output)?;
                        }
                    }
                    Err(e) => {
                        write!(self.print_err_mut(), "couldn't evaluate prompt: {}\n", e)?;
                    }
                }
            }
            Err(e) => {
                write!(self.print_err_mut(), "couldn't parse prompt: {}\n", e)?;
            }
        }
        Ok(())
    }

    fn mock_identifiers(&self) -> Identifiers {
        let mut identifiers = Identifiers::new();
        let none = ExpressionSpan::new_spanless(Expression::Nothing);
        for name in self.identifier_expressions.keys() {
            identifiers.insert(name.clone(), none.clone());
        }
        for name in self.identifiers.keys() {
            identifiers.insert(name.clone(), none.clone());
        }
        identifiers
    }
    fn print_err_mut(&mut self) -> &mut W2 {
        self.print_err.as_mut().unwrap()
    }
}

#[cfg(test)]
mod tests {
    use crate::backend::evaluate::tests::interpret_io;

    #[test]
    fn test_debugger() {
        let result = interpret_io("3 |function (a) {|breakpoint 0}", "a\nquit\n");
        assert_eq!(
            result,
            (
                3,
                "".to_string(),
                "(debugger) result: 3\n(debugger) leaving debugger\n".to_string()
            )
        );
    }
}
