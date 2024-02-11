use std::iter::Peekable;
use std::path::PathBuf;
use std::str::Bytes;

use crate::common::{context, err, AnyError};

#[derive(Clone, PartialEq, Debug)]
pub struct Location {
    pub file: Option<PathBuf>,
    pub start: LineColumn,
    pub end: LineColumn,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub struct LineColumn {
    line: i32,
    column: i32,
}

impl LineColumn {
    pub fn new() -> Self {
        Self { line: 1, column: 1 }
    }
    pub fn read(&mut self, letter: u8) {
        if letter == b'\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
    }
}

#[derive(Debug, Clone)]
pub struct SourceCode {
    pub text: String,
    pub cursor_byte: usize,
    pub cursor: LineColumn,
    pub file: Option<PathBuf>,
}

impl SourceCode {
    pub fn new_from_string_or_file(
        code_string: Option<String>,
        input_file: Option<PathBuf>,
    ) -> Result<SourceCode, AnyError> {
        if code_string.is_some() && input_file.is_some() {
            err("Only the code string or the input file should be provided")?
        } else if let Some(code) = code_string {
            Ok(SourceCode::new_fileless(code))
        } else if let Some(file) = input_file {
            Ok(SourceCode::new(file)?)
        } else {
            err("Either the code string or the input file should be provided")?
        }
    }

    pub fn new(file: PathBuf) -> Result<Self, AnyError> {
        let text = context(
            format!("Reading file '{}'", file.to_string_lossy()),
            std::fs::read_to_string(&file).map_err(|e| e.into()),
        )?;
        Ok(Self {
            text,
            cursor_byte: 0,
            cursor: LineColumn::new(),
            file: Some(file),
        })
    }
    pub fn new_fileless(text: String) -> Self {
        Self {
            text,
            cursor_byte: 0,
            cursor: LineColumn::new(),
            file: None,
        }
    }
    pub fn get_line_column(&self) -> LineColumn {
        self.cursor
    }
}

impl<S: AsRef<str>> From<S> for SourceCode {
    fn from(text: S) -> Self {
        SourceCode::new_fileless(text.as_ref().to_string())
    }
}

impl SourceCode {
    pub fn peek(&mut self) -> Option<u8> {
        self.text.as_bytes().get(self.cursor_byte).cloned()
    }
    pub fn next(&mut self) -> Option<u8> {
        self.cursor_byte += 1;
        self.text.as_bytes().get(self.cursor_byte - 1).cloned()
    }
}
