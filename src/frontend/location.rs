use crate::common::AnyError;
use std::path::PathBuf;

pub struct Location {
    pub file: Option<PathBuf>,
    pub line: i32,
    pub column: i32,
    pub line_end: i32,
    pub column_end: i32,
}

// impl Location {
//     pub fn new(file: PathBuf, line: i32, column: i32, line_end: i32, column_end: i32) -> Self {
//         Self {
//             file: Some(file),
//             line,
//             column,
//             line_end,
//             column_end,
//         }
//     }
//     pub fn new_fileless(line: i32, column: i32, line_end: i32, column_end: i32) -> Self {
//         Self {
//             file: None,
//             line,
//             column,
//             line_end,
//             column_end,
//         }
//     }
// }

pub struct SourceCode {
    pub text: String,
    pub lines_read: i32,
    pub bytes_read: i64,
    pub file: Option<PathBuf>,
}

impl SourceCode {
    pub fn new_from_string_or_file(
        code_string: Option<String>,
        input_file: Option<PathBuf>,
    ) -> Result<SourceCode, AnyError> {
        if code_string.is_some() && input_file.is_some() {
            Err("Only the code string or the input file should be provided")?
        } else if let Some(code) = code_string {
            Ok(SourceCode::new_fileless(code))
        } else if let Some(file) = input_file {
            Ok(SourceCode::new(file)?)
        } else {
            Err("Either the code string or the input file should be provided")?
        }
    }

    pub fn new(file: PathBuf) -> Result<Self, std::io::Error> {
        let text = std::fs::read_to_string(&file)?;
        Ok(Self {
            text,
            lines_read: 0,
            bytes_read: 0,
            file: Some(file),
        })
    }
    pub fn new_fileless(text: String) -> Self {
        Self {
            text,
            lines_read: 0,
            bytes_read: 0,
            file: None,
        }
    }
}
