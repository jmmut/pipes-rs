use crate::common::{context, err, AnyError};
use crate::frontend::token::{LocatedToken, Token};
use std::fmt::{Display, Formatter};
use std::path::PathBuf;

pub const NO_LOCATION: Location = Location {
    line: 0,
    column: 0,
    byte: 0,
};
pub const NO_SPAN: Span = Span {
    start: NO_LOCATION,
    end: NO_LOCATION,
};

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Span {
    pub start: Location,
    pub end: Location,
}
#[derive(Copy, Clone, PartialEq, Debug)]
pub struct Location {
    line: i32,
    column: i32,
    byte: usize,
}

impl Location {
    pub fn new() -> Self {
        Self {
            line: 1,
            column: 1,
            byte: 0,
        }
    }
    pub fn read(&mut self, letter: u8) {
        self.byte += 1;
        if letter == b'\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
    }
}
impl Display for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(Debug, Clone)]
pub struct SourceCode {
    pub text: String,
    pub cursor: Location,
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
            cursor: Location::new(),
            file: Some(file),
        })
    }
    pub fn new_fileless(text: String) -> Self {
        Self {
            text,
            cursor: Location::new(),
            file: None,
        }
    }
    pub fn get_location(&self) -> Location {
        self.cursor
    }
    pub fn located_token(&self, token: Token) -> LocatedToken {
        LocatedToken {
            token,
            span: self.span(),
        }
    }
    pub fn span_token(&self, token: Token, since: Location) -> LocatedToken {
        LocatedToken {
            token,
            span: self.span_since(since),
        }
    }
}

impl<S: AsRef<str>> From<S> for SourceCode {
    fn from(text: S) -> Self {
        SourceCode::new_fileless(text.as_ref().to_string())
    }
}

impl SourceCode {
    pub fn peek(&mut self) -> Option<u8> {
        self.peek_at(self.cursor.byte)
    }
    pub fn peek_at(&mut self, index: usize) -> Option<u8> {
        self.text.as_bytes().get(index).cloned()
    }
    pub fn next(&mut self) -> Option<u8> {
        if let Some(letter) = self.text.as_bytes().get(self.cursor.byte).cloned() {
            self.cursor.read(letter);
            Some(letter)
        } else {
            None
        }
    }
    pub fn consume(&mut self, matching: &str) -> bool {
        for i in 0..matching.len() {
            if let Some(letter) = self.peek_at(self.cursor.byte + i) {
                if letter != matching.as_bytes()[i] {
                    return false;
                }
            } else {
                return false;
            }
        }
        for _ in 0..matching.len() {
            self.next();
        }
        true
    }
    pub fn consume_if<F: Fn(u8, usize) -> Option<T>, T>(&mut self, predicate: F) -> Option<Vec<T>> {
        let mut i = 0;
        let mut consumed = Vec::new();
        loop {
            if let Some(letter) = self.peek_at(self.cursor.byte) {
                if let Some(t) = predicate(letter, i) {
                    consumed.push(t);
                    self.next();
                } else {
                    break;
                }
            } else {
                break;
            }
            i += 1;
        }
        if consumed.len() > 0 {
            Some(consumed)
        } else {
            None
        }
    }
    pub fn consume_while<F: Fn(u8, usize) -> bool>(&mut self, predicate: F) -> bool {
        self.consume_if(|f, i| if predicate(f, i) { Some(()) } else { None })
            .is_some()
    }

    pub fn consumed(&self) -> bool {
        self.cursor.byte == self.text.len()
    }
    pub fn span_since(&self, start: Location) -> Span {
        Span {
            start,
            end: self.get_location(),
        }
    }
    pub fn span(&self) -> Span {
        Span {
            start: self.get_location(),
            end: self.get_location(),
        }
    }
    pub fn format_current_location(&self) -> String {
        self.format_location(self.cursor)
    }
    pub fn format_location(&self, location: Location) -> String {
        let maybe_file = if let Some(file) = &self.file {
            format!("{}:", file.to_string_lossy())
        } else {
            "".to_string()
        };
        let (start, end) = self.get_current_line_indexes(location.byte);
        let line = &self.text[start..end];
        format!(
            " at {}{}:\n{}\n{}^\n",
            maybe_file,
            self.cursor,
            line,
            " ".repeat(location.byte - start)
        )
    }

    pub fn format_span(&self, Span { start, end }: Span) -> String {
        self.format_start_end(start, end)
    }
    pub fn format_start_end(&self, start: Location, end: Location) -> String {
        let maybe_file = if let Some(file) = &self.file {
            format!("{}:", file.to_string_lossy())
        } else {
            "".to_string()
        };
        let (start_start, start_end) = self.get_current_line_indexes(start.byte);
        let line = &self.text[start_start..start_end];
        let (end_start, end_end) = self.get_current_line_indexes(end.byte);
        if start_start == end_start && start_end == end_end {
            let (end_caret, column) = if start.column < end.column {
                ("^", format!("{}-{}", start.column, end.column))
            } else {
                ("", start.column.to_string())
            };
            let mid_caret = if start.byte + 1 < end.byte {
                "-".repeat(end.byte - start.byte - 1)
            } else {
                "".to_string()
            };
            format!(
                " at {maybe_file}{}:{column}:\n{line}\n{}^{mid_caret}{end_caret}\n",
                start.line,
                " ".repeat(start.byte - start_start),
            )
        } else {
            let end_line = &self.text[end_start..end_end];
            let mid_caret = if start.byte + 1 < start_end {
                "-".repeat(start_end - start.byte - 1)
            } else {
                "".to_string()
            };
            format!(
                " at {maybe_file}{start}-{end}:\n{line}\n{}^{mid_caret}\n{end_line}\n{}^\n",
                " ".repeat(start.byte - start_start),
                "-".repeat(end.byte - end_start),
            )
        }
    }
    pub fn get_current_line(&self) -> &str {
        let (start, end) = self.get_current_line_indexes(self.cursor.byte);
        &self.text[start..end]
    }
    pub fn get_current_line_indexes(&self, starting_byte: usize) -> (usize, usize) {
        if let Some(start) = self.index_of_previous(b'\n', starting_byte) {
            let end = self.index_of_next(b'\n', starting_byte);
            (start, end)
        } else {
            (0, 0)
        }
    }

    /// the returned index is non-inclusive, and may be text.len().
    pub fn index_of_next(&self, letter: u8, starting_byte: usize) -> usize {
        let mut index = starting_byte;
        loop {
            if let Some(index_letter) = self.text.as_bytes().get(index) {
                if *index_letter == letter {
                    return index;
                }
            } else {
                return index;
            }
            index += 1;
        }
    }

    /// the returned index points to the next position of letter.
    /// It may be 0, and may be None if the text is empty.
    pub fn index_of_previous(&self, letter: u8, starting_byte: usize) -> Option<usize> {
        let mut index = starting_byte;
        if self.text.len() == 0 {
            return None;
        }
        loop {
            if index == 0 {
                return Some(0);
            } else {
                index -= 1;
            }
            if let Some(index_letter) = self.text.as_bytes().get(index) {
                if *index_letter == letter {
                    return Some(index + 1);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_index_of_next() {
        let text = "asdf\nqwer\nzxcv".to_string();
        let mut code = SourceCode::new_fileless(text.clone());
        code.cursor.byte = 2;
        assert_eq!(code.index_of_next(b'\n', code.cursor.byte), 4);
        code.cursor.byte = 3;
        assert_eq!(code.index_of_next(b'\n', code.cursor.byte), 4);
        code.cursor.byte = 4;
        assert_eq!(code.index_of_next(b'\n', code.cursor.byte), 4);
        code.cursor.byte = 5;
        assert_eq!(code.index_of_next(b'\n', code.cursor.byte), 9);
        code.cursor.byte = 12;
        assert_eq!(code.index_of_next(b'\n', code.cursor.byte), text.len());
        code.cursor.byte = text.len() - 1;
        assert_eq!(code.index_of_next(b'\n', code.cursor.byte), text.len());
        code.cursor.byte = text.len();
        assert_eq!(code.index_of_next(b'\n', code.cursor.byte), text.len());
    }
    #[test]
    fn test_index_of_previous() {
        let text = "asdf\nqwer\nzxcv".to_string();
        let mut code = SourceCode::new_fileless(text.clone());
        code.cursor.byte = 0;
        assert_eq!(code.index_of_previous(b'\n', code.cursor.byte), Some(0));
        code.cursor.byte = 2;
        assert_eq!(code.index_of_previous(b'\n', code.cursor.byte), Some(0));
        code.cursor.byte = 4;
        assert_eq!(code.index_of_previous(b'\n', code.cursor.byte), Some(0));
        code.cursor.byte = 5;
        assert_eq!(code.index_of_previous(b'\n', code.cursor.byte), Some(5));
        code.cursor.byte = 12;
        assert_eq!(code.index_of_previous(b'\n', code.cursor.byte), Some(10));
        code.cursor.byte = text.len() - 1;
        assert_eq!(code.index_of_previous(b'\n', code.cursor.byte), Some(10));
        code.cursor.byte = text.len();
        assert_eq!(code.index_of_previous(b'\n', code.cursor.byte), Some(10));
    }
    #[test]
    fn test_get_current_line() {
        let text = "asdf\nqwer\nzxcv".to_string();
        let mut code = SourceCode::new_fileless(text.clone());
        code.cursor.byte = 6;
        assert_eq!(code.get_current_line(), "qwer");
        code.cursor.byte = text.len();
        assert_eq!(code.get_current_line(), "zxcv");
    }

    #[test]
    fn test_format_location() {
        let text = "asdf\nqwer\nzxcv".to_string();
        let mut code = SourceCode::new_fileless(text.clone());
        for _ in 0..11 {
            code.next();
        }
        assert_eq!(code.format_current_location(), " at 3:2:\nzxcv\n ^\n");
    }
    #[test]
    fn test_format_span_same_line() {
        let text = "asdf\nqwer\nzxcv".to_string();
        let mut code = SourceCode::new_fileless(text.clone());
        let start = code.get_location();
        let end = code.get_location();
        assert_eq!(
            code.format_start_end(start, end),
            r#" at 1:1:
asdf
^
"#
        );
        code.next();
        let end = code.get_location();
        let later_start = end;
        assert_eq!(
            code.format_start_end(start, end),
            r#" at 1:1-2:
asdf
^^
"#
        );
        code.next();
        let end = code.get_location();
        assert_eq!(
            code.format_start_end(start, end),
            r#" at 1:1-3:
asdf
^-^
"#
        );
        code.next();
        let end = code.get_location();
        assert_eq!(
            code.format_start_end(start, end),
            r#" at 1:1-4:
asdf
^--^
"#
        );
        assert_eq!(
            code.format_start_end(later_start, end),
            r#" at 1:2-4:
asdf
 ^-^
"#
        );
        code.next();
        code.next();
        code.next();
        let start = code.get_location();
        code.next();
        let end = code.get_location();
        assert_eq!(
            code.format_start_end(start, end),
            r#" at 2:2-3:
qwer
 ^^
"#
        );
    }

    #[test]
    fn test_format_span_different_line() {
        let text = "asdf\nqwer\nzxcv".to_string();
        let mut code = SourceCode::new_fileless(text.clone());
        let start = code.get_location();
        for _ in 0..5 {
            code.next();
        }
        let end = code.get_location();
        assert_eq!(
            code.format_start_end(start, end),
            r#" at 1:1-2:1:
asdf
^---
qwer
^
"#
        );
        let mut code = SourceCode::new_fileless(text.clone());
        code.next();
        code.next();
        let start = code.get_location();
        for _ in 0..5 {
            code.next();
        }
        let end = code.get_location();
        assert_eq!(
            code.format_start_end(start, end),
            r#" at 1:3-2:3:
asdf
  ^-
qwer
--^
"#
        );
        let mut code = SourceCode::new_fileless(text.clone());
        code.next();
        code.next();
        code.next();
        let start = code.get_location();
        for _ in 0..5 {
            code.next();
        }
        let end = code.get_location();
        assert_eq!(
            code.format_start_end(start, end),
            r#" at 1:4-2:4:
asdf
   ^
qwer
---^
"#
        );
        let mut code = SourceCode::new_fileless(text.clone());
        code.next();
        code.next();
        code.next();
        code.next();
        let start = code.get_location();
        for _ in 0..5 {
            code.next();
        }
        let end = code.get_location();
        assert_eq!(
            code.format_start_end(start, end),
            r#" at 1:5-2:5:
asdf
    ^
qwer
----^
"#
        );
    }

    #[test]
    fn test_consume() {
        let text = "0123456789".to_string();
        let mut code = SourceCode::new_fileless(text.clone());
        assert_eq!(code.consume("345"), false);
        code.next();
        code.next();
        code.next();
        assert_eq!(code.consume("345"), true);
        assert_eq!(
            code.cursor,
            Location {
                line: 1,
                column: 7,
                byte: 6,
            }
        );
        assert_eq!(code.consume("6789"), true);
        assert_eq!(code.consume("0"), false);
        let mut code = SourceCode::new_fileless(text.clone());
        assert_eq!(code.consume("01234567890"), false);
    }
    #[test]
    fn test_consume_if() {
        let text = "abc345cde".to_string();
        let mut code = SourceCode::new_fileless(text.clone());
        let is_digit = |letter, _| {
            if letter >= b'0' && letter <= b'9' {
                Some(letter)
            } else {
                None
            }
        };
        let is_letter = |letter, _| {
            if letter >= b'a' && letter <= b'z' {
                Some(letter)
            } else {
                None
            }
        };
        assert_eq!(code.consume_if(is_digit), None);
        code.next();
        code.next();
        code.next();
        assert_eq!(
            code.consume_if(is_digit),
            Some("345".to_string().as_bytes().to_vec())
        );
        assert_eq!(
            code.cursor,
            Location {
                line: 1,
                column: 7,
                byte: 6,
            }
        );
        assert_eq!(code.peek(), Some(b'c'));
        assert_eq!(
            code.consume_if(is_letter),
            Some("cde".to_string().as_bytes().to_vec())
        );
        assert_eq!(code.consume_if(is_letter), None);

        let mut code = SourceCode::new_fileless(text.clone());
        assert_eq!(
            code.consume_if(|letter, _| Some(letter)),
            Some(text.as_bytes().to_vec())
        );
    }
}
