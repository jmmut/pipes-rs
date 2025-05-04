use pipes_rs::common::{AnyError, err};
use std::fmt;
use std::str::FromStr;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Method {
    Unknown,
    Initialize,
    Initialized,
    Hover,
    Shutdown,
}

impl FromStr for Method {
    type Err = AnyError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "initialize" => Ok(Method::Initialize),
            "initialized" => Ok(Method::Initialized),
            "textDocument/hover" => Ok(Method::Hover),
            "shutdown" => Ok(Method::Shutdown),
            _ => Ok(Method::Unknown),
        }
    }
}

impl fmt::Display for Method {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Method::Unknown => "UNKNOWN",
            Method::Initialize => "INITIALIZE",
            Method::Initialized => "INITIALIZED",
            Method::Hover => "HOVER",
            Method::Shutdown => "SHUTDOWN",
        };
        write!(f, "{}", s)
    }
}

fn method_requires_response_with_id(method: Method) -> bool {
    matches!(
        method,
        Method::Initialize | Method::Hover | Method::Shutdown
    )
}

fn method_requires_position(method: Method) -> bool {
    matches!(method, Method::Hover)
}

pub fn extract_str(request: &str, key: &str) -> Result<String, AnyError> {
    let key_pos = request
        .find(key)
        .ok_or_else(|| format!("Can't find key: {}", key))?;
    let start = request[key_pos + key.len()..]
        .find('"')
        .ok_or_else(|| format!("Can't find start quote for key: {}", key))?
        + key_pos
        + key.len()
        + 1;
    let end = request[start..]
        .find('"')
        .ok_or_else(|| format!("Can't find end quote for key: {}", key))?
        + start;
    Ok(request[start..end].to_string())
}

pub fn extract_method(request: &str) -> Result<String, AnyError> {
    extract_str(request, &double_quote("method"))
}

fn is_number(c: u8) -> bool {
    c.is_ascii_digit()
}

pub fn parse_next_number(message: &str, mut pos: usize) -> i32 {
    let message_bytes = message.as_bytes();
    while pos < message.len() && !is_number(message_bytes[pos]) {
        pos += 1;
    }
    let mut n = 0;
    while pos < message.len() && is_number(message_bytes[pos]) {
        let c = message_bytes[pos];
        n *= 10;
        n += (c as u8 - b'0') as i32;
        pos += 1;
    }
    n
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct PositionInCode {
    pub absolute_char: i32,
    pub line: i32,
    pub char_in_line: i32,
}

fn double_quote(s: &str) -> String {
    format!("\"{}\"", s)
}
pub fn extract_position(request: &str) -> Result<PositionInCode, AnyError> {
    #[track_caller]
    fn can_not_find<T>(s: &str, request: &str) -> Result<T, AnyError> {
        err(format!(
            "Can not parse request. Could not find {}: {}",
            s, request
        ))
    }
    #[track_caller]
    fn find_quoted(key: &str, start: usize, request: &str) -> Result<usize, AnyError> {
        let quoted = double_quote(key);
        if let Some(pos) = request[start..].find(&quoted) {
            Ok(pos)
        } else {
            can_not_find(&quoted, request)
        }
    }

    let position_pos = find_quoted("position", 0, request)?;
    let line_pos = find_quoted("line", position_pos, request)? + position_pos;
    let line = parse_next_number(request, line_pos);
    let char_pos = find_quoted("character", position_pos, request)? + position_pos;
    let character = parse_next_number(request, char_pos);

    Ok(PositionInCode {
        absolute_char: -1,
        line: line + 1,
        char_in_line: character + 1,
    })
}

fn extract_file(request: &str) -> Result<String, AnyError> {
    let uri = extract_str(request, &double_quote("uri"))?;
    let file_path = uri.strip_prefix("file://").unwrap();
    Ok(file_path.to_string())
}

pub struct Request {
    pub method: Method,
    id_: Option<String>,
    location_: Option<PositionInCode>,
    file_: Option<String>,
}

impl Request {
    pub fn id(&self) -> Result<&str, AnyError> {
        self.id_
            .as_deref()
            .ok_or_else(|| format!("Request {:?} didn't have an ID", self.method).into())
    }

    pub fn location(&self) -> Result<PositionInCode, AnyError> {
        self.location_
            .ok_or_else(|| format!("Request {:?} didn't have a location", self.method).into())
    }

    pub fn file(&self) -> Result<&str, AnyError> {
        self.file_
            .as_deref()
            .ok_or_else(|| format!("Request {:?} didn't have a file path", self.method).into())
    }
}

pub fn parse(request: &str) -> Result<Request, AnyError> {
    let method = Method::from_str(&extract_method(request)?)?;
    let id = if method_requires_response_with_id(method) {
        Some(extract_str(request, &double_quote("id"))?)
    } else {
        None
    };
    let location = if method_requires_position(method) {
        Some(extract_position(request)?)
    } else {
        None
    };
    let file = if method_requires_position(method) {
        Some(extract_file(request)?)
    } else {
        None
    };
    Ok(Request {
        method,
        id_: id,
        location_: location,
        file_: file,
    })
}

#[cfg(test)]
mod tests {
    use pipes_rs::common::unwrap_display;
    use super::*;
    
    #[test]
    fn test_initialize_parsing() {
        let input_method_initialize = r#"{"jsonrpc":"2.0","id":"1","method":"initialize","params":{"processId":null,"rootUri":"file:///home/jmmut/Documents/conan/pipes/","capabilities":{"workspace":{"applyEdit":true,"workspaceEdit":{"documentChanges":true},"didChangeConfiguration":{},"didChangeWatchedFiles":{"dynamicRegistration":true},"symbol":{},"executeCommand":{},"workspaceFolders":false,"configuration":true},"textDocument":{"synchronization":{"willSave":true,"willSaveWaitUntil":true,"didSave":true},"completion":{"completionItem":{"snippetSupport":true}},"hover":{},"signatureHelp":{},"references":{},"documentHighlight":{},"formatting":{},"rangeFormatting":{},"onTypeFormatting":{},"definition":{},"codeAction":{},"rename":{"prepareSupport":true,"dynamicRegistration":false},"semanticHighlightingCapabilities":{"semanticHighlighting":false}}}}}"#;

        assert_eq!(
            unwrap_display(extract_method(input_method_initialize)),
            "initialize".to_string()
        );
        assert_eq!(
            unwrap_display(extract_str(input_method_initialize, "\"id\"")),
            "1"
        );
        let parsed = parse(input_method_initialize).unwrap();
        assert_eq!(parsed.method, Method::Initialize);
    }

    #[test]
    fn test_parsing_initialized_method() {
        let initialized = "Content-Length: 52\r\n\r\n{\"jsonrpc\":\"2.0\",\"method\":\"initialized\",\"params\":{}}";
        let parsed = parse(initialized).unwrap();
        assert_eq!(parsed.method, Method::Initialized);
    }
    
    #[test]
    fn test_hover_request_parsing() {
        let input = r#"{"jsonrpc":"2.0","id":"2","method":"textDocument/hover","params":{"textDocument":{"uri":"file:///home/jmmut/Documents/conan/pipes/pipes_programs/tests/test_piped_loop.pipes"},"position":{"line":4,"character":17}}}"#;
        let parsed = parse(input).unwrap();
        assert_eq!(parsed.method, Method::Hover);
        assert_eq!(unwrap_display(parsed.id()), "2");
    }

    #[test]
    fn test_hover_request_parsing_multiline() {
        let input = r#"{"jsonrpc":"2.0","id":"2","method":"textDocument/hover","params": {
          "textDocument": {
            "uri": "file:///home/jmmut/Documents/conan/pipes/corelib/array/sort.pipes"
          },
          "position": {
            "line": 22,
            "character": 25
          }
        }}"#;
        let parsed = parse(input).unwrap();
        assert_eq!(parsed.method, Method::Hover);
        assert_eq!(
            unwrap_display(parsed.location()),
            PositionInCode {
                absolute_char: -1,
                line: 23,
                char_in_line: 26
            }
        );
    }

    #[test]
    fn test_extract_position_simple() {
        let pos_input = r#"  "position": {    "line": 22,    "character": 25  }"#;
        let pos = extract_position(pos_input).unwrap();
        assert_eq!(
            pos,
            PositionInCode {
                absolute_char: -1,
                line: 23,
                char_in_line: 26
            }
        );
    }
    #[test]
    fn test_parse_next_number_simple() {
        let contains_number = r#"ne": 22,  "#;
        let number = parse_next_number(contains_number, 0);
        assert_eq!(number, 22);
    }
}