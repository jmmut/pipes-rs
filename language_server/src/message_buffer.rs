use pipes_rs::common::{AnyError, err};
use std::str::FromStr;

pub struct MessageBuffer {
    buffer_header: Vec<u8>,
    buffer_body: Vec<u8>,
    header_complete: bool,
    body_size: usize,
}

impl MessageBuffer {
    pub fn new() -> Self {
        Self {
            buffer_header: Vec::new(),
            buffer_body: Vec::new(),
            header_complete: false,
            body_size: 0,
        }
    }

    pub fn handle_char(&mut self, c: u8) -> Result<(), AnyError> {
        if !self.header_complete {
            self.buffer_header.push(c);
            if self.did_header_just_complete() {
                self.header_complete = true;
                self.body_size = extract_body_size_from_header(&self.buffer_header)?;
            }
        } else if self.buffer_body.len() < self.body_size {
            self.buffer_body.push(c);
        } else {
            return err("header and body are already complete, \
             you should have reset MessageBuffer before passing more data");
        }
        Ok(())
    }

    pub fn is_message_complete(&self) -> bool {
        self.header_complete && self.buffer_body.len() == self.body_size
    }

    pub fn body(&self) -> Result<String, AnyError> {
        Ok(String::from_utf8(self.buffer_body.clone())?)
    }

    pub fn reset(&mut self) {
        self.buffer_header.clear();
        self.buffer_body.clear();
        self.header_complete = false;
        self.body_size = 0;
    }

    fn did_header_just_complete(&self) -> bool {
        self.buffer_header.ends_with(b"\r\n\r\n")
    }
}

pub fn extract_body_size_from_header(header: &[u8]) -> Result<usize, AnyError> {
    let header_str = String::from_utf8(header.to_vec())?;
    const CONTENT_LENGTH: &'static str = "Content-Length:";
    if let Some(content_length_start) = header_str.find(CONTENT_LENGTH) {
        if let Some(end) = header_str[content_length_start..].find("\r\n") {
            let value = &header_str[content_length_start + CONTENT_LENGTH.len()..end];
            Ok(usize::from_str(value.trim())?) // TODO: trim?
        } else {
            err(format!("No '\r\n' after {}", CONTENT_LENGTH))
        }
    } else {
        err(format!("No '{}' in header", CONTENT_LENGTH))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_basic_header_buffer_incremental() {
        let message = "Content-Length: 4\r\nContent-Type: application/vscode-jsonrpc; charset=utf-8\r\n\r\nasdf";
        let mut buffer = MessageBuffer::new();
        for (i, &c) in message
            .as_bytes()
            .iter()
            .enumerate()
            .take(message.len() - 1)
        {
            buffer.handle_char(c).unwrap();
            assert_eq!(buffer.is_message_complete(), false, "failed at char {}", i);
        }
        buffer
            .handle_char(*message.as_bytes().last().unwrap())
            .unwrap();
        assert_eq!(buffer.is_message_complete(), true);
    }

    #[test]
    fn test_extract_body_size_simple() {
        let header =
            "Content-Length: 4\r\nContent-Type: application/vscode-jsonrpc; charset=utf-8\r\n\r\n";
        let size = extract_body_size_from_header(header.as_bytes()).unwrap();
        assert_eq!(size, 4);
    }

    #[test]
    fn test_extract_body_size_longer() {
        let header =
            "Content-Length: 14\r\nContent-Type: application/vscode-jsonrpc; charset=utf-8\r\n\r\n";
        let size = extract_body_size_from_header(header.as_bytes()).unwrap();
        assert_eq!(size, 14);
    }

    #[test]
    fn test_extract_body_size_invalid() {
        let header =
            "Content-Length: \r\nContent-Type: application/vscode-jsonrpc; charset=utf-8\r\n\r\n";
        let extracted = extract_body_size_from_header(header.as_bytes());
        extracted.expect_err("should fail");
    }

    #[test]
    fn test_full_request_body_parsing() {
        let full_request = "Content-Length: 4\r\nContent-Type: application/vscode-jsonrpc; charset=utf-8\r\n\r\nasdf";
        let mut buffer = MessageBuffer::new();
        for &c in full_request.as_bytes() {
            buffer.handle_char(c).unwrap();
        }
        assert!(buffer.is_message_complete());
        assert_eq!(buffer.body().unwrap(), "asdf");
    }

    #[test]
    fn test_multiple_requests_in_a_row() {
        let full_request = "Content-Length: 4\r\nContent-Type: application/vscode-jsonrpc; charset=utf-8\r\n\r\nasdf";
        let mut buffer = MessageBuffer::new();
        let count = 2;
        let mut completed = 0;

        for _ in 0..count {
            for &c in full_request.as_bytes() {
                buffer.handle_char(c).unwrap();
                if buffer.is_message_complete() {
                    completed += 1;
                    buffer.reset();
                }
            }
        }
        assert_eq!(completed, count);
    }
}
