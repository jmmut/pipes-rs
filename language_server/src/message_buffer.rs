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
            Ok(usize::from_str(value)?) // TODO: trim?
        } else {
            err(format!("No '\r\n' after {}", CONTENT_LENGTH))
        }
    } else {
        err(format!("No '{}' in header", CONTENT_LENGTH))
    }
}
