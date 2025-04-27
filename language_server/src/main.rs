mod analyzer;
mod message_buffer;
mod request;
mod response;

fn main() {
    println!("Hello from language server");
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analyzer::find_expression_at;
    use crate::message_buffer::{MessageBuffer, extract_body_size_from_header};
    use crate::request::{
        Method, PositionInCode, extract_method, extract_position, extract_str, parse,
        parse_next_number,
    };
    use crate::response::{choose_response, generate_message_with_header, response, uglify};
    use pipes_rs::backend::evaluate::GenericValue;
    use pipes_rs::common::unwrap_display;
    use pipes_rs::frontend::expression::{Chain, Expression, Function, Operation, TypedIdentifier};
    use pipes_rs::frontend::program::Program;
    use pipes_rs::frontend::sources::location::{Location, SourceCode, Span};
    use pipes_rs::frontend::sources::token::{Operator, OperatorSpan};
    use pipes_rs::frontend::{lex_and_parse, parse_type};
    use pipes_rs::middleend::intrinsics::builtin_types;
    use pipes_rs::middleend::typing::put_types;

    #[test]
    fn test_basic_header_generate() {
        let actual = generate_message_with_header("asdf");
        let expected = "Content-Length: 4\r\nContent-Type: application/vscode-jsonrpc; charset=utf-8\r\n\r\nasdf";
        assert_eq!(actual, expected);
    }

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
    #[should_panic(expected = "InvalidHeader")]
    fn test_extract_body_size_invalid() {
        let header =
            "Content-Length: \r\nContent-Type: application/vscode-jsonrpc; charset=utf-8\r\n\r\n";
        extract_body_size_from_header(header.as_bytes()).unwrap();
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
    fn test_initialize_response_generation() {
        let input_method_initialize =
            r#"{"jsonrpc":"2.0","id":"1","method":"initialize","params":{}}"#;
        let request = generate_message_with_header(input_method_initialize);
        let _answer = response(&request);
        // Not asserting content for now (was commented in C++)
    }

    #[test]
    fn test_uglify_simple_json() {
        let input = "\n{\n  \"asdf\": \"qwer\",\n  \"a\": {}\n}\n\n";
        let uglified = uglify(input.to_string());
        assert_eq!(uglified, r#"{"asdf":"qwer","a":{}}"#);
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
                line: 22,
                char_in_line: 25
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
                line: 22,
                char_in_line: 25
            }
        );
    }

    #[test]
    fn test_parse_next_number_simple() {
        let contains_number = r#"ne": 22,  "#;
        let number = parse_next_number(contains_number, 0);
        assert_eq!(number, 22);
    }

    fn build_program<S: Into<SourceCode>>(code_text: S) -> Program {
        let mut program = unwrap_display(lex_and_parse(code_text));
        unwrap_display(put_types(&mut program));
        program
    }

    #[test]
    fn test_get_symbol() {
        let program = build_program("function(:i64)(:i64) { 3 } | function(my_func) {5 |my_func}");
        let expression = unwrap_display(find_expression_at(
            &program,
            PositionInCode {
                absolute_char: -1,
                line: 0,
                char_in_line: 41,
            },
        ));
        // let nameless_int = TypedIdentifier::nameless(builtin_types::I64);
        // let location = Location::from(1, 23, 23);
        // let called_function = Expression::Function(Function {
        //     parameters: vec![nameless_int],
        //     returned: nameless_int,
        //     body: Chain {
        //         operations: vec![Operation {
        //             operator: OperatorSpan::new(Operator::Ignore, Span { start: location, end: location }),
        //             operands: vec![],
        //             sem_type: nameless_int.type_,
        //         }]
        //     },
        //
        // });
        let Expression::Function(..) = expression.syn_type() else {
            panic!("expression was not a function: {}", expression.syn_type());
        };
        assert_eq!(
            *expression.sem_type(),
            parse_type("function(my_func :function(:int64)(:int64))(:int64)"),
        );
    }

    #[test]
    fn test_json_response_balance() {
        let input = r#"{"jsonrpc":"2.0","id":"2","method":"textDocument/hover","params":{"textDocument":{"uri":"file://../../../pipes_programs/tests/test_piped_loop.pipes"},"position":{"line":4,"character":15}}}"#;
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
