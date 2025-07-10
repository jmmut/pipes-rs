use crate::analyzer::*;
use crate::request::*;
use pipes_rs::common::AnyError;
use pipes_rs::frontend::expression::{Expression, ExpressionSpan};
use pipes_rs::frontend::lex_and_parse;
use pipes_rs::frontend::sources::location::SourceCode;
use pipes_rs::middleend::typing::put_types;
use std::path::PathBuf;

fn generate_error_unimplemented() -> String {
    r#"
{
    "jsonrpc": "2.0",
    "id": 1,
    "error": {
        "code": -32601,
        "message": "unimplemented Pipes language server"
    }
}
"#
    .to_string()
}

fn _generate_notification(notification: &str) -> String {
    format!(
        r#"
{{
    "jsonrpc": "2.0",
    "method": "window/showMessage",
    "params": {{
        "type": 2,
        "message": "{}"
    }}
}}
"#,
        notification
    )
}

pub fn generate_message_with_header(body: &str) -> String {
    format!(
        "Content-Length: {}\r\nContent-Type: application/vscode-jsonrpc; charset=utf-8\r\n\r\n{}",
        body.len(),
        body
    )
}

fn generate_response_initialize(id: &str) -> String {
    format!(
        r#"
{{
    "jsonrpc": "2.0",
    "id": "{}",
    "result": {{
        "capabilities": {{
            "textDocumentSync": {{ "change": 0 }},
            "hoverProvider": true
        }}
    }}
}}
"#,
        id
    )
}

fn quote(s: &str) -> String {
    format!("'{}'", s)
}

fn syn_type_kind(expression: &ExpressionSpan) -> &'static str {
    match expression.syn_type() {
        Expression::Nothing => "Nothing",
        Expression::Value(_) => "Value",
        Expression::Identifier(_) => "Identifier",
        Expression::Type(_) => "Type",
        Expression::TypedIdentifiers(_) => "TypedIdentifiers",
        Expression::Chain(_) => "Chain",
        Expression::StaticList { .. } => "StaticList",
        Expression::Function(_) => "Function",
        Expression::Composed(_) => "Composed",
        Expression::Abstract(_) => "Abstract",
    }
}
fn format_hover_description(expression: &ExpressionSpan) -> String {
    let mut description = format!("Expression type: {}", syn_type_kind(expression));

    if let Expression::Identifier(name) = expression.syn_type() {
        description += &format!(" {}", quote(name));
    };

    // if expression.get_type() == BINARY_OPERATION {
    //     description += &format!(" {}", quote(&expression.get_binary_operation().operation.to_string()));
    // }

    let type_description = format!("Semantic type: `{}`", expression.sem_type());
    //
    // if matches!(
    //     expression.syn_type(),
    //     CALL | BRANCH | WHILE | BINARY_OPERATION
    // ) {
    //     type_description = format!("result {}", type_description);
    // }
    let serialized = format!("Parsed expression: {}", expression.to_string());

    format!("{}.\n{}.\n{}.", description, type_description, serialized)
}

fn generate_response_hover(request: &Request) -> Result<String, AnyError> {
    let code_string = SourceCode::new(PathBuf::from(request.file()?))?;
    let mut program = lex_and_parse(code_string)?;

    put_types(&mut program)?;
    let expression = find_expression_at(&program, request.location().unwrap())?;

    let pipes_type = format_hover_description(&expression);

    let formatted = format!(
        r#"
{{
    "jsonrpc": "2.0",
    "id": "{}",
    "result": {{
        "contents": {{
            "kind": "markdown",
            "value": "{}"
        }}
    }}
}}
"#,
        request.id().unwrap(),
        pipes_type
    );
    Ok(formatted)
}

fn generate_response_shutdown(id: &str) -> String {
    format!(
        r#"
{{
    "jsonrpc": "2.0",
    "id": "{}",
    "result": null
}}
"#,
        id
    )
}

pub fn choose_response(request: &Request) -> Result<Option<String>, AnyError> {
    match request.method {
        Method::Unknown => Ok(Some(generate_error_unimplemented())),
        Method::Initialize => Ok(Some(generate_response_initialize(request.id()?))),
        Method::Initialized => Ok(None),
        Method::Hover => Ok(Some(generate_response_hover(request)?)),
        Method::Shutdown => Ok(Some(generate_response_shutdown(request.id()?))),
    }
}

#[cfg(test)]
pub fn uglify(mut s: String) -> String {
    s.retain(|c| !(c == ' ' || c == '\n'));
    s
}

pub fn response(request: &str) -> Result<Option<String>, AnyError> {
    let parsed = parse(request)?;
    let response_body = choose_response(&parsed)?;
    if let Some(message) = response_body {
        Ok(Some(generate_message_with_header(&message)))
    } else {
        Ok(response_body)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_header_generate() {
        let actual = generate_message_with_header("asdf");
        let expected = "Content-Length: 4\r\nContent-Type: application/vscode-jsonrpc; charset=utf-8\r\n\r\nasdf";
        assert_eq!(actual, expected);
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
}
