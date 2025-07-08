use crate::common::{err, err_span, AnyError};
use crate::frontend::expression::{Type, TypedIdentifier};
use crate::frontend::parser::reverse_iterative_parser::err_expected_span;
use crate::frontend::sources::lexer::{lex_with_eof, TokenizedSource};
use crate::frontend::sources::location::{SourceCode, Span};
use crate::frontend::sources::token::{Keyword, LocatedToken, Operator, OperatorSpan, Token};
use std::iter::Peekable;
use std::vec::IntoIter;

pub enum NodeType {
    // Number(i64),
    // Operator(Operator),
    // Identifier(String),
    // String(Vec<u8>),
    // Keyword(Keyword),
    // OpenBracket,
}
#[rustfmt::skip]
#[derive(Debug)]
pub enum Node {
    Number { n: i64,span: Span },
    Keyword { keyword: Keyword, span: Span },
    String { bytes: Vec<u8>, span: Span },
    Identifier { name: String, span: Span },
    TypedIdentifier { typed_identifier: TypedIdentifier, span: Span },
    Types { subtypes: Nodes, span: Span },
    // Function { parts: Nodes, span: Span},
    Chain { operations: Nodes, span: Span },
    // List {elems: Nodes, span: Span},
    Operation { operator: OperatorSpan, operands: Nodes, span: Span },
}

pub type Nodes = Vec<Node>;

impl Node {
    pub fn span(&self) -> Span {
        match self {
            Node::Number { span, .. }
            | Node::Keyword { span, .. }
            | Node::String { span, .. }
            | Node::Identifier { span, .. }
            | Node::TypedIdentifier { span, .. }
            | Node::Types { span, .. }
            | Node::Chain { span, .. }
            | Node::Operation { span, .. } => *span,
        }
    }
}

pub fn read_from_str(text: &str) -> Result<(SourceCode, Node), AnyError> {
    let tokens = lex_with_eof(text)?;
    let source = tokens.source_code.clone();
    let node = read_toplevel(tokens)?;
    Ok((source, node))
}

pub fn read_toplevel(
    TokenizedSource {
        mut tokens,
        source_code,
    }: TokenizedSource,
) -> Result<Node, AnyError> {
    if tokens.len() == 0 {
        err("no tokens found")
    } else {
        // let mut iter = tokens.into_iter().peekable();
        let LocatedToken {token, span} = &tokens.first().as_ref().unwrap();
        let span_copy = *span;
        let (chain_res, mut iter) = if let Token::OpenBrace = token {
            let mut iter = tokens.into_iter().peekable();
            (read_chain(&mut iter, &source_code)?, iter)
        } else {
            let last = tokens.pop().unwrap();
            tokens.push(LocatedToken { token: Token::CloseBrace, span: last.span });
            tokens.push(last);
            let mut iter = tokens.into_iter().peekable();
            (read_chain_ops(&mut iter, &source_code, span_copy)?, iter)
        };

        let LocatedToken { token, span } = iter.next().unwrap();
        if let Token::EndOfFile = token {
            let remaining_tokens = iter.collect::<Vec<_>>();
            if remaining_tokens.len() != 0 {
                let span = remaining_tokens
                    .first()
                    .unwrap()
                    .span
                    .merge(&remaining_tokens.last().unwrap().span);
                err_span("unexpected tokens after chain", &source_code, span)
            } else {
                Ok(chain_res)
            }
        } else {
            err_expected_span(Token::EndOfFile.to_string(), token, &source_code, span)
        }
    }
}
type TokenIter = Peekable<IntoIter<LocatedToken>>;

fn read_chain(iter: &mut TokenIter, code: &SourceCode) -> Result<Node, AnyError> {
    let LocatedToken { token, span } = iter.next().unwrap();
    if let Token::OpenBrace = token {
        let open_span = span;
        read_chain_ops(iter, code, open_span)
    } else {
        let expected = format!("a chain starting with {}", Token::OpenBrace);
        err_expected_span(expected, token, code, span)
    }
}

fn read_chain_ops(
    iter: &mut TokenIter,
    code: &SourceCode,
    open_span: Span,
) -> Result<Node, AnyError> {
    let mut operations = Vec::new();
    let LocatedToken{token, span} = iter.peek().unwrap();
    if let Token::Operator(_) = token {
        // operator explicit: handle later in loop
    } else if let Token::CloseBrace = token {
        // empty chain: avoid an empty Ignore
    } else {
        let operator = OperatorSpan::new(Operator::Ignore, *span);
        let operands = read_operands(iter, code)?;
        let span = operation_span(operator, &operands);
        operations.push(Node::Operation { operator, operands, span });
    }
    loop {
        let LocatedToken { token, span } = iter.peek().unwrap();
        let span_copy = *span;
        if let Token::CloseBrace = token {
            let _ = iter.next();
            return Ok(Node::Chain {
                operations,
                span: open_span.merge(&span_copy),
            });
        } else {
            operations.push(read_operation(iter, code)?)
        }
        // match token {
        //     Token::CloseBrace => {
        //         let _ = iter.next();
        //         return Ok(Node::Chain { operations, span: open_span.merge(&span_copy) })
        //     },
        //     _ =>
        //         operations.push(read_operation(iter, code)?),
        // }
    }
}

fn read_operation(iter: &mut TokenIter, code: &SourceCode) -> Result<Node, AnyError> {
    let LocatedToken { token, span } = iter.next().unwrap();
    if let Token::Operator(operator) = token {
        let operator = OperatorSpan { operator, span };
        let operands = read_operands(iter, code)?;
        let span = operation_span(operator, &operands);
        Ok(Node::Operation { operator, operands, span })
    } else {
        err_expected_span(
            "operation (an operator and a list of operands)",
            token,
            code,
            span,
        )
    }
}

fn read_operands(iter: &mut TokenIter, code: &SourceCode) -> Result<Nodes, AnyError> {
    let mut operands = Vec::new();
    loop {
        let LocatedToken { token, .. } = iter.peek().unwrap();
        if let Token::Operator(_) | Token::CloseBrace /*|Token::CloseBracket | Token::CloseParenthesis*/ = token {
            return Ok(operands);
        } else {
            let expr = read_expr(iter, code)?;
            operands.push(expr);
        }
    }
}

fn operation_span(operator: OperatorSpan, operands: &Vec<Node>) -> Span {
    if let Some(last) = operands.last() {
        operator.span.merge(&last.span())
    } else {
        operator.span
    }
}

fn read_expr(iter: &mut TokenIter, code: &SourceCode) -> Result<Node, AnyError> {
    let LocatedToken { token, span } = iter.next().unwrap();
    match token {
        Token::Number(n) => Ok(Node::Number { n, span }),
        Token::Keyword(keyword) => Ok(Node::Keyword { keyword, span }),
        Token::Identifier(name) => Ok(Node::Identifier { name, span }),
        Token::String(bytes) => Ok(Node::String { bytes, span }),
        Token::OpenBrace => read_chain_ops(iter, code, span),
        Token::OpenBracket => {
            unimplemented!()
        }
        Token::OpenParenthesis => read_types(iter, code, span),
        Token::Operator(_)
        | Token::CloseBrace
        | Token::CloseBracket
        | Token::CloseParenthesis
        | Token::EndOfFile => err_expected_span("expression", token, code, span),
    }
}

fn read_types(iter: &mut TokenIter, code: &SourceCode, open_span: Span) -> Result<Node, AnyError> {
    let mut subtypes = Vec::new();
    loop {
        let LocatedToken { token, span } = iter.peek().unwrap();
        let span_copy = *span;
        if let Token::CloseParenthesis = token {
            let _ = iter.next();
            let span = open_span.merge(&span_copy);
            return Ok(Node::Types { subtypes, span });
        } else {
            subtypes.push(read_typed_identifier(iter, code)?)
        }
    }
}

fn read_typed_identifier(iter: &mut TokenIter, code: &SourceCode) -> Result<Node, AnyError> {
    let LocatedToken { token, span } = iter.next().unwrap();
    if let Token::Identifier(name) = token {
        let LocatedToken { token, span } = iter.peek().unwrap();
        if let Token::Operator(Operator::Type) = token {
            let _ = iter.next().unwrap(); // consume :
            let LocatedToken { token, span } = iter.next().unwrap();
            if let Token::Identifier(type_) = token {
                let children = maybe_read_children(iter, code)?;
                let type_ = Type::from(type_, children); // TODO: parse children types
                Ok(Node::TypedIdentifier {
                    typed_identifier: TypedIdentifier::new(name, type_),
                    span,
                })
            } else {
                err_expected_span("type name after ':'", token, code, span)
            }
        } else {
            Ok(Node::TypedIdentifier {
                typed_identifier: TypedIdentifier::any(name),
                span: *span,
            })
        }
    } else if let Token::Operator(Operator::Type) = token {
        let LocatedToken { token, span } = iter.next().unwrap();
        if let Token::Identifier(type_) = token {
            let children = maybe_read_children(iter, code)?;
            let type_ = Type::from(type_, children); // TODO: parse children types
            Ok(Node::TypedIdentifier {
                typed_identifier: TypedIdentifier::nameless(type_),
                span,
            })
        } else {
            err_expected_span("type name after ':'", token, code, span)
        }
    } else {
        err_expected_span(
            "'<name>' or ':<type>' or '<name> :<type>'",
            token,
            code,
            span,
        )
    }
}

fn maybe_read_children(
    iter: &mut TokenIter,
    code: &SourceCode,
) -> Result<Vec<TypedIdentifier>, AnyError> {
    let LocatedToken {
        token,
        span: children_span,
    } = iter.peek().unwrap();
    let children = if let Token::OpenParenthesis = token {
        let children_span = *children_span;
        let _ = iter.next().unwrap();
        let Node::Types { subtypes, .. } = read_types(iter, code, children_span)? else {
            panic!()
        };
        subtypes
            .into_iter()
            .map(|n| {
                let Node::TypedIdentifier {
                    typed_identifier, ..
                } = n
                else {
                    panic!()
                };
                typed_identifier
            })
            .collect()
    } else {
        Vec::new()
    };
    Ok(children)
}

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        use Node::*;
        match (self, other) {
            (Number { n, .. }, Number { n: n_2, .. }) => n == n_2,
            (Keyword { keyword, .. }, Keyword { keyword: p_2, .. }) => keyword == p_2,
            (Chain { operations, .. }, Chain { operations: _2, .. }) => operations == _2,
            (
                Operation {
                    operator, operands, ..
                },
                Operation {
                    operator: opt_2,
                    operands: opn_2,
                    ..
                },
            ) => operator == opt_2 && operands == opn_2,
            (Node::String { bytes, .. }, Node::String { bytes: b_2, .. }) => bytes == b_2,
            (Node::Identifier { name, .. }, Node::Identifier { name: n_2, .. }) => name == n_2,
            (
                Node::TypedIdentifier {
                    typed_identifier, ..
                },
                Node::TypedIdentifier {
                    typed_identifier: n_2,
                    ..
                },
            ) => typed_identifier == n_2,
            (Node::Types { subtypes, .. }, Node::Types { subtypes: s_2, .. }) => subtypes == s_2,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::unwrap_display;
    use crate::frontend::expression::TypedIdentifiers;
    use crate::frontend::sources::location::NO_SPAN;
    use crate::middleend::intrinsics::builtin_types;

    fn read(text :&str) -> Result<Node, AnyError> {
        Ok(read_from_str(text)?.1)
    }

    fn chain(subnodes: Nodes) -> Node {
        Node::Chain {
            operations: subnodes,
            span: NO_SPAN,
        }
    }
    fn empty_chain() -> Node {
        chain(Vec::new())
    }
    fn number(n: i64) -> Node {
        Node::Number { n, span: NO_SPAN }
    }
    fn op(operator: Operator, operands: Nodes) -> Node {
        Node::Operation {
            operator: OperatorSpan::spanless(operator),
            operands,
            span: NO_SPAN,
        }
    }
    fn ident(name: &str) -> Node {
        Node::Identifier {
            name: name.to_string(),
            span: NO_SPAN,
        }
    }
    fn ignore(operand: Node) -> Node {
        op(Operator::Ignore, vec![operand])
    }
    fn ignores(operands: Nodes) -> Node {
        op(Operator::Ignore, operands)
    }
    fn function() -> Node {
        Node::Keyword {
            keyword: Keyword::Function,
            span: NO_SPAN,
        }
    }
    fn i64(name: &str) -> Node {
        Node::TypedIdentifier {
            typed_identifier: TypedIdentifier::new(name.to_string(), builtin_types::I64),
            span: NO_SPAN,
        }
    }
    fn empty_types() -> Node {
        Node::Types {
            subtypes: vec![],
            span: NO_SPAN,
        }
    }
    fn types(subtypes: Nodes) -> Node {
        Node::Types {
            subtypes,
            span: NO_SPAN,
        }
    }
    // fn function(parts: Nodes) -> Node {
    //     Node::Function { parts , span: NO_SPAN }
    // }

    #[test]
    fn test_empty() {
        let text = "";
        // let _ = read(text).expect_err("");
        let read = unwrap_display(read(text));
        assert_eq!(read, empty_chain());
    }
    #[test]
    fn test_empty_chain() {
        let text = "{}";
        // let text = "chain[op[ignore 5] op[call function chain[op[ignore 4]]]]";
        // let text = "function(n 4) 4";
        // let text = "5 |function (n) {4}";

        let read = unwrap_display(read(text));
        assert_eq!(read, empty_chain());
    }
    #[test]
    fn test_number() {
        let text = "{;5}";
        let read = unwrap_display(read(text));
        assert_eq!(read, chain(vec![ignore(number(5))]));
    }
    #[test]
    fn test_function() {
        let text = "{;function{}}";
        let read = unwrap_display(read(text));
        assert_eq!(read, chain(vec![ignores(vec![function(), empty_chain()])]));
    }
    #[test]
    fn test_function_empty_types() {
        let text = "{;function(){}}";
        let read = unwrap_display(read(text));
        assert_eq!(
            read,
            chain(vec![ignores(vec![
                function(),
                empty_types(),
                empty_chain()
            ])])
        );
    }
    #[test]
    fn test_function_types() {
        let text = "{;function(a :i64){}}";
        let read = unwrap_display(read(text));
        assert_eq!(
            read,
            chain(vec![ignores(vec![
                function(),
                types(vec![i64("a")]),
                empty_chain()
            ])])
        );
    }
    #[test]
    fn test_nested_types() {
        let text = "{:tuple(t :tuple(a :i64 b :i64))}";
        let read = unwrap_display(read(text));
        assert_eq!(
            read,
            chain(vec![op(
                Operator::Type,
                vec![
                    ident("tuple"),
                    types(vec![Node::TypedIdentifier {
                        typed_identifier: TypedIdentifier::new(
                            "t".to_string(),
                            Type::children(
                                "tuple",
                                vec![
                                    TypedIdentifier::new("a".to_string(), builtin_types::I64),
                                    TypedIdentifier::new("b".to_string(), builtin_types::I64),
                                ]
                            )
                        ),
                        span: NO_SPAN
                    }]),
                ]
            )])
        );
    }
    #[test]
    fn test_expected_operator() {
        read("{;").expect_err("should fail but was");
        read("{;)").expect_err("should fail but was");
    }
}
