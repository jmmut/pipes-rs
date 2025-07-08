use std::collections::HashMap;
use crate::common::{context, err_span, AnyError};
use crate::frontend::expression::{Chain, Expression, ExpressionSpan, Operation};
use crate::frontend::parser::recursive_reader::{read_toplevel, Node, Nodes};
use crate::frontend::parser::reverse_iterative_parser::Parser;
use crate::frontend::program::{Identifiers, Program};
use crate::frontend::sources::lexer::TokenizedSource;
use crate::frontend::sources::location::{SourceCode, Span};
use crate::frontend::sources::Sources;
use crate::frontend::sources::token::Keyword;

pub fn parse_tokens(tokens: TokenizedSource) -> Result<Program, AnyError> {
    let code = tokens.source_code.clone();
    let node = read_toplevel(tokens)?;
    parse(node, code)
}
pub fn parse(node: Node, code: SourceCode) -> Result<Program, AnyError> {
    context("Nodes to Expression", parse_internal(node, code))
}

fn parse_internal(node: Node, code: SourceCode) -> Result<Program, AnyError> {
    let main = parse_recursive(node, &code)?;
    Ok(Program::new_from(main, Identifiers::new(), Sources::new(code, HashMap::new())))
}

fn parse_recursive(node: Node, code: &SourceCode) -> Result<ExpressionSpan, AnyError> {
    let (expression, span) = match node {
        Node::Number { n, span } => { (Expression::Value(n), span) }
        Node::Chain { operations, span } => { construct_chain(operations, span, code)? }
        Node::Keyword { keyword, span } => {construct_keyword(keyword, span)?}
        _ => unimplemented!()
        // Node::String { .. } => {}
        // Node::Identifier { .. } => {}
        // Node::TypedIdentifier { .. } => {}
        // Node::Types { .. } => {}
        // Node::Operation { .. } => {}
    };
    Ok(ExpressionSpan::new_typeless(expression, span))
}

fn construct_chain(nodes: Nodes, span: Span, code: &SourceCode) -> Result<(Expression, Span), AnyError> {
    let mut operations = Vec::new();
    for node in nodes {
        if let Node::Operation { operator, operands, span } = node {
            let mut operand_exprs = Vec::new();
            for operand in operands {
                operand_exprs.push(parse_recursive(operand, code)?);
            }
            operations.push(Operation::several_no_sem_type(operator, operand_exprs));
        } else {
            return err_span("Bug: expected operation at this point", code , node.span());
        }
    }
    Ok((Expression::chain(operations), span))
}

fn construct_keyword(
    keyword: Keyword,
    span: Span,
) -> Result<(Expression, Span), AnyError> {
    let (expr, content_span) = match keyword {
        Keyword::Nothing => Ok::<(Expression, Span), AnyError>((Expression::Nothing, span)),
        // Keyword::Function => construct_function(),
        _ => unimplemented!()
        // Keyword::Loop => construct_loop(parser),
        // Keyword::Browse => construct_browse(parser),
        // Keyword::BrowseOr => construct_browse_or(parser),
        // Keyword::Times => construct_times(parser),
        // Keyword::TimesOr => construct_times_or(parser),
        // Keyword::Replace => construct_replace(parser),
        // Keyword::Map => construct_map(parser),
        // Keyword::Filter => construct_filter(parser),
        // Keyword::Branch => construct_branch(parser),
        // Keyword::Something => construct_something(parser),
        // Keyword::Inspect => construct_inspect(parser),
        // Keyword::Public => construct_public(parser),
        // Keyword::Cast => construct_cast(parser),
        // Keyword::Comptime => construct_comptime(parser),
    }?;
    Ok((expr, span.merge(&content_span)))
}
// fn construct_function() -> _ {
//     todo!()
// }

 
