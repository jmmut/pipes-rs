use crate::common::{err, err_span, AnyError};
use crate::frontend::expression::{Type, TypedIdentifier};
use crate::frontend::parser::reverse_iterative_parser::{err_expected_span};
use crate::frontend::sources::lexer::{lex_with_eof, TokenizedSource};
use crate::frontend::sources::location::{SourceCode, Span};
use crate::frontend::sources::token::{
    Keyword, LocatedToken, Operator, OperatorSpan, Token,
};
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
#[derive(Debug)]
pub enum Node {
    Number {
        n: i64,
        span: Span,
    },
    Keyword {
        keyword: Keyword,
        span: Span,
    },
    String {
        bytes: Vec<u8>,
        span: Span,
    },
    Identifier {
        name: String,
        span: Span,
    },
    TypedIdentifier {
        typed_identifier: TypedIdentifier,
        span: Span,
    },
    Types {
        subtypes: Nodes,
        span: Span,
    },
    // Function { parts: Nodes, span: Span},
    Chain {
        operations: Nodes,
        span: Span,
    },
    // List {elems: Nodes, span: Span},
    Operation {
        operator: OperatorSpan,
        operands: Nodes,
        span: Span,
    },
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

pub fn read(text: &str) -> Result<Node, AnyError> {
    let tokens = lex_with_eof(text)?;
    read_toplevel(tokens)
}

fn read_toplevel(
    TokenizedSource {
        tokens,
        source_code,
    }: TokenizedSource,
) -> Result<Node, AnyError> {
    if tokens.len() == 0 {
        err("no tokens found")
    } else {
        let mut iter = tokens.into_iter().peekable();
        let chain_res = read_chain(&mut iter, &source_code)?;

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
        let mut operands = Vec::new();
        loop {
            let LocatedToken { token, .. } = iter.peek().unwrap();
            if let Token::Operator(_) | Token::CloseBrace /*|Token::CloseBracket | Token::CloseParenthesis*/ = token {
                let span = operation_span(operator, &mut operands);
                return Ok(Node::Operation { operator, operands, span })
            } else {
                let expr = read_expr(iter, code)?;
                operands.push(expr);
            }
        }
    } else {
        err_expected_span(
            "operation (an operator and a list of operands)",
            token,
            code,
            span,
        )
    }
}

fn operation_span(operator: OperatorSpan, operands: &mut Vec<Node>) -> Span {
    if let Some(last) = operands.last() {
        operator.span.merge(&last.span())
    } else {
        operator.span
    }
}

fn read_expr(iter: &mut TokenIter, code: &SourceCode) -> Result<Node, AnyError> {
    match iter.next().unwrap() {
        LocatedToken { token, span } => match token {
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
        },
    }
}

fn read_types(iter: &mut TokenIter, code: &SourceCode, open_span: Span) -> Result<Node, AnyError> {
    let mut subtypes = Vec::new();
    loop {
        let LocatedToken { token, span } = iter.peek().unwrap();
        let span_copy = *span;
        match token {
            Token::CloseParenthesis => {
                let _ = iter.next();
                return Ok(Node::Types {
                    subtypes,
                    span: open_span.merge(&span_copy),
                });
            }
            _ => subtypes.push(read_typed_identifier(iter, code)?),
        }
    }
}

fn read_typed_identifier(iter: &mut TokenIter, code: &SourceCode) -> Result<Node, AnyError> {
    let LocatedToken { token, span } = iter.next().unwrap();
    match token {
        Token::Identifier(name) => {
            let LocatedToken { token, span } = iter.peek().unwrap();
            match token {
                Token::Operator(Operator::Type) => {
                    let _ = iter.next().unwrap(); // consume :
                    let LocatedToken { token, span } = iter.next().unwrap();
                    match token {
                        Token::Identifier(type_) => {
                            let type_ = Type::from(type_, Vec::new()); // TODO: parse children types
                            Ok(Node::TypedIdentifier {
                                typed_identifier: TypedIdentifier::new(name, type_),
                                span,
                            })
                        }
                        actual => err_expected_span("type name after ':'", actual, code, span),
                    }
                }
                _ => Ok(Node::TypedIdentifier {
                    typed_identifier: TypedIdentifier::any(name),
                    span: *span,
                }),
            }
        }
        Token::Operator(Operator::Type) => {
            let LocatedToken { token, span } = iter.next().unwrap();
            match token {
                Token::Identifier(type_) => {
                    let type_ = Type::from(type_, Vec::new()); // TODO: parse children types
                    Ok(Node::TypedIdentifier {
                        typed_identifier: TypedIdentifier::nameless(type_),
                        span,
                    })
                }
                actual => err_expected_span("type name after ':'", actual, code, span),
            }
        }
        actual => err_expected_span(
            "'<name>' or ':<type>' or '<name> :<type>'",
            actual,
            code,
            span,
        ),
    }
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
    fn empty_types() -> Node {
        Node::Types {
            subtypes: vec![],
            span: NO_SPAN,
        }
    }
    fn types(typed_identifiers: TypedIdentifiers) -> Node {
        let subtypes = typed_identifiers
            .into_iter()
            .map(|t| Node::TypedIdentifier {
                typed_identifier: t,
                span: NO_SPAN,
            })
            .collect();
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
        let _ = read(text).expect_err("");
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
                types(vec![TypedIdentifier::new(
                    "a".to_string(),
                    builtin_types::I64
                )]),
                empty_chain()
            ])])
        );
    }
    #[test]
    fn test_expected_operator() {
        read("{;").expect_err("should fail but was");
        read("{;)").expect_err("should fail but was");
    }
}

/*
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::{Display, Formatter};
use std::path::PathBuf;

use crate::common::{context, err, err_span, AnyError};
use crate::frontend::expression::display::typed_identifiers_to_str;
use crate::frontend::expression::{
    take_single, Branch, Cast, Chain, Composed, Comptime, Expression, ExpressionSpan, Operation,
    Operations, Type, TypedIdentifier, TypedIdentifiers,
};
use crate::frontend::parser::import::import;
use crate::frontend::parser::root::{get_project_root, qualify};
use crate::frontend::program::{IncompleteProgram, Program};
use crate::frontend::sources::lexer::TokenizedSource;
use crate::frontend::sources::location::{SourceCode, Span, NO_SPAN};
use crate::frontend::sources::token::{
    Keyword, LocatedToken, LocatedTokens, Operator, OperatorSpan, Token,
};
use crate::frontend::sources::Sources;
use crate::middleend::intrinsics::builtin_types;

pub fn parse_tokens(tokens: TokenizedSource) -> Result<Program, AnyError> {
    context("Reverse parser", Parser::parse_tokens(tokens))
}
pub fn parse_type(tokens: TokenizedSource) -> Result<Type, AnyError> {
    context("Reverse type parser", Parser::parse_type(tokens))
}

pub fn parse_tokens_cached(tokens: LocatedTokens, ast: Parser) -> Result<Program, AnyError> {
    Ok(parse_tokens_cached_inner(tokens, ast)?.into())
}

pub fn parse_tokens_cached_inner(
    tokens: LocatedTokens,
    mut ast: Parser,
) -> Result<IncompleteProgram, AnyError> {
    ast.raw_parse_tokens(tokens)?;
    finish_construction(ast)
}

#[derive(Debug)]
pub enum PartialExpression {
    // OpenBracket(Span),
    CloseBracket(Span),
    // OpenBrace(Span),
    CloseBrace(Span),
    // OpenParenthesis(Span),
    CloseParenthesis(Span),
    Expression(ExpressionSpan),
    Operation(Operation),
    // Operator(Operator),
    // Keyword(Keyword),
    ChildrenTypes(TypedIdentifiers, Span),
    // TypedIdentifier(TypedIdentifier),
}

impl PartialExpression {
    pub fn expression(e: Expression, span: Span) -> PartialExpression {
        PartialExpression::Expression(ExpressionSpan::new_typeless(e, span))
    }
    pub fn span(&self) -> Span {
        match self {
            PartialExpression::CloseBracket(span)
            | PartialExpression::CloseBrace(span)
            | PartialExpression::CloseParenthesis(span)
            | PartialExpression::ChildrenTypes(_, span) => *span,
            PartialExpression::Expression(expr_span) => expr_span.span(),
            PartialExpression::Operation(op) => op.content_span(),
        }
    }
}
impl Display for PartialExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            // PartialExpression::OpenBracket(_) => write!(f, "["),
            PartialExpression::CloseBracket(_) => write!(f, "]"),
            // PartialExpression::OpenBrace(_) => write!(f, "{{"),
            PartialExpression::CloseBrace(_) => write!(f, "}}"),
            // PartialExpression::OpenParenthesis(_) => write!(f, "("),
            PartialExpression::CloseParenthesis(_) => write!(f, ")"),
            PartialExpression::Expression(expr) => write!(f, "{}", expr),
            PartialExpression::Operation(op) => write!(f, "{}", op),
            // PartialExpression::Operator(op) => write!(f, "{}", op),
            // PartialExpression::Keyword(keyword) => write!(f, "{}", keyword.name()),
            PartialExpression::ChildrenTypes(types, _) => {
                write!(f, "{}", typed_identifiers_to_str(types, true))
            } // PartialExpression::TypedIdentifier(name) => write!(f, "{}", name),
        }
    }
}

pub struct Parser {
    pub accumulated: VecDeque<PartialExpression>,
    pub exported: HashMap<String, ExpressionSpan>,
    pub available: HashSet<String>,
    pub root: Option<PathBuf>,
    pub source: SourceCode,
}

impl Parser {
    pub fn new(source: SourceCode) -> Self {
        let root = get_project_root(&None, &source.file);
        Self::new_with_available(source, HashSet::new(), root.ok()) // TODO: .ok() loses error message
    }
    pub fn new_with_available(
        source: SourceCode,
        available: HashSet<String>,
        root: Option<PathBuf>,
    ) -> Self {
        Self {
            accumulated: VecDeque::new(),
            exported: HashMap::new(),
            available,
            root,
            source,
        }
    }

    pub fn raw_parse_tokens(&mut self, tokens: LocatedTokens) -> Result<(), AnyError> {
        for LocatedToken { token, span } in tokens.into_iter().rev() {
            match token {
                Token::Number(n) => self.push(Expression::Value(n), span),
                Token::Operator(operator) => {
                    let pe = construct_transformation(self, operator, span)?;
                    self.accumulated.push_front(pe);
                }
                Token::Identifier(ident) => {
                    let expression_span = construct_identifier(self, ident, span);
                    self.push_es(expression_span);
                }
                Token::Keyword(keyword) => {
                    let (expr, span) = construct_keyword(self, keyword, span)?;
                    self.push(expr, span);
                }
                Token::OpenBrace => self.push_f(construct_chain, span)?,
                Token::CloseBrace => self.push_pe(PartialExpression::CloseBrace(span)),
                Token::OpenBracket => self.push_f(construct_array, span)?,
                Token::CloseBracket => self.push_pe(PartialExpression::CloseBracket(span)),
                Token::OpenParenthesis => self.push_f_pe(construct_children_types, span)?,
                Token::CloseParenthesis => self.push_pe(PartialExpression::CloseParenthesis(span)),
                Token::String(string) => self.push_pe(construct_string(string, span)), // _ => return error_expected("anything else", token),
            };
        }
        Ok(())
    }
    fn parse_tokens(tokens: TokenizedSource) -> Result<Program, AnyError> {
        let parser = Parser::new(tokens.source_code);
        parse_tokens_cached(tokens.tokens, parser)
    }

    fn parse_type(mut tokens: TokenizedSource) -> Result<Type, AnyError> {
        let mut parser = Parser::new(tokens.source_code);
        tokens
            .tokens
            .insert(0, LocatedToken::spanless(Token::Operator(Operator::Type)));
        parser.raw_parse_tokens(tokens.tokens)?;
        let expression = parser.accumulated.pop_front();
        if !parser.accumulated.is_empty() {
            err(format!(
                "Could not parse as a type because there are extra unused expressions: {:?}",
                parser.accumulated
            ))
        } else if let Some(PartialExpression::Operation(Operation {
            operator:
                OperatorSpan {
                    operator: Operator::Type,
                    ..
                },
            operands,
            ..
        })) = expression
        {
            let operand = take_single(operands);
            match operand {
                Some(ExpressionSpan {
                    syntactic_type: Expression::Type(type_),
                    ..
                }) => Ok(type_),
                _ => err(format!(
                    "Could not parse as a type because resulting expression is not a type: {:?}",
                    operand
                )),
            }
        } else {
            err(format!(
                "Could not parse as a type because resulting expression is not a type: {:?}",
                expression
            ))
        }
    }

    fn push_f(
        &mut self,
        construct_expression: fn(
            &mut VecDeque<PartialExpression>,
            source: &SourceCode,
            span: Span,
        ) -> Result<ExpressionSpan, AnyError>,
        span: Span,
    ) -> Result<(), AnyError> {
        let expression = construct_expression(&mut self.accumulated, &self.source, span)?;
        self.push_es(expression);
        Ok(())
    }
    fn push_f_pe(
        &mut self,
        construct_expression: fn(
            &mut VecDeque<PartialExpression>,
            source: &SourceCode,
            span: Span,
        ) -> Result<PartialExpression, AnyError>,
        span: Span,
    ) -> Result<(), AnyError> {
        let expression = construct_expression(&mut self.accumulated, &self.source, span)?;
        self.push_pe(expression);
        Ok(())
    }

    fn push(&mut self, expression: Expression, span: Span) {
        self.push_pe(PartialExpression::Expression(ExpressionSpan::new_typeless(
            expression, span,
        )));
    }
    fn push_es(&mut self, expression_span: ExpressionSpan) {
        self.push_pe(PartialExpression::Expression(expression_span));
    }
    fn push_pe(&mut self, partial_expression: PartialExpression) {
        self.accumulated.push_front(partial_expression);
    }
}

fn construct_identifier(parser: &mut Parser, identifier: String, mut span: Span) -> ExpressionSpan {
    let maybe_children = parser.accumulated.pop_front();
    let expr = match maybe_children {
        Some(PartialExpression::ChildrenTypes(children, children_span)) => {
            span = span.merge(&children_span);
            Expression::Type(Type::from(identifier, children))
        }
        Some(not_children) => {
            parser.accumulated.push_front(not_children);
            Expression::Identifier(identifier)
        }
        None => Expression::Identifier(identifier),
    };
    ExpressionSpan::new_typeless(expr, span)
}

fn construct_transformation(
    parser: &mut Parser,
    raw_operator: Operator,
    span: Span,
) -> Result<PartialExpression, AnyError> {
    let operator = OperatorSpan {
        operator: raw_operator,
        span,
    };
    let accumulated = &mut parser.accumulated;
    let mut elem_operand = accumulated.pop_front();
    let transformation = if let Operator::Type = raw_operator {
        if let Some(PartialExpression::Expression(ExpressionSpan {
            syntactic_type: Expression::Identifier(typename),
            span,
            ..
        })) = elem_operand
        {
            let mut operand = get_type_maybe_pop_children(accumulated, typename);
            operand.span = operand.span.merge(&span);
            Operation::single_no_sem_type(operator, operand)
        } else if let Some(PartialExpression::Expression(ExpressionSpan {
            syntactic_type: Expression::Type(type_),
            ..
        })) = elem_operand
        {
            let operand = ExpressionSpan::new_spanless(Expression::Type(type_));
            Operation::single_no_sem_type(operator, operand)
        } else {
            error_expected("type or type name after type operator ':'", elem_operand)?
        }
    } else if Operator::Call == raw_operator || Operator::Concatenate == raw_operator {
        let mut operands = Vec::new();
        while let Some(PartialExpression::Expression(expr)) = elem_operand {
            elem_operand = accumulated.pop_front();
            operands.push(expr);
        }
        if let Some(elem) = elem_operand {
            accumulated.push_front(elem);
        }
        if operands.len() > 0 {
            let last_expr_span = operands.last().unwrap().span;
            Operation::several_no_sem_type(
                OperatorSpan {
                    operator: raw_operator,
                    span: span.merge(&last_expr_span),
                },
                operands,
            )
        } else {
            let actual = accumulated.front();
            err(expected("some callable expression", actual))?
        }
    } else {
        if let Some(PartialExpression::Expression(operand)) = elem_operand {
            Operation::single_no_sem_type(operator, operand)
        } else if raw_operator == Operator::Ignore {
            let operand = if let None = elem_operand {
                Expression::empty_chain()
            } else if let Some(PartialExpression::CloseBrace(span)) = elem_operand {
                accumulated.push_front(PartialExpression::CloseBrace(span));
                Expression::empty_chain()
            } else {
                err(expected_span(
                    "expression or closing brace or end of file",
                    elem_operand,
                    &parser.source,
                    operator.span,
                ))?
            };
            let operand = ExpressionSpan::new_spanless(operand);
            Operation::single_no_sem_type(operator, operand)
        } else {
            error_expected("operand after operator", elem_operand)?
        }
    };
    Ok(PartialExpression::Operation(transformation))
}

fn get_type_maybe_pop_children(
    accumulated: &mut VecDeque<PartialExpression>,
    typename: String,
) -> ExpressionSpan {
    let maybe_children = accumulated.pop_front();
    match maybe_children {
        Some(PartialExpression::ChildrenTypes(children, span)) => {
            return ExpressionSpan::new_typeless(
                Expression::Type(Type::from(typename, children)),
                span,
            );
        }
        Some(not_children) => accumulated.push_front(not_children),
        None => {}
    }
    ExpressionSpan::new_spanless(Expression::Type(Type::simple(typename)))
}

fn construct_keyword(
    parser: &mut Parser,
    keyword: Keyword,
    span: Span,
) -> Result<(Expression, Span), AnyError> {
    let (expr, content_span) = match keyword {
        Keyword::Nothing => Ok((Expression::Nothing, span)),
        Keyword::Function => construct_function(&mut parser.accumulated),
        Keyword::Loop => construct_loop(parser),
        Keyword::Browse => construct_browse(parser),
        Keyword::BrowseOr => construct_browse_or(parser),
        Keyword::Times => construct_times(parser),
        Keyword::TimesOr => construct_times_or(parser),
        Keyword::Replace => construct_replace(parser),
        Keyword::Map => construct_map(parser),
        Keyword::Filter => construct_filter(parser),
        Keyword::Branch => construct_branch(parser),
        Keyword::Something => construct_something(parser),
        Keyword::Inspect => construct_inspect(parser),
        Keyword::Public => construct_public(parser),
        Keyword::Cast => construct_cast(parser),
        Keyword::Comptime => construct_comptime(parser),
    }?;
    Ok((expr, span.merge(&content_span)))
}

fn construct_function(
    accumulated: &mut VecDeque<PartialExpression>,
) -> Result<(Expression, Span), AnyError> {
    let mut elem = accumulated.pop_front();
    let (parameters, params_span) =
        if let Some(PartialExpression::ChildrenTypes(children, params_span)) = elem {
            elem = accumulated.pop_front();
            (children, params_span)
        } else {
            (Vec::new(), NO_SPAN)
        };

    let (returned, return_span, elem) = extract_single_child_type_or(
        accumulated,
        elem,
        TypedIdentifier::nameless(builtin_types::ANY),
    );

    match chain(elem) {
        Ok((body, chain_span)) => Ok((
            Expression::function(parameters, returned, body),
            params_span.merge(&chain_span),
        )),
        Err(elem) => {
            if let Some(elem) = elem {
                accumulated.push_front(elem);
            }
            Ok((
                Expression::Type(Type::function(parameters, returned)),
                params_span.merge(&return_span),
            ))
        }
    }
}

fn chain(
    partial_expr: Option<PartialExpression>,
) -> Result<(Chain, Span), Option<PartialExpression>> {
    if let Some(PartialExpression::Expression(ExpressionSpan {
        syntactic_type: Expression::Chain(body),
        span,
        ..
    })) = partial_expr
    {
        Ok((body, span))
    } else {
        Err(partial_expr)
    }
}

fn construct_loop(parser: &mut Parser) -> Result<(Expression, Span), AnyError> {
    match chain(parser.accumulated.pop_front()) {
        Ok((body, span)) => Ok((Expression::loop_(body), span)),
        Err(elem) => error_expected(format!("chain for the {} body", "loop"), elem),
    }
}

fn construct_browse(parser: &mut Parser) -> Result<(Expression, Span), AnyError> {
    construct_type_chain(parser, Expression::browse, "loop")
}

fn construct_browse_or(parser: &mut Parser) -> Result<(Expression, Span), AnyError> {
    construct_type_chain_chain(parser, Expression::browse_or, "loop_or")
}

fn construct_times(parser: &mut Parser) -> Result<(Expression, Span), AnyError> {
    construct_type_chain(parser, Expression::times, "times")
}

fn construct_times_or(parser: &mut Parser) -> Result<(Expression, Span), AnyError> {
    construct_type_chain_chain(parser, Expression::times_or, "times_or")
}

fn construct_replace(parser: &mut Parser) -> Result<(Expression, Span), AnyError> {
    construct_type_chain(parser, Expression::replace, "replace")
}

fn construct_map(parser: &mut Parser) -> Result<(Expression, Span), AnyError> {
    construct_type_chain(parser, Expression::map, "map")
}
fn construct_filter(parser: &mut Parser) -> Result<(Expression, Span), AnyError> {
    construct_type_chain(parser, Expression::filter, "filter")
}

fn construct_type_chain_chain(
    parser: &mut Parser,
    factory: fn(TypedIdentifier, Chain, Chain) -> Expression,
    construct_name: &str,
) -> Result<(Expression, Span), AnyError> {
    let elem = parser.accumulated.pop_front();
    let (parameter, type_span, elem) = extract_single_child_type(&mut parser.accumulated, elem);
    match chain(elem) {
        Ok((body, _span)) => {
            let elem = parser.accumulated.pop_front();
            match chain(elem) {
                Ok((otherwise, span)) => {
                    Ok((factory(parameter, body, otherwise), type_span.merge(&span)))
                }
                Err(elem) => err(expected_span_pe(
                    format!("chain for the '{}' 'otherwise' body", construct_name),
                    elem,
                    &parser.source,
                )),
            }
        }
        Err(elem) => error_expected(format!("chain for the '{}' body", construct_name), elem),
    }
}

fn construct_type_chain(
    parser: &mut Parser,
    factory: fn(TypedIdentifier, Chain) -> Expression,
    construct_name: &str,
) -> Result<(Expression, Span), AnyError> {
    let elem = parser.accumulated.pop_front();
    let (parameter, type_span, elem) = extract_single_child_type(&mut parser.accumulated, elem);

    match chain(elem) {
        Ok((body, span)) => Ok((factory(parameter, body), type_span.merge(&span))),
        Err(elem) => error_expected(format!("chain for the {} body", construct_name), elem),
    }
}

fn extract_single_child_type(
    accumulated: &mut VecDeque<PartialExpression>,
    elem: Option<PartialExpression>,
) -> (TypedIdentifier, Span, Option<PartialExpression>) {
    extract_single_child_type_or(accumulated, elem, TypedIdentifier::nothing())
}
fn extract_single_child_type_or(
    accumulated: &mut VecDeque<PartialExpression>,
    mut elem: Option<PartialExpression>,
    default: TypedIdentifier,
) -> (TypedIdentifier, Span, Option<PartialExpression>) {
    let mut span = NO_SPAN;
    let parameter =
        if let Some(PartialExpression::ChildrenTypes(mut children, children_span)) = elem {
            span = children_span;
            elem = accumulated.pop_front();
            children.truncate(1);
            if let Some(type_) = children.pop() {
                type_
            } else {
                default
            }
        } else {
            default
        };
    (parameter, span, elem)
}

fn construct_branch(parser: &mut Parser) -> Result<(Expression, Span), AnyError> {
    match chain(parser.accumulated.pop_front()) {
        Ok((yes, span_yes)) => match chain(parser.accumulated.pop_front()) {
            Ok((no, span_no)) => Ok((
                Expression::Composed(Composed::Branch(Branch { yes, no })),
                span_yes.merge(&span_no),
            )),
            Err(elem) => error_expected("chain for the branch negative case", elem),
        },
        Err(elem) => error_expected("chain for the branch positive case", elem),
    }
}

fn construct_something(parser: &mut Parser) -> Result<(Expression, Span), AnyError> {
    construct_type_chain_chain(parser, Expression::something, "something")
}
fn construct_inspect(parser: &mut Parser) -> Result<(Expression, Span), AnyError> {
    construct_type_chain(parser, Expression::inspect, "inspect")
}

fn construct_public(parser: &mut Parser) -> Result<(Expression, Span), AnyError> {
    let elem = parser.accumulated.pop_front();
    if let Some(PartialExpression::Expression(expr)) = elem {
        let elem = parser.accumulated.pop_front();
        if let Some(PartialExpression::Operation(Operation {
            operator:
                OperatorSpan {
                    operator: Operator::Assignment,
                    ..
                },
            operands,
            ..
        })) = elem
        {
            let operand = take_single(operands);
            if let Some(ExpressionSpan {
                syntactic_type: Expression::Identifier(name),
                span,
                ..
            }) = operand
            {
                // parser.identifiers.insert(name.clone(), expr);
                let qualified = if let (Some(root), Some(file)) =
                    (parser.root.as_ref(), parser.source.file.as_ref())
                {
                    qualify(&name, root, file)?
                } else {
                    name
                };
                parser.exported.insert(qualified.clone(), expr);
                Ok((Expression::Identifier(qualified), span))
            } else {
                error_expected("identifier after 'public <expression> ='", operand)
            }
        } else {
            error_expected("assignment after 'public <expression>'", elem)
        }
    } else {
        error_expected("expression after 'public'", elem)
    }
}

fn construct_cast(parser: &mut Parser) -> Result<(Expression, Span), AnyError> {
    let elem = parser.accumulated.pop_front();
    if let Some(PartialExpression::ChildrenTypes(mut children, children_span)) = elem {
        children.truncate(1);
        let target_type = if let Some(type_) = children.pop() {
            type_
        } else {
            TypedIdentifier::nothing()
        };
        Ok((
            Expression::Composed(Composed::Cast(Cast { target_type })),
            children_span,
        ))
    } else {
        error_expected("type inside parenthesis", elem)
    }
}
fn construct_comptime(parser: &mut Parser) -> Result<(Expression, Span), AnyError> {
    match chain(parser.accumulated.pop_front()) {
        Ok((body, span_body)) => Ok((
            Expression::Composed(Composed::Comptime(Comptime { body })),
            span_body,
        )),
        Err(elem) => error_expected("chain for the 'comptime' body", elem),
    }
}

fn construct_chain(
    accumulated: &mut VecDeque<PartialExpression>,
    source: &SourceCode,
    open_brace_span: Span,
) -> Result<ExpressionSpan, AnyError> {
    let elem_expression = accumulated.pop_front();
    match elem_expression {
        Some(PartialExpression::CloseBrace(span)) => Ok(ExpressionSpan::new_typeless(
            Expression::empty_chain(),
            open_brace_span.merge(&span),
        )),
        Some(PartialExpression::Expression(initial)) => {
            let span = Span {
                start: initial.span.start,
                end: initial.span.start,
            };
            construct_chain_transformations(
                accumulated,
                Operation::single_no_sem_type(OperatorSpan::new(Operator::Ignore, span), initial),
                source,
                open_brace_span,
            )
        }
        Some(PartialExpression::Operation(operation)) => {
            construct_chain_transformations(accumulated, operation, source, open_brace_span)
        }
        _ => err_span(
            expected("expression or closing brace", elem_expression),
            source,
            open_brace_span,
        ),
    }
}

fn construct_chain_transformations(
    accumulated: &mut VecDeque<PartialExpression>,
    initial: Operation,
    source: &SourceCode,
    open_brace_span: Span,
) -> Result<ExpressionSpan, AnyError> {
    let mut transformations = Operations::new();
    transformations.push(initial);
    let mut last_span = open_brace_span;
    loop {
        let elem_operator = accumulated.pop_front();
        match elem_operator {
            Some(PartialExpression::CloseBrace(span)) => {
                return Ok(ExpressionSpan::new_typeless(
                    Expression::chain(transformations),
                    open_brace_span.merge(&span),
                ))
            }
            Some(PartialExpression::Operation(transformation)) => {
                last_span = transformation
                    .operands
                    .last()
                    .map(|o| o.span)
                    .unwrap_or(transformation.operator.span);
                transformations.push(transformation);
            }
            _ => err_span(
                expected(
                    "operator or closing brace after highlighted code",
                    elem_operator,
                ),
                source,
                open_brace_span.merge(&last_span),
            )?,
        }
    }
}

fn construct_array(
    accumulated: &mut VecDeque<PartialExpression>,
    source: &SourceCode,
    open_bracket_span: Span,
) -> Result<ExpressionSpan, AnyError> {
    let mut elements = Vec::new();
    let mut elem = accumulated.pop_front();
    let mut last_span = open_bracket_span;
    while let Some(PartialExpression::Expression(e)) = elem {
        last_span = e.span;
        elements.push(e);
        elem = accumulated.pop_front()
    }
    if let Some(PartialExpression::CloseBracket(span)) = elem {
        Ok(ExpressionSpan::new_typeless(
            Expression::StaticList { elements },
            open_bracket_span.merge(&span),
        ))
    } else if let Some(PartialExpression::Operation(Operation { operator, .. })) = elem {
        let expected_message = expected("array end or expression", Some(operator.operator));
        let message = format!(
            "List elements need braces ('{{' and '}}') if they are chained operations.\n{}",
            expected_message
        );
        err_span(message, source, operator.span)
    } else {
        err_span(
            expected("array end or expression", elem),
            source,
            open_bracket_span.merge(&last_span),
        )
    }
}
fn construct_children_types(
    accumulated: &mut VecDeque<PartialExpression>,
    source: &SourceCode,
    open_paren_span: Span,
) -> Result<PartialExpression, AnyError> {
    let mut types = TypedIdentifiers::new();
    let mut elem = accumulated.pop_front();
    let mut name_opt = None;
    loop {
        match elem {
            Some(PartialExpression::Expression(ExpressionSpan {
                syntactic_type: Expression::Identifier(name),
                ..
            })) => {
                if let Some(previous_name) = name_opt {
                    types.push(TypedIdentifier::any(previous_name));
                }
                name_opt = Some(name)
            }
            Some(PartialExpression::Operation(Operation {
                operator:
                    OperatorSpan {
                        operator: Operator::Type,
                        ..
                    },
                operands,
                ..
            })) => {
                let operand = take_single(operands);
                if let Some(ExpressionSpan {
                    syntactic_type: Expression::Type(type_),
                    ..
                }) = operand
                {
                    let typed_identifier = if let Some(previous_name) = name_opt {
                        name_opt = None;
                        TypedIdentifier {
                            name: previous_name,
                            type_: type_,
                        }
                    } else {
                        TypedIdentifier::nameless(type_)
                    };
                    types.push(typed_identifier);
                } else {
                    return error_expected("type after ':'", operand);
                }
            }
            Some(PartialExpression::CloseParenthesis(span)) => {
                if let Some(previous_name) = name_opt {
                    types.push(TypedIdentifier::any(previous_name));
                }
                return Ok(PartialExpression::ChildrenTypes(
                    types,
                    open_paren_span.merge(&span),
                ));
            }
            Some(_) | None => {
                return err_span(
                    expected("closing parenthesis or expression", elem),
                    source,
                    open_paren_span,
                )
            }
        }
        elem = accumulated.pop_front();
    }
}

pub fn construct_string(string: Vec<u8>, span: Span) -> PartialExpression {
    let elements = string
        .iter()
        .map(|b| ExpressionSpan::new_typeless(Expression::Value(*b as i64), span))
        .collect::<Vec<_>>();
    PartialExpression::expression(Expression::StaticList { elements }, span)
}

fn finish_construction(mut parser: Parser) -> Result<IncompleteProgram, AnyError> {
    let accumulated = &mut parser.accumulated;
    let mut main = if accumulated.len() <= 1 {
        match accumulated.pop_front() {
            Some(PartialExpression::Expression(e)) => e,
            Some(PartialExpression::Operation(o)) => {
                let span = o.content_span();
                let expr = ExpressionSpan::new_typeless(Expression::chain(vec![o]), span);
                expr
            }
            None => ExpressionSpan::new_spanless(Expression::empty_chain()),
            Some(v) => {
                accumulated.push_front(v);
                return err(format!("unfinished code: {:?}", accumulated));
            }
        }
    } else {
        let error_message = format!("unfinished code: {:?}", accumulated);
        let start_span = accumulated.iter().next().unwrap().span();
        let end_span = accumulated.iter().last().unwrap().span();
        let span = start_span.merge(&end_span);
        accumulated.push_back(PartialExpression::CloseBrace(NO_SPAN));
        let expr = construct_chain(accumulated, &parser.source, span)?;
        if !accumulated.is_empty() {
            return err(error_message);
        } else {
            expr
        }
    };

    let (imported, other_sources) = import(&mut main, &mut parser)?;
    parser.exported.extend(imported);

    Ok(IncompleteProgram {
        main,
        exported: parser.exported,
        available: parser.available,
        sources: Sources::new(parser.source, other_sources),
    })
}

pub fn error_expected<T: Display, R, S: AsRef<str>>(
    expected: S,
    actual: Option<T>,
) -> Result<R, AnyError> {
    Err(anyerror_expected(expected, actual))
}
pub fn anyerror_expected<T: Display, S: AsRef<str>>(expected_: S, actual: Option<T>) -> AnyError {
    expected(expected_.as_ref(), actual).into()
}
pub fn expected<T: Display, S: AsRef<str>>(expected: S, actual: Option<T>) -> String {
    if let Some(actual) = actual.as_ref() {
        format!("expected {} but was '{}'", expected.as_ref(), actual)
    } else {
        format!("expected {} but was None", expected.as_ref())
    }
}
pub fn expected_span<T: Display, S: AsRef<str>>(
    expected_str: S,
    actual: Option<T>,
    source: &SourceCode,
    span: Span,
) -> String {
    expected(expected_str, actual) + &source.format_span(span)
}
pub fn expected_span_pe<S: AsRef<str>>(
    expected_str: S,
    actual: Option<PartialExpression>,
    source: &SourceCode,
) -> String {
    if let Some(pe) = &actual {
        let span = match &pe {
            PartialExpression::CloseBracket(span)
            | PartialExpression::CloseBrace(span)
            | PartialExpression::CloseParenthesis(span)
            | PartialExpression::Expression(ExpressionSpan { span, .. })
            | PartialExpression::Operation(Operation {
                operator: OperatorSpan { span, .. },
                ..
            })
            | PartialExpression::ChildrenTypes(_, span) => *span,
        };
        expected(expected_str, actual) + &source.format_span(span)
    } else {
        expected(expected_str, actual)
    }
}
*/
