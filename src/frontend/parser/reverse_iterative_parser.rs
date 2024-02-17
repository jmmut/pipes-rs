use std::collections::{HashMap, HashSet, VecDeque};
use std::path::PathBuf;

use crate::common::{context, err, AnyError};
use crate::frontend::ast::error_expected;
use crate::frontend::expression::{
    Branch, Cast, Chain, Composed, Expression, ExpressionSpan, Transformation, Transformations,
    Type, TypedIdentifier, TypedIdentifiers,
};
use crate::frontend::lexer::TokenizedSource;
use crate::frontend::location::{SourceCode, Span, NO_SPAN};
use crate::frontend::parser::import::import;
use crate::frontend::parser::root::{get_project_root, qualify};
use crate::frontend::program::{IncompleteProgram, Program};
use crate::frontend::token::{Keyword, LocatedToken, LocatedTokens, Operator, OperatorSpan, Token};

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
    ast = raw_parse_tokens(tokens, ast)?;
    finish_construction(ast)
}

#[derive(Debug)]
pub enum PartialExpression {
    OpenBracket(Span),
    CloseBracket(Span),
    OpenBrace(Span),
    CloseBrace(Span),
    OpenParenthesis(Span),
    CloseParenthesis(Span),
    Expression(ExpressionSpan),
    Operation(Transformation),
    Operator(Operator),
    Keyword(Keyword),
    ChildrenTypes(TypedIdentifiers),
    TypedIdentifier(TypedIdentifier),
}

impl PartialExpression {
    pub fn expression_no_span(e: Expression) -> PartialExpression {
        PartialExpression::Expression(ExpressionSpan::new(e, NO_SPAN))
    }
    pub fn expression(e: Expression, span: Span) -> PartialExpression {
        PartialExpression::Expression(ExpressionSpan::new(e, span))
    }
}

pub fn raw_parse_tokens(tokens: LocatedTokens, mut ast: Parser) -> Result<Parser, AnyError> {
    for LocatedToken { token, span } in tokens.into_iter().rev() {
        match token {
            Token::Number(n) => ast.push(Expression::Value(n), span),
            Token::Operator(operator) => {
                let pe = construct_transformation(&mut ast, operator, span)?;
                ast.accumulated.push_front(pe);
            }
            Token::Identifier(ident) => ast.push(Expression::Identifier(ident), span),
            Token::Keyword(keyword) => {
                let pe = construct_keyword(&mut ast, keyword, span)?;
                ast.accumulated.push_front(pe);
            }
            Token::OpenBrace => ast.push_f(construct_chain, span)?,
            Token::CloseBrace => ast.push_pe(PartialExpression::CloseBrace(span)),
            Token::OpenBracket => ast.push_f(construct_array, span)?,
            Token::CloseBracket => ast.push_pe(PartialExpression::CloseBracket(span)),
            Token::OpenParenthesis => ast.push_f_pe(construct_children_types)?,
            Token::CloseParenthesis => ast.push_pe(PartialExpression::CloseParenthesis(span)),
            Token::String(string) => ast.push_pe(construct_string(string, span)), // _ => return error_expected("anything else", token),
        };
    }
    Ok(ast)
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
    fn parse_tokens(tokens: TokenizedSource) -> Result<Program, AnyError> {
        let parser = Parser::new(tokens.source_code);
        parse_tokens_cached(tokens.tokens, parser)
    }

    fn parse_type(mut tokens: TokenizedSource) -> Result<Type, AnyError> {
        let parser = Parser::new(tokens.source_code);
        tokens
            .tokens
            .insert(0, LocatedToken::spanless(Token::Operator(Operator::Type)));
        let mut parser = raw_parse_tokens(tokens.tokens, parser)?;
        let expression = parser.accumulated.pop_front();
        if !parser.accumulated.is_empty() {
            err(format!(
                "Could not parse as a type because there are extra unused expressions: {:?}",
                parser.accumulated
            ))
        } else if let Some(PartialExpression::Operation(Transformation {
            operator:
                OperatorSpan {
                    operator: Operator::Type,
                    ..
                },
            operand:
                ExpressionSpan {
                    syntactic_type: Expression::Type(type_),
                    ..
                },
        })) = expression
        {
            Ok(type_)
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
        ) -> Result<ExpressionSpan, AnyError>,
        span: Span,
    ) -> Result<(), AnyError> {
        let expression = construct_expression(&mut self.accumulated)?;
        self.push_es(expression);
        Ok(())
    }
    fn push_f_pe(
        &mut self,
        construct_expression: fn(
            &mut VecDeque<PartialExpression>,
        ) -> Result<PartialExpression, AnyError>,
    ) -> Result<(), AnyError> {
        let expression = construct_expression(&mut self.accumulated)?;
        self.push_pe(expression);
        Ok(())
    }

    fn push(&mut self, expression: Expression, span: Span) {
        // TODO: use span
        self.push_pe(PartialExpression::Expression(ExpressionSpan::new(
            expression, span,
        )));
    }
    fn push_es(&mut self, expression_span: ExpressionSpan) {
        // TODO: make ExpressionSpan
        self.push_pe(PartialExpression::Expression(expression_span));
    }
    fn push_pe(&mut self, partial_expression: PartialExpression) {
        self.accumulated.push_front(partial_expression);
    }
}

fn ident(maybe_pe: Option<PartialExpression>) -> (Option<String>, Option<PartialExpression>) {
    if let Some(PartialExpression::Expression(ExpressionSpan {
        syntactic_type: Expression::Identifier(typename),
        span,
    })) = maybe_pe
    {
        (Some(typename), None)
    } else {
        (None, maybe_pe)
    }
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
    let elem_operand = accumulated.pop_front();
    let transformation = if let Operator::Type = raw_operator {
        let (maybe_typename, elem_operand) = ident(elem_operand);
        if let Some(typename) = maybe_typename {
            let operand = get_type_maybe_pop_children(accumulated, typename);
            Transformation { operator, operand }
        } else if let Some(PartialExpression::Expression(ExpressionSpan {
            syntactic_type: Expression::Type(type_),
            span,
        })) = elem_operand
        {
            let operand = ExpressionSpan::new_spanless(Expression::Type(type_));
            Transformation { operator, operand }
        } else {
            error_expected("type or type name after type operator ':'", elem_operand)?
        }
    } else {
        if let Some(PartialExpression::Expression(operand)) = elem_operand {
            Transformation { operator, operand }
        } else if raw_operator == Operator::Ignore {
            let operand = if let None = elem_operand {
                Expression::empty_chain()
            } else if let Some(PartialExpression::CloseBrace(span)) = elem_operand {
                accumulated.push_front(PartialExpression::CloseBrace(span));
                Expression::empty_chain()
            } else {
                error_expected("expression or close brace or end of file", elem_operand)?
            };
            let operand = ExpressionSpan::new_spanless(operand);
            Transformation { operator, operand }
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
        Some(PartialExpression::ChildrenTypes(children)) => {
            return ExpressionSpan::new_spanless(Expression::Type(Type::from(typename, children)));
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
) -> Result<PartialExpression, AnyError> {
    let accumulated = &mut parser.accumulated;
    match keyword {
        Keyword::Function => construct_function(accumulated, span),
        Keyword::Loop => construct_loop(accumulated),
        Keyword::LoopOr => construct_loop_or(accumulated),
        Keyword::Times => construct_times(accumulated),
        Keyword::TimesOr => construct_times_or(accumulated),
        Keyword::Replace => construct_replace(accumulated),
        Keyword::Map => construct_map(accumulated),
        Keyword::Branch => construct_branch(accumulated),
        Keyword::Something => construct_something(accumulated),
        Keyword::Inspect => construct_inspect(accumulated),
        Keyword::Public => construct_public(parser),
        Keyword::Cast => construct_cast(parser),
    }
}

fn construct_function(
    accumulated: &mut VecDeque<PartialExpression>,
    span: Span,
) -> Result<PartialExpression, AnyError> {
    let elem = accumulated.pop_front();

    let (parameter, elem) = extract_single_child_type(accumulated, elem);

    if let Some(PartialExpression::Expression(ExpressionSpan {
        syntactic_type: Expression::Chain(body),
        span: chain_span,
    })) = elem
    {
        Ok(PartialExpression::expression(
            Expression::function(parameter, body),
            span.merge(&chain_span),
        ))
    } else {
        let (returned, elem) = extract_single_child_type(accumulated, elem);

        if let Some(elem) = elem {
            accumulated.push_front(elem);
        }

        Ok(PartialExpression::expression_no_span(Expression::Type(
            Type::function(parameter, returned),
        )))
    }
}

fn construct_loop(
    accumulated: &mut VecDeque<PartialExpression>,
) -> Result<PartialExpression, AnyError> {
    construct_type_chain(accumulated, Expression::loop_, "loop")
}

fn construct_loop_or(
    accumulated: &mut VecDeque<PartialExpression>,
) -> Result<PartialExpression, AnyError> {
    construct_type_chain_chain(accumulated, Expression::loop_or, "loop_or")
}

fn construct_times(
    accumulated: &mut VecDeque<PartialExpression>,
) -> Result<PartialExpression, AnyError> {
    construct_type_chain(accumulated, Expression::times, "times")
}

fn construct_times_or(
    accumulated: &mut VecDeque<PartialExpression>,
) -> Result<PartialExpression, AnyError> {
    construct_type_chain_chain(accumulated, Expression::times_or, "times_or")
}

fn construct_replace(
    accumulated: &mut VecDeque<PartialExpression>,
) -> Result<PartialExpression, AnyError> {
    construct_type_chain(accumulated, Expression::replace, "replace")
}

fn construct_map(
    accumulated: &mut VecDeque<PartialExpression>,
) -> Result<PartialExpression, AnyError> {
    construct_type_chain(accumulated, Expression::map, "map")
}

fn construct_type_chain_chain(
    accumulated: &mut VecDeque<PartialExpression>,
    factory: fn(TypedIdentifier, Chain, Chain) -> Expression,
    construct_name: &str,
) -> Result<PartialExpression, AnyError> {
    let elem = accumulated.pop_front();
    let (parameter, elem) = extract_single_child_type(accumulated, elem);

    if let Some(PartialExpression::Expression(ExpressionSpan {
        syntactic_type: Expression::Chain(body),
        span,
    })) = elem
    {
        let elem = accumulated.pop_front();
        if let Some(PartialExpression::Expression(ExpressionSpan {
            syntactic_type: Expression::Chain(otherwise),
            span,
        })) = elem
        {
            Ok(PartialExpression::expression_no_span(factory(
                parameter, body, otherwise,
            )))
        } else {
            error_expected(
                format!("chain for the '{}' 'otherwise' body", construct_name),
                elem,
            )
        }
    } else {
        error_expected(format!("chain for the '{}' body", construct_name), elem)
    }
}

fn construct_type_chain(
    accumulated: &mut VecDeque<PartialExpression>,
    factory: fn(TypedIdentifier, Chain) -> Expression,
    construct_name: &str,
) -> Result<PartialExpression, AnyError> {
    let elem = accumulated.pop_front();
    let (parameter, elem) = extract_single_child_type(accumulated, elem);

    if let Some(PartialExpression::Expression(ExpressionSpan {
        syntactic_type: Expression::Chain(body),
        span,
    })) = elem
    {
        Ok(PartialExpression::expression_no_span(factory(
            parameter, body,
        )))
    } else {
        error_expected(format!("chain for the {} body", construct_name), elem)
    }
}

fn extract_single_child_type(
    accumulated: &mut VecDeque<PartialExpression>,
    mut elem: Option<PartialExpression>,
) -> (TypedIdentifier, Option<PartialExpression>) {
    let parameter = if let Some(PartialExpression::ChildrenTypes(mut children)) = elem {
        elem = accumulated.pop_front();
        children.truncate(1);
        if let Some(type_) = children.pop() {
            type_
        } else {
            TypedIdentifier::nothing()
        }
    } else {
        TypedIdentifier::nothing()
    };
    (parameter, elem)
}

fn construct_branch(
    accumulated: &mut VecDeque<PartialExpression>,
) -> Result<PartialExpression, AnyError> {
    let elem = accumulated.pop_front();
    if let Some(PartialExpression::Expression(ExpressionSpan {
        syntactic_type: Expression::Chain(yes),
        span,
    })) = elem
    {
        let elem = accumulated.pop_front();
        if let Some(PartialExpression::Expression(ExpressionSpan {
            syntactic_type: Expression::Chain(no),
            span,
        })) = elem
        {
            Ok(PartialExpression::expression_no_span(Expression::Composed(
                Composed::Branch(Branch { yes, no }),
            )))
        } else {
            error_expected("chain for the branch negative case", elem)
        }
    } else {
        error_expected("chain for the branch positive case", elem)
    }
}

fn construct_something(
    accumulated: &mut VecDeque<PartialExpression>,
) -> Result<PartialExpression, AnyError> {
    construct_type_chain_chain(accumulated, Expression::something, "something")
}
fn construct_inspect(
    accumulated: &mut VecDeque<PartialExpression>,
) -> Result<PartialExpression, AnyError> {
    construct_type_chain(accumulated, Expression::inspect, "inspect")
}

fn construct_public(parser: &mut Parser) -> Result<PartialExpression, AnyError> {
    let elem = parser.accumulated.pop_front();
    if let Some(PartialExpression::Expression(expr)) = elem {
        let elem = parser.accumulated.pop_front();
        if let Some(PartialExpression::Operation(Transformation {
            operator:
                OperatorSpan {
                    operator: Operator::Assignment,
                    ..
                },
            operand:
                ExpressionSpan {
                    syntactic_type: Expression::Identifier(name),
                    span,
                },
        })) = elem
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
            Ok(PartialExpression::expression_no_span(
                Expression::Identifier(qualified),
            ))
        } else {
            error_expected(
                "assignment and identifier after 'public <expression>'",
                elem,
            )
        }
    } else {
        error_expected("expression after 'public'", elem)
    }
}

fn construct_cast(parser: &mut Parser) -> Result<PartialExpression, AnyError> {
    let elem = parser.accumulated.pop_front();
    if let Some(PartialExpression::ChildrenTypes(mut children)) = elem {
        children.truncate(1);
        let target_type = if let Some(type_) = children.pop() {
            type_
        } else {
            TypedIdentifier::nothing()
        };
        Ok(PartialExpression::expression_no_span(Expression::Composed(
            Composed::Cast(Cast { target_type }),
        )))
    } else {
        error_expected("type inside parenthesis", elem)
    }
}

fn construct_chain(
    accumulated: &mut VecDeque<PartialExpression>,
) -> Result<ExpressionSpan, AnyError> {
    let elem_expression = accumulated.pop_front();
    match elem_expression {
        Some(PartialExpression::CloseBrace(span)) => {
            Ok(ExpressionSpan::new_spanless(Expression::empty_chain()))
        }
        Some(PartialExpression::Expression(initial)) => {
            construct_chain_transformations(accumulated, initial)
        }
        _ => error_expected("expression or closing brace", elem_expression),
    }
}

fn construct_chain_transformations(
    accumulated: &mut VecDeque<PartialExpression>,
    initial: ExpressionSpan,
) -> Result<ExpressionSpan, AnyError> {
    let mut transformations = Transformations::new();
    loop {
        let elem_operator = accumulated.pop_front();
        match elem_operator {
            Some(PartialExpression::CloseBrace(span)) => {
                return Ok(ExpressionSpan::new_spanless(Expression::chain(
                    Box::new(initial),
                    transformations,
                )))
            }
            Some(PartialExpression::Operation(transformation)) => {
                transformations.push(transformation);
            }
            _ => error_expected("operator or closing brace", elem_operator)?,
        }
    }
}

fn construct_array(
    accumulated: &mut VecDeque<PartialExpression>,
) -> Result<ExpressionSpan, AnyError> {
    let mut elements = Vec::new();
    let mut elem = accumulated.pop_front();
    while let Some(PartialExpression::Expression(e)) = elem {
        elements.push(e);
        elem = accumulated.pop_front()
    }
    if let Some(PartialExpression::CloseBracket(span)) = elem {
        Ok(ExpressionSpan::new_spanless(Expression::StaticList {
            elements,
        }))
    } else {
        error_expected("array end or expression", elem)
    }
}
fn construct_children_types(
    accumulated: &mut VecDeque<PartialExpression>,
) -> Result<PartialExpression, AnyError> {
    let mut types = TypedIdentifiers::new();
    let mut elem = accumulated.pop_front();
    let mut name_opt = None;
    loop {
        match elem {
            Some(PartialExpression::Expression(ExpressionSpan {
                syntactic_type: Expression::Identifier(name),
                span,
            })) => {
                if let Some(previous_name) = name_opt {
                    types.push(TypedIdentifier::any(previous_name));
                }
                name_opt = Some(name)
            }
            Some(PartialExpression::Operation(Transformation {
                operator:
                    OperatorSpan {
                        operator: Operator::Type,
                        ..
                    },
                operand:
                    ExpressionSpan {
                        syntactic_type: Expression::Type(type_),
                        span,
                    },
            })) => {
                let typed_identifier = if let Some(previous_name) = name_opt {
                    name_opt = None;
                    TypedIdentifier {
                        name: previous_name,
                        type_,
                    }
                } else {
                    TypedIdentifier::nameless(type_)
                };
                types.push(typed_identifier);
            }
            Some(PartialExpression::CloseParenthesis(span)) => {
                if let Some(previous_name) = name_opt {
                    types.push(TypedIdentifier::any(previous_name));
                }
                return Ok(PartialExpression::ChildrenTypes(types));
            }
            Some(_) | None => return error_expected("closing parenthesis or expression", elem),
        }
        elem = accumulated.pop_front();
    }
}

pub fn construct_string(string: Vec<u8>, span: Span) -> PartialExpression {
    let elements = string
        .iter()
        .map(|b| ExpressionSpan::new(Expression::Value(*b as i64), span))
        .collect::<Vec<_>>();
    PartialExpression::expression_no_span(Expression::StaticList { elements })
}

fn finish_construction(mut parser: Parser) -> Result<IncompleteProgram, AnyError> {
    let accumulated = &mut parser.accumulated;
    let mut main = if accumulated.len() <= 1 {
        match accumulated.pop_front() {
            Some(PartialExpression::Expression(e)) => e,
            None => ExpressionSpan::new_spanless(Expression::Nothing),
            Some(v) => {
                accumulated.push_front(v);
                return err(format!("unfinished code: {:?}", accumulated));
            }
        }
    } else {
        let error_message = format!("unfinished code: {:?}", accumulated);
        accumulated.push_back(PartialExpression::CloseBrace(NO_SPAN));
        let e = construct_chain(accumulated)?;
        if !accumulated.is_empty() {
            return err(error_message);
        } else {
            e
        }
    };

    let (imported, other_sources) = import(&mut main, &mut parser)?;
    parser.exported.extend(imported);

    Ok(IncompleteProgram {
        main: main,
        exported: parser.exported,
        available: parser.available,
        sources: other_sources,
        main_source: parser.source,
    })
}
