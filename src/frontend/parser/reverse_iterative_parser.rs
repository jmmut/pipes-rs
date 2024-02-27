use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::{Display, Formatter};
use std::path::PathBuf;

use crate::common::{context, err, err_span, AnyError};
use crate::frontend::ast::{error_expected, expected};
use crate::frontend::expression::display::typed_identifiers_to_str;
use crate::frontend::expression::{
    take_single, Branch, Cast, Chain, Composed, Expression, ExpressionSpan, Operation, Operations,
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
    ast.raw_parse_tokens(tokens)?;
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
    Operation(Operation),
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
impl Display for PartialExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            PartialExpression::OpenBracket(_) => write!(f, "["),
            PartialExpression::CloseBracket(_) => write!(f, "]"),
            PartialExpression::OpenBrace(_) => write!(f, "{{"),
            PartialExpression::CloseBrace(_) => write!(f, "}}"),
            PartialExpression::OpenParenthesis(_) => write!(f, "("),
            PartialExpression::CloseParenthesis(_) => write!(f, ")"),
            PartialExpression::Expression(expr) => write!(f, "{}", expr),
            PartialExpression::Operation(op) => write!(f, "{}", op),
            PartialExpression::Operator(op) => write!(f, "{}", op),
            PartialExpression::Keyword(keyword) => write!(f, "{}", keyword.name()),
            PartialExpression::ChildrenTypes(types) => {
                write!(f, "{}", typed_identifiers_to_str(types, true))
            }
            PartialExpression::TypedIdentifier(name) => write!(f, "{}", name),
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
                Token::Identifier(ident) => self.push(Expression::Identifier(ident), span),
                Token::Keyword(keyword) => {
                    let pe = construct_keyword(self, keyword, span)?;
                    self.accumulated.push_front(pe);
                }
                Token::OpenBrace => self.push_f(construct_chain, span)?,
                Token::CloseBrace => self.push_pe(PartialExpression::CloseBrace(span)),
                Token::OpenBracket => self.push_f(construct_array, span)?,
                Token::CloseBracket => self.push_pe(PartialExpression::CloseBracket(span)),
                Token::OpenParenthesis => self.push_f_pe(construct_children_types)?,
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
        })) = expression
        {
            let operand = take_single(operands);
            match operand {
                Some(ExpressionSpan {
                    syntactic_type: Expression::Type(type_),
                    span,
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
        })) = elem_operand
        {
            let operand = get_type_maybe_pop_children(accumulated, typename);
            Operation::single(operator, operand)
        } else if let Some(PartialExpression::Expression(ExpressionSpan {
            syntactic_type: Expression::Type(type_),
            span,
        })) = elem_operand
        {
            let operand = ExpressionSpan::new_spanless(Expression::Type(type_));
            Operation::single(operator, operand)
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
            Operation {
                operator: OperatorSpan {
                    operator: raw_operator,
                    span: span.merge(&last_expr_span),
                },
                operands,
            }
        } else {
            let actual = accumulated.front();
            err(expected("some callable expression", actual))?
        }
    } else {
        if let Some(PartialExpression::Expression(operand)) = elem_operand {
            Operation::single(operator, operand)
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
            Operation::single(operator, operand)
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
    let mut elem = accumulated.pop_front();
    let parameters = if let Some(PartialExpression::ChildrenTypes(children)) = elem {
        elem = accumulated.pop_front();
        children
    } else {
        Vec::new()
    };

    if let Some(PartialExpression::Expression(ExpressionSpan {
        syntactic_type: Expression::Chain(body),
        span: chain_span,
    })) = elem
    {
        Ok(PartialExpression::expression(
            Expression::function(parameters, body),
            span.merge(&chain_span),
        ))
    } else {
        let (returned, elem) = extract_single_child_type(accumulated, elem);

        if let Some(elem) = elem {
            accumulated.push_front(elem);
        }

        Ok(PartialExpression::expression_no_span(Expression::Type(
            Type::function(parameters, returned),
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
        if let Some(PartialExpression::Operation(Operation {
            operator:
                OperatorSpan {
                    operator: Operator::Assignment,
                    ..
                },
            operands,
        })) = elem
        {
            let operand = take_single(operands);
            if let Some(ExpressionSpan {
                syntactic_type: Expression::Identifier(name),
                span,
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
                Ok(PartialExpression::expression_no_span(
                    Expression::Identifier(qualified),
                ))
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
    source: &SourceCode,
    span: Span,
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
    let mut transformations = Operations::new();
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
        Ok(ExpressionSpan::new(
            Expression::StaticList { elements },
            span,
        ))
    } else if let Some(PartialExpression::Operation(Operation { operator, operands })) = elem {
        println!("span: {:?}", last_span);
        let expected_message = expected("array end or expression", Some(operator.operator));
        let message = format!(
            "List elements need braces ('{{' and '}}') if they are chained operations.\n{}",
            expected_message
        );
        err_span(message, source, operator.span)
    } else {
        println!("span: {:?}", last_span);
        err_span(
            expected("array end or expression", elem),
            source,
            open_bracket_span.merge(&last_span),
        )
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
            Some(PartialExpression::Operation(Operation {
                operator:
                    OperatorSpan {
                        operator: Operator::Type,
                        ..
                    },
                operands,
            })) => {
                let operand = take_single(operands);
                if let Some(ExpressionSpan {
                    syntactic_type: Expression::Type(type_),
                    span,
                }) = operand
                {
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
                } else {
                    return error_expected("type after ':'", operand);
                }
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
        let expr = construct_chain(accumulated, &parser.source, NO_SPAN)?;
        if !accumulated.is_empty() {
            return err(error_message);
        } else {
            expr
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
