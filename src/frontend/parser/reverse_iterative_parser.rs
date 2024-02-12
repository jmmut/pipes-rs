use std::collections::{HashMap, HashSet, VecDeque};
use std::path::PathBuf;

use crate::common::{context, err, AnyError};
use crate::frontend::ast::{error_expected, PartialExpression};
use crate::frontend::expression::{
    Branch, Cast, Chain, Composed, Expression, Transformation, Transformations, Type,
    TypedIdentifier, TypedIdentifiers,
};
use crate::frontend::lexer::TokenizedSource;
use crate::frontend::parser::import::import;
use crate::frontend::parser::root::{get_project_root, qualify};
use crate::frontend::program::{IncompleteProgram, Program};
use crate::frontend::token::{Keyword, Operator, Token, Tokens};

pub fn parse_tokens(tokens: TokenizedSource) -> Result<Program, AnyError> {
    context("Reverse parser", Parser::parse_tokens(tokens))
}
pub fn parse_type(tokens: TokenizedSource) -> Result<Type, AnyError> {
    context("Reverse type parser", Parser::parse_type(tokens))
}

pub fn parse_tokens_cached(tokens: Tokens, ast: Parser) -> Result<Program, AnyError> {
    Ok(parse_tokens_cached_inner(tokens, ast)?.into())
}

pub fn parse_tokens_cached_inner(
    tokens: Tokens,
    mut ast: Parser,
) -> Result<IncompleteProgram, AnyError> {
    ast = raw_parse_tokens(tokens, ast)?;
    finish_construction(ast)
}

pub fn raw_parse_tokens(tokens: Tokens, mut ast: Parser) -> Result<Parser, AnyError> {
    for token in tokens.into_iter().rev() {
        match token {
            Token::Number(n) => ast.push(Expression::Value(n)),
            Token::Operator(operator) => {
                let pe = construct_transformation(&mut ast, operator)?;
                ast.accumulated.push_front(pe);
            }
            Token::Identifier(ident) => ast.push(Expression::Identifier(ident)),
            Token::Keyword(keyword) => {
                let pe = construct_keyword(&mut ast, keyword)?;
                ast.accumulated.push_front(pe);
            }
            Token::OpenBrace => ast.push_f(construct_chain)?,
            Token::CloseBrace => ast.push_pe(PartialExpression::CloseBrace),
            Token::OpenBracket => ast.push_f(construct_array)?,
            Token::CloseBracket => ast.push_pe(PartialExpression::CloseBracket),
            Token::OpenParenthesis => ast.push_f_pe(construct_children_types)?,
            Token::CloseParenthesis => ast.push_pe(PartialExpression::CloseParenthesis),
            Token::String(string) => ast.push_pe(construct_string(string)), // _ => return error_expected("anything else", token),
        };
    }
    Ok(ast)
}

pub struct Parser {
    pub accumulated: VecDeque<PartialExpression>,
    pub exported: HashMap<String, IdentifierValue>,
    pub available: HashSet<String>,
    pub file: Option<PathBuf>,
    pub root: Option<PathBuf>,
}

/// present if it's a public identifier, None if private
pub type IdentifierValue = Expression;

impl Parser {
    pub fn new(file_opt: Option<PathBuf>) -> Self {
        let root = get_project_root(&None, &file_opt);
        Self::new_with_available(file_opt, HashSet::new(), root.ok()) // TODO: .ok() loses error message
    }
    pub fn new_with_available(
        file: Option<PathBuf>,
        available: HashSet<String>,
        root: Option<PathBuf>,
    ) -> Self {
        Self {
            accumulated: VecDeque::new(),
            exported: HashMap::new(),
            available,
            file,
            root,
        }
    }
    fn parse_tokens(tokens: TokenizedSource) -> Result<Program, AnyError> {
        let parser = Parser::new(tokens.source_code.file);
        parse_tokens_cached(tokens.tokens, parser)
    }

    fn parse_type(mut tokens: TokenizedSource) -> Result<Type, AnyError> {
        let parser = Parser::new(tokens.source_code.file);
        tokens.tokens.insert(0, Token::Operator(Operator::Type));
        let mut parser = raw_parse_tokens(tokens.tokens, parser)?;
        let expression = parser.accumulated.pop_front();
        if !parser.accumulated.is_empty() {
            err(format!(
                "Could not parse as a type because there are extra unused expressions: {:?}",
                parser.accumulated
            ))
        } else if let Some(PartialExpression::Operation(Transformation {
            operator: Operator::Type,
            operand: Expression::Type(type_),
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
        construct_expression: fn(&mut VecDeque<PartialExpression>) -> Result<Expression, AnyError>,
    ) -> Result<(), AnyError> {
        let expression = construct_expression(&mut self.accumulated)?;
        self.push(expression);
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

    fn push(&mut self, expression: Expression) {
        self.push_pe(PartialExpression::Expression(expression));
    }
    fn push_pe(&mut self, partial_expression: PartialExpression) {
        self.accumulated.push_front(partial_expression);
    }
}

fn construct_transformation(
    parser: &mut Parser,
    operator: Operator,
) -> Result<PartialExpression, AnyError> {
    let accumulated = &mut parser.accumulated;
    let elem_operand = accumulated.pop_front();
    let transformation = if let Operator::Type = operator {
        if let Some(PartialExpression::Expression(Expression::Identifier(typename))) = elem_operand
        {
            let operand = get_type_maybe_pop_children(accumulated, typename);
            Transformation { operator, operand }
        } else if let Some(PartialExpression::Expression(Expression::Type(type_))) = elem_operand {
            let operand = Expression::Type(type_);
            Transformation { operator, operand }
        } else {
            error_expected("type or type name after type operator ':'", elem_operand)?
        }
    } else {
        if let Some(PartialExpression::Expression(operand)) = elem_operand {
            Transformation { operator, operand }
        } else if operator == Operator::Ignore {
            let operand = if let None = elem_operand {
                Expression::empty_chain()
            } else if let Some(PartialExpression::CloseBrace) = elem_operand {
                accumulated.push_front(PartialExpression::CloseBrace);
                Expression::empty_chain()
            } else {
                error_expected("expression or close brace or end of file", elem_operand)?
            };
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
) -> Expression {
    let maybe_children = accumulated.pop_front();
    match maybe_children {
        Some(PartialExpression::ChildrenTypes(children)) => {
            return Expression::Type(Type::from(typename, children));
        }
        Some(not_children) => accumulated.push_front(not_children),
        None => {}
    }
    Expression::Type(Type::simple(typename))
}

fn construct_keyword(parser: &mut Parser, keyword: Keyword) -> Result<PartialExpression, AnyError> {
    let accumulated = &mut parser.accumulated;
    match keyword {
        Keyword::Function => construct_function(accumulated),
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
) -> Result<PartialExpression, AnyError> {
    let elem = accumulated.pop_front();

    let (parameter, elem) = extract_single_child_type(accumulated, elem);

    if let Some(PartialExpression::Expression(Expression::Chain(body))) = elem {
        Ok(PartialExpression::Expression(Expression::function(
            parameter, body,
        )))
    } else {
        let (returned, elem) = extract_single_child_type(accumulated, elem);

        if let Some(elem) = elem {
            accumulated.push_front(elem);
        }

        Ok(PartialExpression::Expression(Expression::Type(
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

    if let Some(PartialExpression::Expression(Expression::Chain(body))) = elem {
        let elem = accumulated.pop_front();
        if let Some(PartialExpression::Expression(Expression::Chain(otherwise))) = elem {
            Ok(PartialExpression::Expression(factory(
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

    if let Some(PartialExpression::Expression(Expression::Chain(body))) = elem {
        Ok(PartialExpression::Expression(factory(parameter, body)))
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
    if let Some(PartialExpression::Expression(Expression::Chain(yes))) = elem {
        let elem = accumulated.pop_front();
        if let Some(PartialExpression::Expression(Expression::Chain(no))) = elem {
            Ok(PartialExpression::Expression(Expression::Composed(
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
            operator: Operator::Assignment,
            operand: Expression::Identifier(name),
        })) = elem
        {
            // parser.identifiers.insert(name.clone(), expr);
            let qualified =
                if let (Some(root), Some(file)) = (parser.root.as_ref(), parser.file.as_ref()) {
                    qualify(&name, root, file)?
                } else {
                    name
                };
            parser.exported.insert(qualified.clone(), expr);
            Ok(PartialExpression::Expression(Expression::Identifier(
                qualified,
            )))
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
        Ok(PartialExpression::Expression(Expression::Composed(
            Composed::Cast(Cast { target_type }),
        )))
    } else {
        error_expected("type inside parenthesis", elem)
    }
}

fn construct_chain(accumulated: &mut VecDeque<PartialExpression>) -> Result<Expression, AnyError> {
    let elem_expression = accumulated.pop_front();
    match elem_expression {
        Some(PartialExpression::CloseBrace) => Ok(Expression::empty_chain()),
        Some(PartialExpression::Expression(initial)) => {
            construct_chain_transformations(accumulated, initial)
        }
        _ => error_expected("expression or closing brace", elem_expression),
    }
}

fn construct_chain_transformations(
    accumulated: &mut VecDeque<PartialExpression>,
    initial: Expression,
) -> Result<Expression, AnyError> {
    let mut transformations = Transformations::new();
    loop {
        let elem_operator = accumulated.pop_front();
        match elem_operator {
            Some(PartialExpression::CloseBrace) => {
                return Ok(Expression::chain(Box::new(initial), transformations))
            }
            Some(PartialExpression::Operation(transformation)) => {
                transformations.push(transformation);
            }
            _ => error_expected("operator or closing brace", elem_operator)?,
        }
    }
}

fn construct_array(accumulated: &mut VecDeque<PartialExpression>) -> Result<Expression, AnyError> {
    let mut elements = Vec::new();
    let mut elem = accumulated.pop_front();
    while let Some(PartialExpression::Expression(e)) = elem {
        elements.push(e);
        elem = accumulated.pop_front()
    }
    if let Some(PartialExpression::CloseBracket) = elem {
        Ok(Expression::StaticList { elements })
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
            Some(PartialExpression::Expression(Expression::Identifier(name))) => {
                if let Some(previous_name) = name_opt {
                    types.push(TypedIdentifier::any(previous_name));
                }
                name_opt = Some(name)
            }
            Some(PartialExpression::Operation(Transformation {
                operator: Operator::Type,
                operand: Expression::Type(type_),
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
            Some(PartialExpression::CloseParenthesis) => {
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

pub fn construct_string(string: Vec<u8>) -> PartialExpression {
    let elements = string
        .iter()
        .map(|b| Expression::Value(*b as i64))
        .collect::<Vec<_>>();
    PartialExpression::Expression(Expression::StaticList { elements })
}

fn finish_construction(mut parser: Parser) -> Result<IncompleteProgram, AnyError> {
    let accumulated = &mut parser.accumulated;
    let mut main = if accumulated.len() <= 1 {
        match accumulated.pop_front() {
            Some(PartialExpression::Expression(e)) => e,
            None => Expression::Nothing,
            Some(v) => {
                accumulated.push_front(v);
                return err(format!("unfinished code: {:?}", accumulated));
            }
        }
    } else {
        let error_message = format!("unfinished code: {:?}", accumulated);
        accumulated.push_back(PartialExpression::CloseBrace);
        let e = construct_chain(accumulated)?;
        if !accumulated.is_empty() {
            return err(error_message);
        } else {
            e
        }
    };

    let imported = import(&mut main, &mut parser)?;
    parser.exported.extend(imported);

    Ok(IncompleteProgram {
        main,
        exported: parser.exported,
        available: parser.available,
    })
}
