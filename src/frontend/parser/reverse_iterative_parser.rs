use std::collections::{HashMap, HashSet, VecDeque};

use crate::common::{context, AnyError};
use crate::frontend::ast::{error_expected, PartialExpression};
use crate::frontend::expression::{
    Branch, Expression, Transformation, Transformations, Type, TypedIdentifier, TypedIdentifiers,
};
use crate::frontend::lexer::{Keyword, Operator, Token, TokenizedSource};
use crate::frontend::parser::import::import;
use crate::frontend::program::Program;

pub fn parse_tokens(tokens: TokenizedSource) -> Result<Program, AnyError> {
    context("Reverse parser", Parser::parse_tokens(tokens))
}
pub struct Parser {
    pub accumulated: VecDeque<PartialExpression>,
    pub identifiers: HashMap<String, IdentifierValue>,
    pub unresolved_identifiers: HashSet<String>,
}

/// present if it's a public identifier, None if private
type IdentifierValue = Option<Expression>;

impl Parser {
    fn parse_tokens(tokens: TokenizedSource) -> Result<Program, AnyError> {
        let mut ast = Parser {
            accumulated: VecDeque::new(),
            identifiers: HashMap::new(),
            unresolved_identifiers: HashSet::new(),
        };
        for token in tokens.tokens.into_iter().rev() {
            match token {
                Token::Number(n) => ast.push(Expression::Value(n)),
                Token::Operator(operator) => {
                    let pe = construct_transformation(&mut ast, operator)?;
                    ast.accumulated.push_front(pe);
                }
                Token::Identifier(ident) => {
                    if !ast.identifiers.contains_key(&ident) {
                        ast.unresolved_identifiers.insert(ident.clone());
                    }
                    ast.push(Expression::Identifier(ident))
                }
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
        let ast = import(ast)?;
        finish_construction(ast)
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
        } else {
            error_expected("type name after type operator ':'", elem_operand)?
        }
    } else {
        if let Some(PartialExpression::Expression(operand)) = elem_operand {
            if let (Expression::Identifier(name), Operator::Assignment) = (&operand, operator) {
                parser.unresolved_identifiers.remove(name);
                parser.identifiers.insert(name.to_string(), None);
            }
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
    Ok(PartialExpression::Transformation(transformation))
}

fn get_type_maybe_pop_children(
    accumulated: &mut VecDeque<PartialExpression>,
    typename: String,
) -> Expression {
    let maybe_children = accumulated.pop_front();
    match maybe_children {
        Some(PartialExpression::ChildrenTypes(children)) => {
            return Expression::Type(Type::children(typename, children))
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
        Keyword::Replace => construct_replace(accumulated),
        Keyword::Map => construct_map(accumulated),
        Keyword::Branch => construct_branch(accumulated),
        Keyword::Public => construct_public(parser),
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
        error_expected("chain for the function body", elem)
    }
}

fn construct_loop(
    accumulated: &mut VecDeque<PartialExpression>,
) -> Result<PartialExpression, AnyError> {
    let elem = accumulated.pop_front();
    let (parameter, elem) = extract_single_child_type(accumulated, elem);

    if let Some(PartialExpression::Expression(Expression::Chain(body))) = elem {
        Ok(PartialExpression::Expression(Expression::loop_(
            parameter, body,
        )))
    } else {
        error_expected("chain for the 'loop' body", elem)
    }
}

fn construct_loop_or(
    accumulated: &mut VecDeque<PartialExpression>,
) -> Result<PartialExpression, AnyError> {
    let elem = accumulated.pop_front();
    let (parameter, elem) = extract_single_child_type(accumulated, elem);

    if let Some(PartialExpression::Expression(Expression::Chain(body))) = elem {
        let elem = accumulated.pop_front();
        if let Some(PartialExpression::Expression(Expression::Chain(otherwise))) = elem {
            return Ok(PartialExpression::Expression(Expression::loop_or(
                parameter, body, otherwise,
            )));
        } else {
            error_expected("chain for the 'loop_or' 'otherwise' body", elem)
        }
    } else {
        error_expected("chain for the 'loop_or' body", elem)
    }
}

fn construct_times(
    accumulated: &mut VecDeque<PartialExpression>,
) -> Result<PartialExpression, AnyError> {
    let elem = accumulated.pop_front();
    let (parameter, elem) = extract_single_child_type(accumulated, elem);

    if let Some(PartialExpression::Expression(Expression::Chain(body))) = elem {
        Ok(PartialExpression::Expression(Expression::times(
            parameter, body,
        )))
    } else {
        error_expected("chain for the 'times' body", elem)
    }
}

fn construct_replace(
    accumulated: &mut VecDeque<PartialExpression>,
) -> Result<PartialExpression, AnyError> {
    let elem = accumulated.pop_front();
    let (parameter, elem) = extract_single_child_type(accumulated, elem);

    if let Some(PartialExpression::Expression(Expression::Chain(body))) = elem {
        Ok(PartialExpression::Expression(Expression::replace(
            parameter, body,
        )))
    } else {
        error_expected("chain for the 'replace' body", elem)
    }
}

fn construct_map(
    accumulated: &mut VecDeque<PartialExpression>,
) -> Result<PartialExpression, AnyError> {
    let elem = accumulated.pop_front();
    let (parameter, elem) = extract_single_child_type(accumulated, elem);

    if let Some(PartialExpression::Expression(Expression::Chain(body))) = elem {
        Ok(PartialExpression::Expression(Expression::map(
            parameter, body,
        )))
    } else {
        error_expected("chain for the 'map' body", elem)
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
            Ok(PartialExpression::Expression(Expression::Branch(Branch {
                yes,
                no,
            })))
        } else {
            error_expected("chain for the branch negative case", elem)
        }
    } else {
        error_expected("chain for the branch positive case", elem)
    }
}

fn construct_public(parser: &mut Parser) -> Result<PartialExpression, AnyError> {
    let elem = parser.accumulated.pop_front();
    if let Some(PartialExpression::Expression(expr)) = elem {
        let elem = parser.accumulated.get(0);
        if let Some(PartialExpression::Transformation(Transformation {
            operator: Operator::Assignment,
            operand: Expression::Identifier(name),
        })) = elem
        {
            parser.identifiers.insert(name.clone(), Some(expr.clone()));
            Ok(PartialExpression::Expression(expr))
        } else {
            error_expected("assignment and identifer after 'public <expression>'", elem)
        }
    } else {
        error_expected("expression after 'public'", elem)
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
            Some(PartialExpression::Transformation(transformation)) => {
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
                    types.push(TypedIdentifier::unknown_type(previous_name));
                }
                name_opt = Some(name)
            }
            Some(PartialExpression::Transformation(Transformation {
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
                    types.push(TypedIdentifier::unknown_type(previous_name));
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

fn finish_construction(mut parser: Parser) -> Result<Program, AnyError> {
    let accumulated = &mut parser.accumulated;
    let main = if accumulated.len() <= 1 {
        match accumulated.pop_front() {
            Some(PartialExpression::Expression(e)) => e,
            None => Expression::Nothing,
            Some(v) => {
                accumulated.push_front(v);
                return Err(format!("unfinished code: {:?}", accumulated).into());
            }
        }
    } else {
        let error_message = format!("unfinished code: {:?}", accumulated);
        accumulated.push_back(PartialExpression::CloseBrace);
        let e = construct_chain(accumulated)?;
        if !accumulated.is_empty() {
            return Err(error_message.into());
        } else {
            e
        }
    };
    Ok(Program {
        main,
        identifiers: parser
            .identifiers
            .into_iter()
            .filter_map(|(name, opt_expr)| opt_expr.map(|expr| (name, expr)))
            .collect(),
    })
}
