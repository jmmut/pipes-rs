use crate::common::context;
use crate::frontend::lexer::{Operator, Token, Tokens};
use crate::AnyError;

#[derive(PartialEq, Debug)]
pub enum Expression {
    Nothing,
    Value(i64),
    Identifier(String),
    // Operator {
    //     operator: Operator,
    // },
    // Operation {
    //     operator: Operator,
    //     operands: Expressions,
    // },
    // Transformations {
    //     transformations: Transformations,
    // },
    Chain {
        initial: Box<Expression>,
        transformations: Transformations,
    },
    StaticList(StaticList),
}

// pub type Identifier = String;

#[derive(PartialEq, Debug)]
pub struct Transformation {
    pub operator: Operator,
    pub operand: Expression,
}

#[derive(PartialEq, Debug)]
pub struct StaticList {
    pub elements: Expressions,
}

pub type Expressions = Vec<Expression>;
pub type Transformations = Vec<Transformation>;

pub fn parse(tokens: Tokens) -> Result<Expression, AnyError> {
    let mut iter = tokens.into_iter();
    context("Parser", parse_chain(&mut iter))
}
pub fn parse_chain(iter: &mut impl Iterator<Item = Token>) -> Result<Expression, AnyError> {
    let initial = parse_expression(iter)?.unwrap_or(Expression::Nothing);
    let mut transformations = Vec::new();
    while let Some(token) = iter.next() {
        let operator = match token {
            Token::Operator(operator) => operator,
            Token::CloseBrace => break,
            _ => return Err(format!("unexpected token {:?}, expected operator", token))?,
        };
        if let Some(operand) = parse_expression(iter)? {
            transformations.push(Transformation { operator, operand });
        } else {
            return Err(format!(
                "unfinished operation after operator {:?}",
                operator
            ))?;
        }
    }
    if transformations.is_empty() {
        return Ok(initial);
    } else {
        let top_level = Expression::Chain {
            initial: Box::new(initial),
            transformations,
        };
        Ok(top_level)
    }
}

fn parse_expression(
    iter: &mut impl Iterator<Item = Token>,
) -> Result<Option<Expression>, AnyError> {
    if let Some(token) = iter.next() {
        Ok(Some(parse_expression_from_token(token, iter)?))
    } else {
        Ok(None)
    }
}

fn parse_expression_from_token(
    token: Token,
    iter: &mut impl Iterator<Item = Token>,
) -> Result<Expression, AnyError> {
    let expression = match token {
        Token::Number(n) => Expression::Value(n),
        Token::Identifier(name) => Expression::Identifier(name),
        Token::OpenBracket => parse_list(iter)?,
        Token::OpenBrace => parse_chain(iter)?,
        _ => return Err(format!("unexpected token {:?}, expected expression", token))?,
    };
    Ok(expression)
}

fn parse_list(iter: &mut impl Iterator<Item = Token>) -> Result<Expression, AnyError> {
    let mut elements = Expressions::new();
    while let Some(token) = iter.next() {
        match token {
            Token::CloseBracket => return Ok(Expression::StaticList(StaticList { elements })),
            _ => elements.push(parse_expression_from_token(token, iter)?),
        }
    }
    Err("Unclosed square bracket")?
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::lexer::lex;
    use crate::frontend::parser::Expression::Identifier;
    use Expression::{Chain, Value};

    #[test]
    fn add_numbers() {
        let expression = parse(vec![
            Token::Number(5),
            Token::Operator(Operator::Add),
            Token::Number(7),
        ])
        .unwrap();
        assert_eq!(
            expression,
            Chain {
                initial: Box::new(Value(5)),
                transformations: vec![Transformation {
                    operator: Operator::Add,
                    operand: Value(7)
                }],
            }
        )
    }

    #[test]
    fn test_add_several_numbers() {
        let tokens = lex("5+7+12+34").unwrap();
        let expression = parse(tokens).unwrap();
        assert_eq!(
            expression,
            Chain {
                initial: Box::new(Value(5)),
                transformations: vec![
                    Transformation {
                        operator: Operator::Add,
                        operand: Value(7)
                    },
                    Transformation {
                        operator: Operator::Add,
                        operand: Value(12)
                    },
                    Transformation {
                        operator: Operator::Add,
                        operand: Value(34)
                    },
                ],
            }
        );
    }

    #[test]
    fn test_call() {
        let tokens = lex("5|print_char").unwrap();
        let expression = parse(tokens).unwrap();

        assert_eq!(
            expression,
            Chain {
                initial: Box::new(Value(5)),
                transformations: vec![Transformation {
                    operator: Operator::Call,
                    operand: Identifier("print_char".to_string()),
                }],
            }
        );
    }

    #[test]
    fn test_list() {
        let tokens = lex("[5 6 7]").unwrap();
        let parsed = parse(tokens);

        assert_eq!(
            parsed.unwrap(),
            Expression::StaticList(StaticList {
                elements: vec![Value(5), Value(6), Value(7),]
            })
        );
    }
}
