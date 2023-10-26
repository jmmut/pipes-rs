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
    AppliedTransformation {
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
    parse_recursive(&mut iter)
}
pub fn parse_recursive(iter: &mut impl Iterator<Item = Token>) -> Result<Expression, AnyError> {
    let initial = if let Some(token) = iter.next() {
        parse_atom(iter, token)?
    } else {
        return Ok(Expression::Nothing);
    };
    let mut transformations = Vec::new();
    while let Some(token) = iter.next() {
        let operator = match token {
            Token::Operator(operator) => operator,
            _ => return Err(format!("unexpected token {:?}, expected operator", token))?,
        };
        if let Some(token) = iter.next() {
            let operand = parse_atom(iter, token)?;
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
        let top_level = Expression::AppliedTransformation {
            initial: Box::new(initial),
            transformations,
        };
        Ok(top_level)
    }
}

fn parse_atom(
    iter: &mut impl Iterator<Item = Token>,
    token: Token,
) -> Result<Expression, AnyError> {
    Ok(match token {
        Token::Number(n) => Expression::Value(n),
        Token::Identifier(name) => Expression::Identifier(name),
        Token::OpenBracket => parse_list(iter)?,
        _ => return Err(format!("unexpected token {:?}, expected expression", token))?,
    })
}

fn parse_list(iter: &mut impl Iterator<Item = Token>) -> Result<Expression, AnyError> {
    let mut elements = Expressions::new();
    while let Some(token) = iter.next() {
        match token {
            Token::CloseBracket => return Ok(Expression::StaticList(StaticList { elements })),
            _ => elements.push(parse_atom(iter, token)?),
        }
    }
    Err("Unclosed square bracket")?
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::lexer::lex;

    #[test]
    fn add_numbers() {
        let expression = parse(vec![Token::Number(5), Token::add(), Token::Number(7)]).unwrap();
        assert_eq!(
            expression,
            Expression::AppliedTransformation {
                initial: Box::new(Expression::Value(5)),
                transformations: vec![Transformation {
                    operator: Operator::Add,
                    operand: Expression::Value(7)
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
            Expression::AppliedTransformation {
                initial: Box::new(Expression::Value(5)),
                transformations: vec![
                    Transformation {
                        operator: Operator::Add,
                        operand: Expression::Value(7)
                    },
                    Transformation {
                        operator: Operator::Add,
                        operand: Expression::Value(12)
                    },
                    Transformation {
                        operator: Operator::Add,
                        operand: Expression::Value(34)
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
            Expression::AppliedTransformation {
                initial: Box::new(Expression::Value(5)),
                transformations: vec![Transformation {
                    operator: Operator::Call,
                    operand: Expression::Identifier("print_char".to_string()),
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
                elements: vec![
                    Expression::Value(5),
                    Expression::Value(6),
                    Expression::Value(7),
                ]
            })
        );
    }
}
