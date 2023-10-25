use crate::frontend::lexer::{Operator, Token, Tokens};
use crate::AnyError;

#[derive(PartialEq, Debug)]
pub enum Expression {
    Nothing,
    Value(i64),
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
}

#[derive(PartialEq, Debug)]
pub struct Transformation {
    pub operator: Operator,
    pub expression: Expression,
}

// pub type Expressions = Vec<Expression>;
pub type Transformations = Vec<Transformation>;

pub fn parse(tokens: Tokens) -> Result<Expression, AnyError> {
    let mut iter = tokens.into_iter();
    let initial = if let Some(token) = iter.next() {
        match token {
            Token::Number(n) => Expression::Value(n),
            _ => return Err(format!("unexpected token {:?}, expected expression", token))?,
        }
    } else {
        return Ok(Expression::Nothing);
    };
    let mut transformations = Vec::new();
    while let Some(token) = iter.next() {
        let operator = match token {
            Token::Operator { operator } => operator,
            _ => return Err(format!("unexpected token {:?}, expected operator", token))?,
        };
        if let Some(token) = iter.next() {
            let expression = match token {
                Token::Number(n) => Expression::Value(n),
                _ => return Err(format!("unexpected token {:?}, expected expression", token))?,
            };
            transformations.push(Transformation {
                operator,
                expression,
            });
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
                    expression: Expression::Value(7)
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
                        expression: Expression::Value(7)
                    },
                    Transformation {
                        operator: Operator::Add,
                        expression: Expression::Value(12)
                    },
                    Transformation {
                        operator: Operator::Add,
                        expression: Expression::Value(34)
                    },
                ],
            }
        );
    }
}
