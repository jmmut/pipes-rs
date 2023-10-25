use crate::lexer::{Operator, Token, Tokens};
use crate::AnyError;

#[derive(PartialEq, Debug)]
pub enum Expression {
    Nothing,
    Value(i64),
    Operation {
        operands: Vec<Expression>,
        operator: Operator,
    },
    Operator {
        operator: Operator,
    },
}

pub fn parse(tokens: Tokens) -> Result<Expression, AnyError> {
    let mut iter = tokens.into_iter();
    let mut top_level = if let Some(token) = iter.next() {
        match token {
            Token::Number(n) => Expression::Value(n),
            Token::Operator { operator } => Expression::Operator { operator },
        }
    } else {
        return Ok(Expression::Nothing);
    };
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
            top_level = Expression::Operation {
                operands: vec![top_level, expression],
                operator,
            }
        } else {
            return Err(format!(
                "unfinished operation after operator {:?}",
                operator
            ))?;
        }
    }
    Ok(top_level)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn add_numbers() {
        let expression = parse(vec![Token::Number(5), Token::add(), Token::Number(7)]).unwrap();
        assert_eq!(
            expression,
            Expression::Operation {
                operands: vec![Expression::Value(5), Expression::Value(7)],
                operator: Operator::Add
            }
        )
    }
}
