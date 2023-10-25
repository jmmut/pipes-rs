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

pub fn parse(tokens: &Tokens) -> Result<Expression, AnyError> {
    if let Some(token) = tokens.first().cloned() {
        match token {
            Token::Number(n) => Ok(Expression::Value(n)),
            Token::Operator { operator } => Ok(Expression::Operator { operator }),
        }
    } else {
        Ok(Expression::Nothing)
    }
}
