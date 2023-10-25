use crate::lexer::{Token, Tokens};
use crate::AnyError;

#[derive(PartialEq, Debug)]
pub enum Expression {
    Nothing,
    Value(i64),
    // Operation {
    //     operands: Vec<Expression>,
    //     operator: Operator,
    // },
}

// #[derive(PartialEq, Debug)]
// pub enum Operator {
//     Add,
// }

pub fn parse(tokens: &Tokens) -> Result<Expression, AnyError> {
    if let Some(token) = tokens.first() {
        match token {
            Token::Number(n) => Ok(Expression::Value(*n)),
        }
    } else {
        Ok(Expression::Nothing)
    }
}
