use crate::lexer::Operator;
use crate::parser::{Expression, Expressions};
use crate::AnyError;

pub fn evaluate(expression: Expression) -> Result<i64, AnyError> {
    match expression {
        Expression::Value(n) => Ok(n),
        Expression::Operation { operator, operands } => evaluate_operation(operator, operands),
        _ => Err(format!("Can't evaluate expression {:?}", expression))?,
    }
}

fn evaluate_operation(operator: Operator, operands: Expressions) -> Result<i64, AnyError> {
    Ok(match operator {
        Operator::Add => {
            let mut accumulated = 0;
            for operand in operands {
                let value = evaluate(operand)?;
                accumulated += value;
            }
            accumulated
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::lex_and_parse;

    #[test]
    fn test_addition() {
        let expression = lex_and_parse("23+5+1000").unwrap();
        let result = evaluate(expression);
        assert_eq!(result.unwrap(), 1028);
    }
}
