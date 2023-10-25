use crate::frontend::lexer::Operator;
use crate::frontend::parser::{Expression, Transformation, Transformations};
use crate::AnyError;

pub fn evaluate(expression: Expression) -> Result<i64, AnyError> {
    match expression {
        Expression::Value(n) => Ok(n),
        Expression::AppliedTransformation {
            initial,
            transformations,
        } => evaluate_transformation(initial, transformations),
        _ => Err(format!("Can't evaluate expression {:?}", expression))?,
    }
}

fn evaluate_transformation(
    initial: Box<Expression>,
    transformations: Transformations,
) -> Result<i64, AnyError> {
    let mut accumulated = evaluate(*initial)?;
    for Transformation {
        operator,
        expression,
    } in transformations
    {
        match operator {
            Operator::Add => {
                let value = evaluate(expression)?;
                accumulated += value;
            }
        }
    }
    Ok(accumulated)
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
