use crate::frontend::lexer::Operator;
use crate::frontend::parser::{Expression, Transformation, Transformations};
use crate::AnyError;

pub fn evaluate(expression: Expression) -> Result<i64, AnyError> {
    match expression {
        Expression::Value(n) => Ok(n),
        Expression::AppliedTransformation {
            initial,
            transformations,
        } => evaluate_applied_transformation(initial, transformations),
        _ => Err(format!("Can't evaluate expression {:?}", expression))?,
    }
}

fn evaluate_applied_transformation(
    initial: Box<Expression>,
    transformations: Transformations,
) -> Result<i64, AnyError> {
    let mut accumulated = evaluate(*initial)?;
    for Transformation { operator, operand } in transformations {
        match operator {
            Operator::Add => accumulated += evaluate(operand)?,
            Operator::Substract => accumulated -= evaluate(operand)?,
            Operator::Ignore => accumulated = evaluate(operand)?,
            Operator::Call => accumulated = call_intrinsic(accumulated, operand)?,
        }
    }
    Ok(accumulated)
}

fn call_intrinsic(parameter: i64, function: Expression) -> Result<i64, AnyError> {
    match function {
        Expression::Identifier(function_name) => {
            if function_name == "print_char" {
                print!("{}", parameter as u8 as char);
                Ok(parameter)
            } else {
                Err(format!("Unknown function '{function_name}'"))?
            }
        }
        _ => Err(format!(
            "Can not use expression as a function: {:?}",
            function
        ))?,
    }
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

    #[test]
    fn test_substreaction() {
        let expression = lex_and_parse("23-5+1000-10").unwrap();
        let result = evaluate(expression);
        assert_eq!(result.unwrap(), 1008);
    }
    #[test]
    fn test_ignore() {
        let expression = lex_and_parse("23+5;1000").unwrap();
        let result = evaluate(expression);
        assert_eq!(result.unwrap(), 1000);
    }
}
