use crate::frontend::lexer::Operator;
use crate::frontend::parser::{Expression, StaticList, Transformation, Transformations};
use crate::AnyError;
use std::collections::HashMap;

pub type ListPointer = i64;
pub type GenericValue = i64;

pub struct Runtime {
    lists: HashMap<ListPointer, Vec<GenericValue>>,
}

impl Runtime {
    pub fn evaluate(expression: Expression) -> Result<GenericValue, AnyError> {
        let mut runtime = Runtime {
            lists: HashMap::new(),
        };
        runtime.evaluate_recursive(expression)
    }

    fn evaluate_recursive(&mut self, expression: Expression) -> Result<GenericValue, AnyError> {
        match expression {
            Expression::Value(n) => Ok(n),
            Expression::AppliedTransformation {
                initial,
                transformations,
            } => self.evaluate_applied_transformation(initial, transformations),
            Expression::StaticList(list) => self.allocate_list(list),
            _ => Err(format!("Can't evaluate expression {:?}", expression))?,
        }
    }

    fn evaluate_applied_transformation(
        &mut self,
        initial: Box<Expression>,
        transformations: Transformations,
    ) -> Result<i64, AnyError> {
        let mut accumulated = self.evaluate_recursive(*initial)?;
        for Transformation { operator, operand } in transformations {
            match operator {
                Operator::Add => accumulated += self.evaluate_recursive(operand)?,
                Operator::Substract => accumulated -= self.evaluate_recursive(operand)?,
                Operator::Ignore => accumulated = self.evaluate_recursive(operand)?,
                Operator::Call => accumulated = self.call_intrinsic(accumulated, operand)?,
                Operator::Get => accumulated = self.get_list_element(accumulated, operand)?,
            }
        }
        Ok(accumulated)
    }

    fn allocate_list(&mut self, list: StaticList) -> Result<ListPointer, AnyError> {
        let mut to_allocate = Vec::with_capacity(list.elements.len());
        for e in list.elements {
            to_allocate.push(self.evaluate_recursive(e)?);
        }
        let new_pointer = self.lists.len() as i64;
        self.lists.insert(new_pointer, to_allocate);
        Ok(new_pointer)
    }

    fn get_list_element(
        &mut self,
        list_pointer: ListPointer,
        operand: Expression,
    ) -> Result<GenericValue, AnyError> {
        match operand {
            Expression::Value(index) => Ok(*self
                .lists
                .get(&list_pointer)
                .ok_or("")?
                .get(index as usize)
                .ok_or("")?),
            _ => Err("")?,
        }
    }

    fn call_intrinsic(&mut self, parameter: i64, function: Expression) -> Result<i64, AnyError> {
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
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::lex_and_parse;

    #[test]
    fn test_addition() {
        let expression = lex_and_parse("23+5+1000").unwrap();
        let result = Runtime::evaluate(expression);
        assert_eq!(result.unwrap(), 1028);
    }

    #[test]
    fn test_substraction() {
        let expression = lex_and_parse("23-5+1000-10").unwrap();
        let result = Runtime::evaluate(expression);
        assert_eq!(result.unwrap(), 1008);
    }
    #[test]
    fn test_ignore() {
        let expression = lex_and_parse("23+5;1000").unwrap();
        let result = Runtime::evaluate(expression);
        assert_eq!(result.unwrap(), 1000);
    }
    #[test]
    fn test_get_element() {
        let expression = lex_and_parse("[5 6 7] #1").unwrap();
        let result = Runtime::evaluate(expression);
        assert_eq!(result.unwrap(), 6);
    }
}
