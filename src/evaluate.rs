use crate::common::context;
use crate::frontend::expression::{Expression, StaticList, Transformation, Transformations};
use crate::frontend::lexer::Operator;
use crate::AnyError;
use std::collections::HashMap;

pub type ListPointer = i64;
pub type GenericValue = i64;
pub const NOTHING: i64 = i64::MIN;

pub struct Runtime {
    lists: HashMap<ListPointer, Vec<GenericValue>>,
}

#[allow(unused)]
fn unimplemented<T>() -> Result<T, AnyError> {
    Err("unimplemented".into())
}

impl Runtime {
    pub fn evaluate(expression: Expression) -> Result<GenericValue, AnyError> {
        let mut runtime = Runtime {
            lists: HashMap::new(),
        };
        context("Runtime", runtime.evaluate_recursive(expression))
    }

    fn evaluate_recursive(&mut self, expression: Expression) -> Result<GenericValue, AnyError> {
        match expression {
            Expression::Value(n) => Ok(n),
            Expression::Chain {
                initial,
                transformations,
            } => self.evaluate_applied_transformation(initial, transformations),
            Expression::Nothing => Ok(NOTHING),
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
                Operator::Type => {}
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
                .ok_or("Tried to access elements of something that is not a valid array")?
                .get(index as usize)
                .ok_or("Index out of bounds")?), // TODO: add info
            _ => Err("Index should be an integer")?, // TODO: add info
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

    fn interpret(code_text: &str) -> GenericValue {
        let expression = lex_and_parse(code_text).unwrap();
        let result = Runtime::evaluate(expression);
        result.unwrap()
    }

    #[test]
    fn test_addition() {
        assert_eq!(interpret("22 + 5 + 1000"), 1027);
    }

    #[test]
    fn test_substraction() {
        assert_eq!(interpret("22 - 5 + 1000 - 10"), 1007);
    }

    #[test]
    fn test_ignore() {
        assert_eq!(interpret("22 + 5 ; 1000"), 1000);
    }

    #[test]
    fn test_get_element() {
        assert_eq!(interpret("[5 6 7] #1"), 6);
    }
    #[test]
    fn test_nested_array_operations() {
        assert_eq!(interpret("[{5 - 6} 7] #0"), -1);
    }
    #[test]
    fn test_evaluate_nothing() {
        assert_eq!(interpret("{}"), NOTHING);
    }
}
