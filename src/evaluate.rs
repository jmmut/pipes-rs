use std::collections::HashMap;
use std::io::Read;
use std::rc::Rc;

use crate::common::context;
use crate::evaluate::intrinsics::Intrinsic;
use crate::frontend::expression::{Chain, Expression, Expressions, Function, Transformation};
use crate::frontend::lexer::Operator;
use crate::AnyError;

pub type ListPointer = i64;
pub type FunctionPointer = i64;
pub type GenericValue = i64;
pub type BindingsStack = Vec<GenericValue>;
pub const NOTHING: i64 = i64::MIN;

pub struct Runtime {
    /// using a map<index,list> instead of a vec<list> to be able to deallocate individual lists
    lists: HashMap<ListPointer, Vec<GenericValue>>,
    functions: Vec<Rc<FunctionOrIntrinsic>>,
    identifiers: HashMap<String, BindingsStack>,
}

enum FunctionOrIntrinsic {
    Function(Function),
    Intrinsic(Intrinsic),
}

#[allow(unused)]
fn unimplemented<T>() -> Result<T, AnyError> {
    Err("unimplemented".into())
}

mod intrinsics {
    #[derive(Copy, Clone)]
    pub enum Intrinsic {
        PrintChar,
        ReadChar,
    }
    impl Intrinsic {
        pub fn name(&self) -> &'static str {
            match self {
                Intrinsic::PrintChar => "print_char",
                Intrinsic::ReadChar => "read_char",
            }
        }
    }
    use Intrinsic::*;
    pub const INTRINSICS: &[Intrinsic] = &[PrintChar, ReadChar];
}

impl Runtime {
    pub fn evaluate(expression: Expression) -> Result<GenericValue, AnyError> {
        let (identifiers, functions) = Self::build_intrinsics();
        let mut runtime = Runtime {
            lists: HashMap::new(),
            functions,
            identifiers,
        };
        context("Runtime", runtime.evaluate_recursive(&expression))
    }

    fn build_intrinsics() -> (
        HashMap<String, Vec<GenericValue>>,
        Vec<Rc<FunctionOrIntrinsic>>,
    ) {
        let mut identifiers = HashMap::new();
        let mut functions = Vec::new();
        let mut i = 0;
        for intrinsic in intrinsics::INTRINSICS {
            functions.push(Rc::new(FunctionOrIntrinsic::Intrinsic(*intrinsic)));
            identifiers.insert(intrinsic.name().to_string(), vec![i]);
            i += 1;
        }
        (identifiers, functions)
    }

    fn evaluate_recursive(&mut self, expression: &Expression) -> Result<GenericValue, AnyError> {
        match expression {
            Expression::Nothing => Ok(NOTHING),
            Expression::Value(n) => Ok(*n),
            Expression::Identifier(name) => self.get_identifier(name),
            Expression::Chain(chain) => self.evaluate_chain(chain),
            Expression::StaticList { elements } => self.allocate_list(elements),
            Expression::Function(function) => self.allocate_function(function.clone()),
            _ => Err(format!("Can't evaluate expression {:?}", expression))?,
        }
    }

    fn get_identifier(&self, name: &String) -> Result<GenericValue, AnyError> {
        self.identifiers
            .get(name)
            .ok_or_else(|| format!("Bug: Undefined identifier {}. This should have been detected by earlier stages.", name))?
            .last()
            .cloned()
            .ok_or_else(|| format!("Bug: Identifier '{}' is not binded to any value", name).into())
    }

    fn evaluate_chain(
        &mut self,
        Chain {
            initial,
            transformations,
        }: &Chain,
    ) -> Result<i64, AnyError> {
        let mut identifiers = HashMap::<String, usize>::new();
        let mut accumulated = self.evaluate_recursive(&*initial)?;
        for Transformation { operator, operand } in transformations {
            match operator {
                Operator::Add => accumulated += self.evaluate_recursive(operand)?,
                Operator::Substract => accumulated -= self.evaluate_recursive(operand)?,
                Operator::Ignore => accumulated = self.evaluate_recursive(operand)?,
                Operator::Call => accumulated = self.call_function(accumulated, operand)?,
                Operator::Get => accumulated = self.get_list_element(accumulated, operand)?,
                Operator::Type => {}
                Operator::Assignment => {
                    self.evaluate_assignment(accumulated, operand, &mut identifiers)?
                }
            }
        }
        for (identifier, times_redefined_in_this_chain) in identifiers {
            self.unbind_identifier(&identifier, times_redefined_in_this_chain)?;
        }
        Ok(accumulated)
    }

    fn unbind_identifier(&mut self, identifier: &String, times: usize) -> Result<(), AnyError> {
        let option = self.identifiers.get_mut(identifier);
        let stack = option.ok_or_else(|| {
            format!(
                "Bug: tried to unbind non-existing identifier {}",
                identifier
            )
        })?;
        if times > stack.len() {
            return Err(format!(
                "Bug: tried to unbind identifier {} {} times, but it's shadowed only {} times",
                identifier,
                times,
                stack.len()
            )
            .into());
        } else {
            stack.truncate(times);
            Ok(())
        }
    }

    fn call_function(&mut self, argument: i64, function: &Expression) -> Result<i64, AnyError> {
        match function {
            Expression::Identifier(function_name) => {
                let function_ptr = self.get_identifier(function_name)?;
                if let Some(function_expr) = self.functions.get(function_ptr as usize).cloned() {
                    match function_expr.as_ref() {
                        FunctionOrIntrinsic::Function(function) => {
                            self.call_function_expression(argument, function)
                        }
                        FunctionOrIntrinsic::Intrinsic(intrinsic) => {
                            Self::call_intrinsic(argument, *intrinsic)
                        }
                    }
                } else {
                    Err(format!("Bug: Identifier '{}' is not a function", function_name).into())
                }
            }
            Expression::Function(function) => self.call_function_expression(argument, function),
            _ => Err(format!(
                "Can not use expression as a function: {:?}",
                function
            ))?,
        }
    }

    fn call_function_expression(
        &mut self,
        argument: i64,
        Function { parameter, body }: &Function,
    ) -> Result<i64, AnyError> {
        self.identifiers
            .entry(parameter.name.clone())
            .or_insert(Vec::new())
            .push(argument);
        let result = self.evaluate_chain(body)?;
        self.unbind_identifier(&parameter.name, 1)?;
        Ok(result)
    }
    fn call_intrinsic(
        argument: GenericValue,
        intrinsic: Intrinsic,
    ) -> Result<GenericValue, AnyError> {
        match intrinsic {
            Intrinsic::PrintChar => {
                print!("{}", argument as u8 as char);
                Ok(argument)
            }
            Intrinsic::ReadChar => {
                let one_byte_buffer: &mut [u8] = &mut [0; 1];
                std::io::stdin().read_exact(one_byte_buffer)?;
                Ok(one_byte_buffer[0] as i64)
            }
        }
    }
    fn get_list_element(
        &mut self,
        list_pointer: ListPointer,
        operand: &Expression,
    ) -> Result<GenericValue, AnyError> {
        let list = self
            .lists
            .get(&list_pointer)
            .ok_or("Tried to access elements of something that is not a valid array")?;
        match operand {
            Expression::Value(index) => list.get(*index as usize).cloned().ok_or_else(|| {
                format!(
                    "Index out of bounds. Index: {}, list ({} elements): {:?}",
                    index,
                    list.len(),
                    list
                )
                .into()
            }),
            _ => Err(format!("Index should be an integer, but was {:?}", operand))?,
        }
    }

    fn evaluate_assignment(
        &mut self,
        accumulated: GenericValue,
        operand: &Expression,
        identifiers: &mut HashMap<String, usize>,
    ) -> Result<(), AnyError> {
        match operand {
            Expression::Identifier(name) => {
                self.identifiers
                    .entry(name.clone())
                    .or_insert(Vec::new())
                    .push(accumulated);
                *identifiers.entry(name.clone()).or_insert(0) += 1;
                Ok(())
            }
            _ => Err(format!("Can only assign to identifiers, not to a {:?}", operand).into()),
        }
    }

    fn allocate_list(&mut self, elements: &Expressions) -> Result<ListPointer, AnyError> {
        let mut to_allocate = Vec::with_capacity(elements.len());
        for e in elements {
            to_allocate.push(self.evaluate_recursive(e)?);
        }
        let new_pointer = self.lists.len() as i64;
        self.lists.insert(new_pointer, to_allocate);
        Ok(new_pointer)
    }

    fn allocate_function(&mut self, function: Function) -> Result<FunctionPointer, AnyError> {
        self.functions
            .push(Rc::new(FunctionOrIntrinsic::Function(function)));
        Ok((self.functions.len() - 1) as i64)
    }
}

#[cfg(test)]
mod tests {
    use crate::frontend::lex_and_parse;

    use super::*;

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
    #[test]
    fn test_function() {
        assert_eq!(interpret("5 |function(x) {x}"), 5);
    }
    #[test]
    fn test_pass_function() {
        assert_eq!(
            interpret("function(x) {x +1} | function(increment) {6 |increment}"),
            7
        );
    }
    #[test]
    #[ignore] // for now
    fn test_pass_branch() {
        assert_eq!(interpret("branch {5} {7} |function(f) {0 |f}"), 7)
    }

    #[test]
    fn test_name_function() {
        assert_eq!(interpret("function(x) {x +1} =increment ;6 |increment"), 7);
    }
    #[test]
    fn test_deshadow_identifier() {
        let code = "
            function (x) { x + 1 }
            =increment
            |function {
                function(x) { x - 1 }
                =increment // this is really decrement, but shadows the outer increment
                ;5
                |increment // really, decrement to 4
            }
            |increment // this should be the real increment, so put back to 5
            ";
        assert_eq!(interpret(code), 5);
    }
    #[test]
    fn test_deshadow_parameter() {
        let code = "
            5
            |function(x) {
                8
                |function(x) { x + 1 }
                ;x
            }";
        assert_eq!(interpret(code), 5);
    }
    #[test]
    fn test_pass_intrinsics() {
        let code = " print_char |function(f) { 5 +48 |f }";
        assert_eq!(interpret(code), 53);
    }
    #[test]
    fn test_deshadow_intrinsics() {
        let code = "
            {
                function(x) {x+1}
                =print_char // shadowing print_char with an increment function
                ;5 + 48
                |print_char // 53 -> 54
            }
            |print_char     // print 54: '6'
            ";
        assert_eq!(interpret(code), 54);
    }
}
