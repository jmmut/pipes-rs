use std::collections::HashMap;
use std::io::{Read, Write};
use std::rc::Rc;

use crate::common::context;
use crate::evaluate::intrinsics::Intrinsic;
use crate::frontend::expression::{
    Chain, Expression, Expressions, Function, Loop, Transformation, Transformations,
};
use crate::frontend::lexer::{Comparison, Operator};
use crate::AnyError;

pub type ListPointer = i64;
pub type FunctionPointer = i64;
pub type GenericValue = i64;
pub type BindingsStack = Vec<GenericValue>;
pub const NOTHING: i64 = i64::MIN;

pub struct Runtime<R: Read, W: Write> {
    /// using a map<index,list> instead of a vec<list> to be able to deallocate individual lists
    lists: HashMap<ListPointer, Vec<GenericValue>>,
    functions: Vec<Rc<FunctionOrIntrinsic>>,
    identifiers: HashMap<String, BindingsStack>,
    read_input: R,
    print_output: W,
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
        Print,
        ToStr,
    }
    impl Intrinsic {
        pub fn name(&self) -> &'static str {
            match self {
                Intrinsic::PrintChar => "print_char",
                Intrinsic::ReadChar => "read_char",
                Intrinsic::Print => "print",
                Intrinsic::ToStr => "to_str",
            }
        }
    }
    use Intrinsic::*;
    pub const INTRINSICS: &[Intrinsic] = &[PrintChar, ReadChar, Print, ToStr];
}

impl<R: Read, W: Write> Runtime<R, W> {
    pub fn evaluate(
        expression: Expression,
        read_input: R,
        print_output: W,
    ) -> Result<GenericValue, AnyError> {
        let mut runtime = Self::new(read_input, print_output);
        context("Runtime", runtime.evaluate_recursive(&expression))
    }

    fn new(read_input: R, print_output: W) -> Runtime<R, W> {
        let (identifiers, functions) = Self::build_intrinsics();
        Runtime {
            lists: HashMap::new(),
            functions,
            identifiers,
            read_input,
            print_output,
        }
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
            Expression::StaticList { elements } => self.evaluate_list(elements),
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
            .ok_or_else(|| format!("Bug: Identifier '{}' is not bound to any value", name).into())
    }

    fn get_list(&self, list_pointer: ListPointer) -> Result<&Vec<GenericValue>, AnyError> {
        self.lists
            .get(&list_pointer)
            .ok_or_else(|| format!("Pointer {} is not a valid array", list_pointer).into())
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
                Operator::Concatenate => {
                    accumulated = self.evaluate_concatenate(accumulated, operand)?
                }
                Operator::Comparison(comparison) => {
                    accumulated = Self::evaluate_compare(accumulated, *comparison, operand)?
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
                self.call_function_pointer(argument, function_ptr)
                    .map_err(|err| {
                        format!(
                            "Bug: Identifier '{}' is not a function. Details: {}",
                            function_name, err
                        )
                        .into()
                    })
            }
            Expression::Function(function) => self.call_function_expression(argument, function),
            Expression::Loop(loop_) => self.call_loop_expression(argument, loop_),
            Expression::Branch(branch) => self.evaluate_chain(if argument != 0 {
                &branch.yes
            } else {
                &branch.no
            }),
            Expression::Chain(chain) => {
                let function_pointer = self.evaluate_chain(chain)?;
                self.call_function_pointer(argument, function_pointer)
            }
            _ => Err(format!(
                "Can not use expression as a function: {:?}",
                function
            ))?,
        }
    }

    fn call_function_pointer(
        &mut self,
        argument: i64,
        function_ptr: GenericValue,
    ) -> Result<i64, AnyError> {
        if let Some(function_expr) = self.functions.get(function_ptr as usize).cloned() {
            match function_expr.as_ref() {
                FunctionOrIntrinsic::Function(function) => {
                    self.call_function_expression(argument, function)
                }
                FunctionOrIntrinsic::Intrinsic(intrinsic) => {
                    self.call_intrinsic(argument, *intrinsic)
                }
            }
        } else {
            Err(format!("invalid function pointer {}", function_ptr).into())
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
    fn call_loop_expression(
        &mut self,
        argument: i64,
        Loop {
            iteration_elem,
            body,
        }: &Loop,
    ) -> Result<i64, AnyError> {
        let list = self.get_list(argument)?.clone();
        let mut transformations: Transformations = vec![Transformation {
            operator: Operator::Ignore,
            operand: *body.initial.clone(),
        }];
        transformations.append(&mut body.transformations.clone());
        let mut chain = Chain {
            initial: Box::new(Expression::Nothing),
            transformations,
        };
        let mut result = NOTHING;
        for value in list {
            self.identifiers
                .entry(iteration_elem.name.clone())
                .or_insert(Vec::new())
                .push(value);
            let initial = Box::new(Expression::Value(value));
            chain.initial = initial;
            result = self.evaluate_chain(&chain)?;
            self.unbind_identifier(&iteration_elem.name, 1)?;
            if result != NOTHING {
                break;
            }
        }
        Ok(result)
    }
    fn call_intrinsic(
        &mut self,
        argument: GenericValue,
        intrinsic: Intrinsic,
    ) -> Result<GenericValue, AnyError> {
        match intrinsic {
            Intrinsic::PrintChar => {
                write!(self.print_output, "{}", argument as u8 as char)?;
                Ok(argument)
            }
            Intrinsic::ReadChar => {
                let one_byte_buffer: &mut [u8] = &mut [0; 1];
                self.read_input.read_exact(one_byte_buffer)?;
                Ok(one_byte_buffer[0] as i64)
            }
            Intrinsic::Print => {
                match self.lists.get(&argument) {
                    Some(list) => {
                        let s = String::from_utf8(list.iter().map(|b| *b as u8).collect())?;
                        writeln!(self.print_output, "{}", s)?;
                    }
                    None => Err(format!(
                        "\"print\" was called with an invalid array {}",
                        argument
                    ))?,
                }
                Ok(argument)
            }
            Intrinsic::ToStr => {
                let string = format!("{}", argument);
                let bytes = string.bytes().map(|b| b as i64).collect();
                Ok(self.allocate_list(bytes))
            }
        }
    }

    fn get_list_element(
        &mut self,
        list_pointer: ListPointer,
        operand: &Expression,
    ) -> Result<GenericValue, AnyError> {
        match operand {
            Expression::Value(index) => {
                let list = self.lists.get(&list_pointer).ok_or_else(|| {
                    Into::<AnyError>::into(format!(
                        "Attempted accessing element '{}' of array '{}' \
                             which is not a valid array pointer",
                        index, list_pointer,
                    ))
                })?;
                list.get(*index as usize).cloned().ok_or_else(|| {
                    format!(
                        "Index out of bounds. Index: {}, list ({} elements): {:?}",
                        index,
                        list.len(),
                        list
                    )
                    .into()
                })
            }
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
    fn evaluate_compare(
        accumulated: GenericValue,
        operator: Comparison,
        operand: &Expression,
    ) -> Result<GenericValue, AnyError> {
        if let Expression::Value(value) = operand {
            let compared = match operator {
                Comparison::Equals => accumulated == *value,
                Comparison::LessThan => accumulated < *value,
                Comparison::GreaterThan => accumulated > *value,
                Comparison::LessThanEquals => accumulated <= *value,
                Comparison::GreaterThanEquals => accumulated >= *value,
            };
            Ok(compared as i64)
        } else {
            Err(format!(
                "Can only compare ({:?}) values, not a {:?}",
                operator, operand
            )
            .into())
        }
    }

    fn evaluate_concatenate(
        &mut self,
        accumulated: GenericValue,
        operand: &Expression,
    ) -> Result<ListPointer, AnyError> {
        let second_pointer = match operand {
            Expression::StaticList { elements } => self.evaluate_list(&elements),
            Expression::Identifier(name) => self.get_identifier(name),
            Expression::Chain(chain) => self.evaluate_chain(chain),
            _ => Err(format!(
                "Expected to concatenate two lists, second operand is {:?}",
                operand
            )
            .into()),
        }?;
        let first_elems = self.get_list(accumulated)?;
        let second_elems = self.get_list(second_pointer)?;
        let mut new_list = first_elems.clone();
        new_list.append(&mut second_elems.clone());
        Ok(self.allocate_list(new_list))
    }

    fn evaluate_list(&mut self, elements: &Expressions) -> Result<ListPointer, AnyError> {
        let mut to_allocate = Vec::with_capacity(elements.len());
        for e in elements {
            to_allocate.push(self.evaluate_recursive(e)?);
        }
        Ok(self.allocate_list(to_allocate))
    }

    fn allocate_list(&mut self, to_allocate: Vec<GenericValue>) -> ListPointer {
        let new_pointer = self.lists.len() as i64;
        self.lists.insert(new_pointer, to_allocate);
        new_pointer
    }

    fn allocate_function(&mut self, function: Function) -> Result<FunctionPointer, AnyError> {
        self.functions
            .push(Rc::new(FunctionOrIntrinsic::Function(function)));
        Ok((self.functions.len() - 1) as i64)
    }
}

#[cfg(test)]
mod tests {
    use crate::common::assert_mentions;
    use crate::frontend::lex_and_parse;

    use super::*;

    fn interpret(code_text: &str) -> GenericValue {
        let expression = lex_and_parse(code_text).unwrap();
        let result = Runtime::evaluate(expression, std::io::stdin(), std::io::stdout());
        result.unwrap()
    }
    fn interpret_fallible(code_text: &str) -> Result<GenericValue, AnyError> {
        let expression = lex_and_parse(code_text)?;
        let result = Runtime::evaluate(expression, std::io::stdin(), std::io::stdout());
        result
    }
    fn interpret_io(code_text: &str, read_input: &[u8]) -> (GenericValue, Vec<u8>) {
        let print_output = Vec::<u8>::new();
        let expression = lex_and_parse(code_text).unwrap();
        let mut runtime = Runtime::new(read_input, print_output);
        let result = runtime.evaluate_recursive(&expression);
        (result.unwrap(), runtime.print_output)
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
    fn test_get_non_existing_element() {
        let err = interpret_fallible("[5 6 7] #3").expect_err("should have failed");
        assert_mentions(err, &["index", "3", "out of bounds"])
    }
    #[test]
    fn test_get_on_non_array() {
        let err = interpret_fallible("4 #3").expect_err("should have failed");
        assert_mentions(err, &["array", "4"])
    }
    #[test]
    fn test_get_on_non_number_index() {
        let err = interpret_fallible("4 #[]").expect_err("should have failed");
        assert_mentions(err, &["index"])
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
        let err = interpret_fallible("4|3").expect_err("should have failed");
        assert_mentions(err, &["as a function", "3"])
    }
    #[test]
    fn test_function_closure() {
        assert_eq!(
            interpret(
                r#"
        3 |{
            5 |function(x){
                function(y){
                    x-y
                }
            }
        }"#
            ),
            2
        );
    }
    #[test]
    fn test_pass_function() {
        assert_eq!(
            interpret("function(x) {x +1} | function(increment) {6 |increment}"),
            7
        );
    }
    #[test]
    fn test_branch() {
        assert_eq!(interpret("1 |branch {5} {7}"), 5);
        assert_eq!(interpret("0 |branch {5} {7}"), 7);
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
    fn test_intrinsics() {
        let mut out = Vec::<u8>::new();
        let into = vec![b'7'];
        let expression = lex_and_parse("5 +48 |print_char; 0|read_char").unwrap();
        let result = Runtime::evaluate(expression, &*into, &mut out);
        assert_eq!(result.unwrap() as u8, '7' as u8);
        assert_eq!(out, vec![b'5']);
    }
    #[test]
    fn test_to_str() {
        let result = interpret("123 |to_str #1");
        assert_eq!(result, b'2' as i64);
    }
    #[test]
    fn test_print_wrong_array() {
        let err = interpret_fallible("0|print").expect_err("should have failed");
        assert_mentions(err, &["print", "invalid array"])
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

    #[test]
    fn test_wrong_assignment() {
        let err = interpret_fallible("4+1=5").expect_err("should have failed");
        assert_mentions(err, &["assign", "5"]);
    }
    #[test]
    fn test_concat() {
        assert_eq!(interpret("[10 11] ++[12 13] #2"), 12);
        assert_eq!(interpret("[10 11] |function(list) {list ++[12 13] #2}"), 12);
        assert_eq!(interpret("[10 11] |function(list) {[12 13] ++list #2}"), 10);
        assert_eq!(interpret("[10 11] ++{[12] ++[13]} #2"), 12);
    }

    #[test]
    fn test_loop() {
        let (result, print_output) = interpret_io("[10 11] |loop(n :i64) {n |to_str |print;}", &[]);
        assert_eq!(result, NOTHING);
        assert_eq!(&String::from_utf8(print_output).unwrap(), "10\n11\n")
    }

    #[test]
    fn test_loop_broken() {
        let (result, print_output) =
            interpret_io("[10 11] |loop(n :i64) {n |to_str |print; 5}", &[]);
        assert_eq!(result, 5);
        assert_eq!(&String::from_utf8(print_output).unwrap(), "10\n")
    }
    #[test]
    fn test_comparison() {
        assert_eq!(interpret("5 <9"), 1);
        assert_eq!(interpret("5 <4"), 0);
        assert_eq!(interpret("5 <5"), 0);

        assert_eq!(interpret("5 <=9"), 1);
        assert_eq!(interpret("5 <=4"), 0);
        assert_eq!(interpret("5 <=5"), 1);

        assert_eq!(interpret("5 >9"), 0);
        assert_eq!(interpret("5 >4"), 1);
        assert_eq!(interpret("5 >5"), 0);

        assert_eq!(interpret("5 >=9"), 0);
        assert_eq!(interpret("5 >=4"), 1);
        assert_eq!(interpret("5 >=5"), 1);

        assert_eq!(interpret("5 =?9"), 0);
        assert_eq!(interpret("5 =?4"), 0);
        assert_eq!(interpret("5 =?5"), 1);
    }
}
