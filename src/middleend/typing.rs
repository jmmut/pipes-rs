use std::collections::HashMap;

use strum::IntoEnumIterator;

use crate::common::{context, err, AnyError};
use crate::frontend::expression::{
    Chain, Composed, Expression, Expressions, Function, Transformation, Type, TypedIdentifier,
};
use crate::frontend::lexer::Operator;
use crate::frontend::program::Program;
use crate::middleend::intrinsics::{builtin_types, BuiltinType, Intrinsic};
use crate::middleend::typing::cast::cast;
use crate::middleend::typing::unify::{all_same_type, unify};

pub mod cast;
pub mod unify;

pub fn check_types(program: &Program) -> Result<(), AnyError> {
    get_type(program).map(|_| ())
}

pub fn get_type(program: &Program) -> Result<Type, AnyError> {
    let mut typer = Typer::new(program)?;
    typer.get_type(&typer.program.main)
}

pub fn is_builtin_nested_type(name: &str) -> Option<&'static str> {
    if name == BuiltinType::Array.name() {
        Some(BuiltinType::Array.name())
    } else if name == BuiltinType::Struct.name() {
        Some(BuiltinType::Struct.name())
    } else if name == BuiltinType::Tuple.name() {
        Some(BuiltinType::Tuple.name())
    } else {
        None
    }
}

pub fn is_builtin_simple_type(name: &str) -> Option<Type> {
    if name == BuiltinType::I64.name() {
        Some(builtin_types::I64)
    } else {
        None
    }
}

type BindingsTypesStack = Vec<Type>;
struct Typer<'a> {
    program: &'a Program,
    identifier_types: HashMap<String, BindingsTypesStack>,
}

impl<'a> Typer<'a> {
    pub fn new(program: &'a Program) -> Result<Self, AnyError> {
        let identifier_types = Self::build_intrinsics();
        let mut typer = Self {
            program,
            identifier_types,
        };
        typer.setup_constants()?;
        Ok(typer)
    }

    fn build_intrinsics() -> HashMap<String, BindingsTypesStack> {
        let mut identifiers = HashMap::new();
        for intrinsic in Intrinsic::iter() {
            identifiers.insert(intrinsic.name().to_string(), vec![intrinsic.type_()]);
        }
        identifiers
    }

    fn setup_constants(&mut self) -> Result<(), AnyError> {
        let mut identifiers_vec = self.program.identifiers.keys().collect::<Vec<_>>();
        loop {
            let mut failed = Vec::<&String>::new();
            let identifiers_count_previous = identifiers_vec.len();
            let mut errors = Vec::new();
            for name in identifiers_vec {
                match context(
                    "Runtime setup",
                    self.get_type(self.program.identifiers.get(name).unwrap()),
                ) {
                    Ok(type_) => self.bind_identifier_type(name.clone(), type_),
                    Err(e) => {
                        errors.push(e);
                        failed.push(name);
                    }
                }
            }
            if failed.len() == 0 {
                return Ok(());
            }
            if failed.len() == identifiers_count_previous {
                let error_intro = if failed.len() == 1 {
                    "A constant has an incorrect type"
                } else {
                    "Some constants have incorrect types. (Are there cyclic dependencies?)"
                };
                let error_messages = errors
                    .iter()
                    .map(|e| e.to_string())
                    .reduce(|accum, e| accum + "\n" + &e)
                    .unwrap();
                return err(format!("{}:{}", error_intro, error_messages));
            }
            identifiers_vec = failed;
        }
    }

    fn bind_identifier_type(&mut self, identifier: String, type_: Type) {
        self.identifier_types
            .entry(identifier)
            .or_insert(Vec::new())
            .push(type_);
    }

    fn unbind_identifier(&mut self, identifier: &String, times: usize) -> Result<(), AnyError> {
        if let Some(stack) = self.identifier_types.get_mut(identifier) {
            if times <= stack.len() {
                stack.truncate(stack.len() - times);
                Ok(())
            } else {
                err(format!(
                    "Bug: tried to unbind identifier {} {} times, but it's shadowed only {} times",
                    identifier,
                    times,
                    stack.len()
                ))
            }
        } else {
            err(format!(
                "Bug: tried to unbind non-existing identifier {}",
                identifier
            ))
        }
    }

    fn get_identifier_type(&self, name: &String) -> Result<Type, AnyError> {
        if let Some(types) = self.identifier_types.get(name) {
            if let Some(last) = types.last() {
                Ok(last.clone())
            } else {
                err(format!(
                    "Bug: Identifier '{}' is not bound to any value",
                    name
                ))
            }
        } else {
            err(format!(
                "Bug: Undefined identifier '{}'. This should have been detected by earlier stages.",
                name
            ))
        }
    }

    pub fn get_type(&mut self, expression: &Expression) -> Result<Type, AnyError> {
        match expression {
            Expression::Nothing => Ok(builtin_types::NOTHING),
            Expression::Value(_) => Ok(builtin_types::I64),
            Expression::Identifier(name) => self.check_types_identifier(name),
            Expression::Type(_) => Ok(builtin_types::TYPE),
            Expression::Chain(chain) => self.check_types_chain(chain),
            Expression::StaticList { elements } => self.check_types_list(elements),
            Expression::Function(function) => self.check_type_function(function),
            Expression::Composed(_) => {
                unimplemented!()
            }
        }
    }
    fn check_types_identifier(&mut self, name: &String) -> Result<Type, AnyError> {
        self.get_identifier_type(name)
    }

    fn check_types_chain(&mut self, chain: &Chain) -> Result<Type, AnyError> {
        let accumulated = &chain.initial;
        let mut accumulated_type = self.get_type(accumulated.as_ref())?;
        let mut assigned_in_this_chain = HashMap::new();
        for t in &chain.transformations {
            accumulated_type = self.get_operation_type(accumulated, t.operator, &t.operand)?;
            if let Transformation {
                operator: Operator::Assignment,
                operand: Expression::Identifier(name),
            } = t
            {
                *assigned_in_this_chain.entry(name.clone()).or_insert(0) += 1;
                self.bind_identifier_type(name.clone(), accumulated_type.clone());
            }
        }
        for (to_unbind, times) in assigned_in_this_chain {
            self.unbind_identifier(&to_unbind, times)?;
        }
        Ok(accumulated_type)
    }

    fn check_type_function(&mut self, function: &Function) -> Result<Type, AnyError> {
        let Function { parameter, body } = function;
        self.bind_identifier_type(parameter.name.clone(), parameter.type_.clone());
        let chain_type = self.check_types_chain(body);
        self.unbind_identifier(&parameter.name, 1)?;
        let function = Type::function(parameter.clone(), TypedIdentifier::nameless(chain_type?));
        Ok(function)
    }

    fn check_types_list(&mut self, elements: &Expressions) -> Result<Type, AnyError> {
        let mut types = Vec::new();
        for e in elements {
            types.push(self.get_type(e)?);
        }
        return if types.len() == 0 {
            Ok(Type::from_nameless(
                BuiltinType::Array.name(),
                vec![builtin_types::ANY],
            ))
        } else if all_same_type(&types) {
            types.truncate(1);
            Ok(Type::from_nameless(BuiltinType::Array.name(), types))
        } else {
            Ok(Type::from_nameless(BuiltinType::Tuple.name(), types))
        };
    }

    fn get_operation_type(
        &mut self,
        input: &Expression,
        operator: Operator,
        operand: &Expression,
    ) -> Result<Type, AnyError> {
        match operator {
            Operator::Add
            | Operator::Substract
            | Operator::Multiply
            | Operator::Divide
            | Operator::Modulo => {
                let unified_input = self.assert_type_unifies(input, &builtin_types::I64)?;
                let unified_operand = self.assert_type_unifies(operand, &unified_input)?;
                return Ok(unified_operand);
            }
            Operator::Ignore => return self.get_type(operand),
            Operator::Call => {
                return self.get_call_type(input, operand);
            }
            Operator::Get => {
                let array =
                    Type::from_nameless(BuiltinType::Array.name(), vec![builtin_types::ANY]);
                let unified_input = self.assert_type_unifies(&input, &array)?;
                self.assert_type_unifies(operand, &builtin_types::I64)?;
                if let Type::Nested { children, .. } = unified_input {
                    Ok(children[0].type_.clone())
                } else {
                    err("Bug: array should be a Type::nested at this point")
                }
            }
            Operator::Type => {
                if let Expression::Type(expected_type) = operand {
                    self.assert_type_unifies(input, expected_type)
                } else {
                    err(format!(
                        "Operator ':' can only be followed by a type, not {:?}",
                        operand
                    ))
                }
            }
            Operator::Assignment | Operator::Overwrite => self.get_type(input),
            Operator::Concatenate => {
                let array =
                    Type::from_nameless(BuiltinType::Array.name(), vec![builtin_types::ANY]);
                let unified_input = self.assert_type_unifies(input, &array)?;
                let unified_operand = self.assert_type_unifies(operand, &unified_input)?;
                return Ok(unified_operand);
            }
            Operator::Comparison(_) => unimplemented!(),
        }
    }

    fn get_call_type(
        &mut self,
        input: &Expression,
        operand: &Expression,
    ) -> Result<Type, AnyError> {
        match operand {
            Expression::Composed(Composed::Cast(cast)) => {
                self.is_castable_to(input, &cast.target_type)
            }
            Expression::Identifier(name) => {
                let callable_type = self.get_identifier_type(name)?;
                self.check_type_callable(input, operand, &callable_type)
            }
            Expression::Chain(chain) => {
                let callable_type = self.check_types_chain(chain)?;
                self.check_type_callable(input, operand, &callable_type)
            }
            Expression::Function(function) => {
                let callable_type = self.check_type_function(function)?;
                self.check_type_callable(input, operand, &callable_type)
            }
            Expression::Nothing
            | Expression::Value(_)
            | Expression::Type(_)
            | Expression::StaticList { .. } => err(format!(
                "Can not call this type of expression: {:?}",
                operand
            )),
            Expression::Composed(Composed::Loop(_)) => unimplemented!(),
            Expression::Composed(Composed::LoopOr(_)) => unimplemented!(),
            Expression::Composed(Composed::Times(_)) => unimplemented!(),
            Expression::Composed(Composed::TimesOr(_)) => unimplemented!(),
            Expression::Composed(Composed::Replace(_)) => unimplemented!(),
            Expression::Composed(Composed::Map(_)) => unimplemented!(),
            Expression::Composed(Composed::Branch(_)) => unimplemented!(),
            Expression::Composed(Composed::Something(_)) => unimplemented!(),
            Expression::Composed(Composed::Inspect(_)) => unimplemented!(),
        }
    }

    fn check_type_callable(
        &mut self,
        input: &Expression,
        callable: &Expression,
        callable_type: &Type,
    ) -> Result<Type, AnyError> {
        if let Type::Function {
            parameter,
            returned,
        } = callable_type
        {
            self.assert_type_unifies(input, &parameter.type_)?;
            Ok(returned.type_.clone())
        } else {
            let input_type = self.get_type(input)?;
            let expected_prototype = Type::function(
                TypedIdentifier::nameless(input_type),
                TypedIdentifier::unknown(),
            );
            err(type_mismatch(callable, &callable_type, &expected_prototype))
        }
    }

    fn is_castable_to(
        &mut self,
        input: &Expression,
        target_type: &TypedIdentifier,
    ) -> Result<Type, AnyError> {
        let input_type = self.get_type(input)?;
        let unified = cast(&input_type, &target_type.type_);
        if let Some(unified) = unified {
            Ok(unified)
        } else {
            err(type_mismatch(input, &input_type, &target_type.type_))
        }
    }

    fn assert_type_unifies(
        &mut self,
        actual_expression: &Expression,
        expected: &Type,
    ) -> Result<Type, AnyError> {
        let actual_type = self.get_type(actual_expression)?;
        let unified = unify(expected, &actual_type);
        if let Some(unified) = unified {
            Ok(unified)
        } else {
            err(type_mismatch(actual_expression, &actual_type, expected))
        }
    }
}

fn type_mismatch(actual_expression: &Expression, actual: &Type, expected: &Type) -> String {
    format!(
        "Type mismatch for expression '{:?}':\
        \n  actual:   {:?}\
        \n  expected: {:?}",
        actual_expression, actual, expected
    )
}

#[cfg(test)]
mod tests {
    use crate::common::unwrap_display;
    use std::collections::HashSet;

    use crate::evaluate::Runtime;
    use crate::frontend::{lex_and_parse, lex_and_parse_with_identifiers};

    use super::*;

    fn parse(code: &str) -> Program {
        lex_and_parse(code).unwrap()
    }

    fn assert_ok<T>(res: Result<T, AnyError>) {
        match res {
            Ok(_) => {}
            Err(e) => {
                panic!("{}", e);
            }
        }
    }
    fn assert_types_ok(code: &str) {
        let program = &parse(code);
        assert_ok(check_types(program))
    }
    fn assert_types_wrong(code: &str) {
        let program = &parse(code);
        check_types(program).expect_err("should fail");
    }

    #[test]
    fn test_basic_i64_type() {
        assert_types_ok("4 :i64");
    }

    #[test]
    fn test_basic_wrong_type() {
        assert_types_wrong("4 :function")
    }

    #[test]
    fn test_basic_function_type() {
        assert_types_ok("function{4} :function()(:i64)");
    }

    #[test]
    fn test_basic_operation() {
        assert_types_ok("4 +3 -1 |* 5 |/ 3 %2 :i64");
    }

    #[test]
    fn test_basic_array() {
        assert_types_ok("[1 2] :array(:i64)");
    }
    #[test]
    fn test_basic_tuple() {
        assert_types_ok("[1 function{}] :tuple(:i64 :function)");
    }
    #[test]
    fn test_tuples_and_arrays_not_mixed() {
        assert_types_wrong("[1 function{}] :array(:i64)");
        assert_types_wrong("[1 2] :tuple(:i64 :i64)");
    }

    #[test]
    fn test_empty_array() {
        let parsed = parse("[] :array(:i64)");
        let type_ = unwrap_display(get_type(&parsed));
        assert_eq!(
            type_,
            Type::from_nameless(BuiltinType::Array.name(), vec![builtin_types::I64])
        )
    }

    #[test]
    fn test_cast() {
        assert_types_ok("[] |cast(:array(:i64))");
    }

    #[test]
    fn test_chain_inside_array() {
        assert_types_ok("[{5 + 4}]");
    }

    #[test]
    fn test_identifier() {
        let func = lex_and_parse("function(x :i64) {x+1}").unwrap();
        let identifiers = HashMap::from([("increment".to_string(), func.main)]);
        let lib: HashSet<String> = identifiers.keys().cloned().collect();
        let mut main = lex_and_parse_with_identifiers("4 |increment", lib.clone()).unwrap();
        main.identifiers = identifiers.clone();

        assert_ok(check_types(&main));
        let read_input: &[u8] = &[];
        let print_output = Vec::<u8>::new();
        let result = Runtime::evaluate(main, read_input, print_output);
        assert_eq!(result.unwrap(), 5);

        let mut main = lex_and_parse_with_identifiers("increment", lib).unwrap();
        main.identifiers = identifiers;
        let type_ = Typer::new(&main).unwrap().get_type(&main.main).unwrap();
        let i64 = TypedIdentifier::nameless(builtin_types::I64);
        assert_eq!(type_, Type::function(i64.clone(), i64))
    }

    #[test]
    fn test_chain() {
        assert_types_ok("[1] ++[2] #0 :i64 =n +1 =>n");
    }
}
