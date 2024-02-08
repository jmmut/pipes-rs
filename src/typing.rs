mod unify;

use crate::common::{err, AnyError};
use crate::frontend::expression::{
    Chain, Composed, Expression, Function, Transformation, Type, TypedIdentifier,
};
use crate::frontend::lexer::Operator;
use crate::frontend::program::Program;
use crate::typing::unify::{all_same_type, unify};

pub fn check_types(program: &Program) -> Result<(), AnyError> {
    match &program.main {
        Expression::Nothing => {}
        Expression::Value(_) => {}
        Expression::Identifier(_) => {}
        Expression::Type(_) => {}
        Expression::Chain(chain) => {
            check_types_chain(chain)?;
        }
        Expression::StaticList { .. } => {
            unimplemented!()
        }
        Expression::Function(_) => {
            unimplemented!()
        }
        Expression::Composed(_) => {
            unimplemented!()
        }
    }
    Ok(())
}

pub mod type_names {
    #[allow(unused)]
    pub const UNKNOWN: &'static str = "unknown";
    #[allow(unused)]
    pub const I64: &'static str = "i64";
    #[allow(unused)]
    pub const TUPLE: &'static str = "tuple";
    #[allow(unused)]
    pub const ARRAY: &'static str = "array";
    #[allow(unused)]
    pub const STRUCT: &'static str = "struct";
    pub const FUNCTION: &'static str = "function";
    #[allow(unused)]
    pub const TYPE: &'static str = "type";
}

pub fn is_builtin_nested_type(name: &str) -> Option<&'static str> {
    if name == type_names::ARRAY {
        Some(type_names::ARRAY)
    } else if name == type_names::STRUCT {
        Some(type_names::STRUCT)
    } else if name == type_names::TUPLE {
        Some(type_names::TUPLE)
    } else {
        None
    }
}

pub fn is_builtin_simple_type(name: &str) -> Option<Type> {
    if name == type_names::I64 {
        Some(builtin_types::I64)
    } else {
        None
    }
}

pub mod builtin_types {
    use crate::frontend::expression::Type;
    use crate::typing::type_names;

    pub const NOTHING: Type = Type::Builtin {
        type_name: "nothing",
    };
    #[allow(unused)]
    pub const I64: Type = Type::Builtin {
        type_name: type_names::I64,
    };
}

fn type_mismatch<T>(
    actual_expression: &Expression,
    actual: &Type,
    expected: &Type,
) -> Result<T, AnyError> {
    err(format!(
        "Type mismatch for expression '{:?}':\
        \n  actual:   {:?}\
        \n  expected: {:?}",
        actual_expression, actual, expected
    ))
}

fn assert_same_type(actual_expression: &Expression, expected: &Type) -> Result<Type, AnyError> {
    let actual_type = get_type(actual_expression)?;
    if actual_type != *expected {
        type_mismatch(actual_expression, &actual_type, expected)
    } else {
        Ok(actual_type)
    }
}

fn check_types_chain(chain: &Chain) -> Result<Type, AnyError> {
    let accumulated = &chain.initial;
    let mut accumulated_type = get_type(accumulated.as_ref())?;
    for t in &chain.transformations {
        if let Transformation {
            operator: Operator::Type,
            operand: Expression::Type(expected_type),
        } = t
        {
            if accumulated_type != *expected_type {
                return type_mismatch(accumulated, &accumulated_type, expected_type);
            }
        } else {
            accumulated_type = get_operation_type(accumulated, t.operator, &t.operand)?;
        }
    }
    Ok(accumulated_type)
}

fn get_type(expression: &Expression) -> Result<Type, AnyError> {
    match expression {
        Expression::Nothing => Ok(builtin_types::NOTHING),
        Expression::Value(_) => Ok(builtin_types::I64),
        Expression::Identifier(_) => {
            unimplemented!()
        }
        Expression::Type(_) => {
            unimplemented!()
        }
        Expression::Chain(_) => {
            unimplemented!()
        }
        Expression::StaticList { elements } => {
            let mut types = Vec::new();
            for e in elements {
                types.push(get_type(e)?);
            }
            return if types.len() == 0 {
                Ok(Type::BuiltinSingle {
                    type_name: type_names::ARRAY,
                    child: Box::new(TypedIdentifier::nameless(Type::Unknown)),
                })
            } else if all_same_type(&types) {
                Ok(Type::BuiltinSingle {
                    type_name: type_names::ARRAY,
                    child: Box::new(TypedIdentifier::nameless(types.pop().unwrap())),
                })
            } else {
                Ok(Type::BuiltinSeveral {
                    type_name: type_names::TUPLE,
                    children: types
                        .into_iter()
                        .map(|t| TypedIdentifier::nameless(t))
                        .collect(),
                })
            };
        }
        Expression::Function(Function { parameter, body }) => {
            let chain_type = check_types_chain(body);
            let children = vec![parameter.clone(), TypedIdentifier::nameless(chain_type?)];
            Ok(Type::BuiltinSeveral {
                type_name: type_names::FUNCTION,
                children,
            })
        }
        Expression::Composed(_) => {
            unimplemented!()
        }
    }
}

fn get_operation_type(
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
            assert_same_type(input, &builtin_types::I64)?;
            assert_same_type(operand, &builtin_types::I64)?;
            return Ok(builtin_types::I64);
        }
        Operator::Ignore => return get_type(operand),
        Operator::Call => {
            return get_call_type(input, operand);
        }
        Operator::Get => {}
        Operator::Type => {}
        Operator::Assignment => {}
        Operator::Overwrite => {}
        Operator::Concatenate => {}
        Operator::Comparison(_) => {}
    }
    unimplemented!()
}

fn get_call_type(input: &Expression, operand: &Expression) -> Result<Type, AnyError> {
    match operand {
        Expression::Composed(Composed::Cast(cast)) => {
            return is_castable_to(input, &cast.target_type);
        }
        _ => unimplemented!(),
    }
}

fn is_castable_to(input: &Expression, target_type: &TypedIdentifier) -> Result<Type, AnyError> {
    let input_type = get_type(input)?;
    unify(&input_type, &target_type.type_)
        .ok_or_else(|| type_mismatch::<Type>(input, &input_type, &target_type.type_).unwrap_err())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::lex_and_parse;

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
        assert_types_wrong("[] :array(:i64)");
    }

    #[test]
    fn test_cast() {
        assert_types_ok("[] |cast(:array(:i64))");
    }
}
