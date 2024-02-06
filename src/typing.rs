use crate::common::{err, AnyError};
use crate::frontend::expression::{
    Chain, Expression, Function, Transformation, Type, TypedIdentifier,
};
use crate::frontend::lexer::Operator;
use crate::frontend::program::Program;

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

pub mod builtin_types {
    use crate::frontend::expression::Type;

    pub const NOTHING: Type = Type::Builtin {
        type_name: "Nothing",
    };
    #[allow(unused)]
    pub const I64: Type = Type::Builtin { type_name: "i64" };
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
        Expression::StaticList { .. } => {
            unimplemented!()
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
        Operator::Call => {}
        Operator::Get => {}
        Operator::Type => {}
        Operator::Assignment => {}
        Operator::Overwrite => {}
        Operator::Concatenate => {}
        Operator::Comparison(_) => {}
    }
    unimplemented!()
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

    #[test]
    fn test_basic_i64_type() {
        let program = &parse("4 :i64");
        assert_ok(check_types(program))
    }

    #[test]
    fn test_basic_wrong_type() {
        let program = &parse("4 :function");
        check_types(program).expect_err("should fail");
    }

    #[test]
    fn test_basic_function_type() {
        let program = &parse("function{4} :function()(:i64)");
        assert_ok(check_types(program))
    }

    #[test]
    fn test_basic_operation() {
        let program = &parse("4 +3 -1 |* 5 |/ 3 %2 :i64");
        assert_ok(check_types(program))
    }

    #[test]
    fn test_basic_array() {
        let program = &parse("[ 1 2 ] :array(num :i64)");
        assert_ok(check_types(program))
    }
}
