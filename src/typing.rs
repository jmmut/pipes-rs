use crate::common::{err, AnyError};
use crate::frontend::expression::{Chain, Expression, Transformation, Type};
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

fn check_types_chain(chain: &Chain) -> Result<(), AnyError> {
    let accumulated = &chain.initial;
    let mut accumulated_type = get_type(accumulated.as_ref());
    for t in &chain.transformations {
        if let Transformation {
            operator: Operator::Type,
            operand: Expression::Type(expected_type),
        } = t
        {
            if accumulated_type != *expected_type {
                return err(format!(
                    "Type mismatch:\
                \n  actual:   {:?}\
                \n  expected: {:?}",
                    accumulated_type, expected_type
                ));
            }
        } else {
            accumulated_type = get_operation_type(accumulated, t.operator, &t.operand);
        }
    }
    Ok(())
}

fn get_type(expression: &Expression) -> Type {
    match expression {
        Expression::Nothing => builtin_types::NOTHING,
        Expression::Value(_) => builtin_types::I64,
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
        Expression::Function(_) => {
            unimplemented!()
        }
        Expression::Composed(_) => {
            unimplemented!()
        }
    }
}

fn get_operation_type(input: &Expression, operator: Operator, operand: &Expression) -> Type {
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
        let program = &parse("function{4} :function");
        assert_ok(check_types(program))
    }
}
