use crate::common::AnyError;
use crate::frontend::program::Program;

pub fn check_types(program: Program) -> Result<(), AnyError> {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::lex_and_parse;

    #[test]
    fn test_basic_type() {
        let program = lex_and_parse("4 :i64").unwrap();
        check_types(program).unwrap()
    }

    #[test]
    fn test_basic_wrong_type() {
        let program = lex_and_parse("4 :function").unwrap();
        check_types(program).expect_err("should fail");
    }
}
