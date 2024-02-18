

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::AnyError;
    use crate::frontend::expression::{Expression, ExpressionSpan};
    use crate::frontend::program::Program;

    fn lex_and_parse(code: &str) -> Result<Program, AnyError> {
        Ok(Program::new_raw(Expression::Nothing))
    }
    #[test]
    fn test_basic() {
        let program = lex_and_parse("").unwrap();
        assert_eq!(
            program,
            Program::new(ExpressionSpan::new_spanless(Expression::Nothing))
        )
    }
}
