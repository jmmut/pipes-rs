use crate::request::PositionInCode;
use pipes_rs::common::{AnyError, err};
use pipes_rs::frontend::expression::ExpressionSpan;
use pipes_rs::frontend::program::Program;

pub fn find_expression_at(
    program: &Program,
    position_in_code: PositionInCode,
) -> Result<ExpressionSpan, AnyError> {
    err("")
}
