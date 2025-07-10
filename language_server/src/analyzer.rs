use crate::request::PositionInCode;
use pipes_rs::common::{AnyError, err};
use pipes_rs::frontend::expression::{
    Chain, Composed, Expression, ExpressionSpan, Expressions, Function,
};
use pipes_rs::frontend::program::Program;
use pipes_rs::frontend::sources::location::{Location, Span};

pub fn find_expression_at(
    program: &Program,
    position_in_code: PositionInCode,
) -> Result<ExpressionSpan, AnyError> {
    let location = pos_to_location(position_in_code);
    if span_contains(program.main.span, location) {
        let mut highest_start_expr = &program.main;
        find_in_expr(&program.main, location, &mut highest_start_expr);
        Ok(highest_start_expr.clone())
    } else {
        err(format!(
            "code location {} is not contained in file {:?} which spans {}",
            location,
            program.sources.get_main().file.as_ref().unwrap(),
            program.main.span
        ))
    }
}

fn find_in_expr<'a>(
    expr_span: &'a ExpressionSpan,
    location: Location,
    mut highest_start_expr: &mut &'a ExpressionSpan,
) {
    match &expr_span.syntactic_type {
        Expression::Nothing
        | Expression::Value(_)
        | Expression::Identifier(_)
        | Expression::Type(_)
        | Expression::TypedIdentifiers(_)
        | Expression::Abstract(_) => {}
        Expression::Chain(chain) => {
            find_in_chain(chain, &expr_span, location, &mut highest_start_expr)
        }
        Expression::StaticList { elements } => {
            find_in_array(elements, location, &mut highest_start_expr);
        }
        Expression::Function(func) => {
            find_in_function(func, &expr_span, location, &mut highest_start_expr);
        }
        Expression::Composed(composed) => {
            find_in_composed(composed, &expr_span, location, &mut highest_start_expr);
        }
    }
}

fn pos_to_location(p: PositionInCode) -> Location {
    let PositionInCode {
        absolute_char,
        line,
        char_in_line,
    } = p;
    let absolute_char = if absolute_char > 0 {
        absolute_char as usize
    } else {
        0
    };
    let location = Location::from(line, char_in_line, absolute_char);
    location
}

fn span_contains(span: Span, location: Location) -> bool {
    let inside_start = span.start.line() < location.line()
        || span.start.line() == location.line() && span.start.column() <= location.column();
    let inside_end = span.end.line() > location.line()
        || span.end.line() == location.line() && span.end.column() >= location.column();
    inside_start && inside_end
}

fn find_in_chain<'a>(
    chain: &'a Chain,
    _chain_expr: &'a ExpressionSpan,
    location: Location,
    mut highest_start_expr: &mut &'a ExpressionSpan,
) {
    for operation in &chain.operations {
        if span_contains(operation.content_span(), location) {
            // if span_contains(operation.operator.span, location) {
            // *highest_start_expr = chain_expr; // TODO: would need an OperationSpan here and make Operation an Expression?
            // } else {
            for operand in &operation.operands {
                if span_contains(operand.span, location) {
                    *highest_start_expr = operand;
                    find_in_expr(&operand, location, &mut highest_start_expr);
                    break;
                }
            }
            // }
        }
    }
}

fn find_in_array<'a>(
    elements: &'a Expressions,
    location: Location,
    mut highest_start_expr: &mut &'a ExpressionSpan,
) {
    for element in elements {
        if span_contains(element.span, location) {
            *highest_start_expr = element;
            find_in_expr(element, location, &mut highest_start_expr);
            break;
        }
    }
}

fn find_in_function<'a>(
    function: &'a Function,
    func_expr: &'a ExpressionSpan,
    location: Location,
    mut highest_start_expr: &mut &'a ExpressionSpan,
) {
    if span_contains(function.body.content_span(), location) {
        find_in_chain(&function.body, func_expr, location, &mut highest_start_expr);
    }
}

fn find_in_composed<'a>(
    composed: &'a Composed,
    composed_expr: &'a ExpressionSpan,
    location: Location,
    mut highest_start_expr: &mut &'a ExpressionSpan,
) {
    for chain in composed.chains() {
        find_in_chain(chain, composed_expr, location, &mut highest_start_expr);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pipes_rs::common::unwrap_display;
    use pipes_rs::frontend::sources::location::SourceCode;
    use pipes_rs::frontend::{lex_and_parse, parse_type};
    use pipes_rs::middleend::typing::put_types;

    fn build_program<S: Into<SourceCode>>(code_text: S) -> Program {
        let mut program = unwrap_display(lex_and_parse(code_text));
        unwrap_display(put_types(&mut program));
        program
    }

    #[test]
    fn test_get_symbol() {
        let program = build_program("function(:i64)(:i64) { 3 } | function(my_func) {5 |my_func}");
        let expression = unwrap_display(find_expression_at(
            &program,
            PositionInCode {
                absolute_char: -1,
                line: 1,
                char_in_line: 41,
            },
        ));
        // let nameless_int = TypedIdentifier::nameless(builtin_types::I64);
        // let location = Location::from(1, 23, 23);
        // let called_function = Expression::Function(Function {
        //     parameters: vec![nameless_int],
        //     returned: nameless_int,
        //     body: Chain {
        //         operations: vec![Operation {
        //             operator: OperatorSpan::new(Operator::Ignore, Span { start: location, end: location }),
        //             operands: vec![],
        //             sem_type: nameless_int.type_,
        //         }]
        //     },
        //
        // });
        let Expression::Function(..) = expression.syn_type() else {
            panic!("expression was not a function: {}", expression.syn_type());
        };
        assert_eq!(
            *expression.sem_type(),
            parse_type("function(my_func :function(:i64)(:i64))(:i64)"),
        );
    }

    #[test]
    fn test_identifier() {
        let program = build_program("function(:i64)(:i64) { 3 } | function(my_func) {5 |my_func}");
        let expression = unwrap_display(find_expression_at(
            &program,
            PositionInCode {
                absolute_char: -1,
                line: 1,
                char_in_line: 53,
            },
        ));

        assert_eq!(
            *expression.syn_type(),
            Expression::Identifier("my_func".to_string())
        );
        assert_eq!(*expression.sem_type(), parse_type("function(:i64)(:i64)"),);
    }
}
