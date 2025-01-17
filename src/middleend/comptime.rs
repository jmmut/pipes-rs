use crate::backend::Runtime;
use crate::common::{context, AnyError};
use crate::frontend::expression::{Chain, Composed, Comptime, Expression, ExpressionSpan};
use crate::frontend::program::{Identifiers, Program};
use crate::frontend::sources::Sources;
use crate::middleend::intrinsics::builtin_types;
use std::io::{stdin, stdout, Stdin, Stdout};

pub fn rewrite(program: Program) -> Result<Program, AnyError> {
    let (main, identifiers, sources) = program.take();
    let runtime = context(
        "Comptime evaluation setup",
        Runtime::new(stdin(), stdout(), identifiers.clone(), sources.clone()),
    )?;
    let rewriter = Rewriter {
        runtime,
        identifiers,
        sources,
    };
    context("Comptime evaluation", rewriter.rewrite_contextless(main))
}

struct Rewriter {
    runtime: Runtime<Stdin, Stdout>,
    pub identifiers: Identifiers,
    pub sources: Sources,
}
impl Rewriter {
    fn rewrite_contextless(mut self, mut main: ExpressionSpan) -> Result<Program, AnyError> {
        self.rewrite_expression_span(&mut main)?;
        Ok(Program::new_from(main, self.identifiers, self.sources))
    }

    fn rewrite_expression_span(
        &mut self,
        expression_span: &mut ExpressionSpan,
    ) -> Result<(), AnyError> {
        match expression_span.syn_type_mut() {
            Expression::Nothing => {}
            Expression::Value(_) => {}
            Expression::Identifier(_) => {}
            Expression::Type(_) => {}
            Expression::Chain(chain) => self.rewrite_chain(chain)?,
            Expression::StaticList { .. } => {}
            Expression::Function(_) => {}
            Expression::Composed(Composed::Comptime(comptime)) => {
                let mut taken = Comptime {
                    body: Chain::empty(),
                };
                std::mem::swap(&mut taken, comptime);
                let mut composed_expr_span = ExpressionSpan::new(
                    Expression::Chain(taken.body),
                    expression_span.sem_type().clone(),
                    expression_span.span,
                );
                self.rewrite_expression_span(&mut composed_expr_span)?;
                let value = self.runtime.evaluate_recursive(&composed_expr_span)?;
                let new_expression = Expression::Value(value); // TODO: this might be a function pointer or other things
                let new_expr_span =
                    ExpressionSpan::new(new_expression, builtin_types::I64, expression_span.span);
                *expression_span = new_expr_span;
            }
            Expression::Composed(_) => {}
        }
        Ok(())
    }

    fn rewrite_chain(&mut self, chain: &mut Chain) -> Result<(), AnyError> {
        for operation in &mut chain.operations {
            for operand in &mut operation.operands {
                self.rewrite_expression_span(operand)?
            }
        }
        Ok(())
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::unwrap_display;
    use crate::frontend::expression::Expression;
    use crate::frontend::lex_and_parse;
    use crate::frontend::program::Program;
    use crate::frontend::tests::{assert_exprs_eq, chain_init, val};
    use crate::middleend::typing::{add_types, put_types};

    fn parse(code: &str) -> Program {
        unwrap_display(lex_and_parse(code))
    }

    fn test_rewrite(code: &str) -> Program {
        let mut program = parse(code);
        unwrap_display(put_types(&mut program));
        let rewritten = unwrap_display(rewrite(program));
        rewritten
    }
    fn typed(expression: Expression) -> Expression {
        let program = Program::new_raw(expression);
        unwrap_display(add_types(&program)).main.syntactic_type
    }

    #[test]
    fn test_basic() {
        let program = test_rewrite("comptime { 5 +2 }");
        assert_eq!(program.main.syntactic_type, Expression::Value(7));
    }
    #[test]
    fn test_basic_nested() {
        let program = test_rewrite("{;comptime { 5 +2 }}");
        assert_exprs_eq(program.main.syntactic_type, typed(chain_init(val(7), &[])));
    }
    #[test]
    fn test_nested_comptime() {
        let program = test_rewrite("{;comptime {5 +{1 +comptime {2 +4}}}}");
        assert_exprs_eq(program.main.syntactic_type, typed(chain_init(val(12), &[])));
    }
}
