use crate::backend::Runtime;
use crate::common::{context, AnyError};
use crate::frontend::expression::{Chain, Composed, Comptime, Expression, ExpressionSpan};
use crate::frontend::program::{Identifiers, Program};
use crate::frontend::sources::Sources;
use crate::middleend::intrinsics::builtin_types;

pub fn rewrite(program: Program) -> Result<Program, AnyError> {
    let (main, identifiers, sources) = program.take();
    let rewriter = Rewriter {
        identifiers,
        sources,
    };
    context("Comptime evaluation", rewriter.rewrite_contextless(main))
}

struct Rewriter {
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
                let composed_expr_span = ExpressionSpan::new(
                    Expression::Chain(taken.body),
                    expression_span.sem_type().clone(),
                    expression_span.span,
                );
                let identifiers = std::mem::take(&mut self.identifiers);
                let sources = std::mem::take(&mut self.sources);
                let value = Runtime::evaluate(
                    Program::new_from(composed_expr_span, identifiers, sources),
                    std::io::stdin(),
                    std::io::stdout(),
                )?;
                let new_expression = Expression::Value(value);
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

    fn parse(code: &str) -> Program {
        unwrap_display(lex_and_parse(code))
    }

    #[test]
    fn test_basic() {
        let program = parse("comptime { 5 +2 }");
        let rewritten = unwrap_display(rewrite(program));
        assert_eq!(rewritten.main.syntactic_type, Expression::Value(7));
    }
    #[test]
    fn test_basic_nested() {
        let program = parse("{;comptime { 5 +2 }}");
        let rewritten = unwrap_display(rewrite(program));
        assert_exprs_eq(rewritten.main.syntactic_type, chain_init(val(7), &[]));
    }
}
