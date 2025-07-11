use crate::backend::Runtime;
use crate::common::{bug, context, err, err_span, AnyError};
use crate::frontend::expression::{
    is_macro, Abstract, Chain, Composed, Comptime, Expression, ExpressionSpan, Function,
    TypedIdentifier, TypedIdentifiers,
};
use crate::frontend::parser::reverse_iterative_parser::{err_expected_span, expected_span};
use crate::frontend::program::{Identifiers, Program};
use crate::frontend::sources::location::{Span, NO_SPAN};
use crate::frontend::sources::token::{Keyword, Operator};
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
        self.delete_macros(&mut main);
        Ok(Program::new_from(main, self.identifiers, self.sources))
    }

    fn delete_macros(&mut self, main: &mut ExpressionSpan) {
        let mut to_delete = Vec::new();
        for (identifier, expr) in &self.identifiers {
            if is_macro(expr) {
                to_delete.push(identifier.clone());
            }
        }

        for to_delete in to_delete {
            self.identifiers.remove(&to_delete);

            replace(
                &TypedIdentifier::any(to_delete.clone()),
                &ExpressionSpan::new_typeless(Expression::Nothing, NO_SPAN),
                main,
            )
        }
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
                let value = self.runtime.evaluate_recursive(&composed_expr_span)?; // TODO: take identifiers that might have been created
                let new_expression = Expression::Value(value); // TODO: this might be a function pointer or other things
                let new_expr_span =
                    ExpressionSpan::new(new_expression, builtin_types::I64, expression_span.span);
                *expression_span = new_expr_span;
            }
            Expression::Composed(composed) => {
                for chain in composed.chains_mut() {
                    self.rewrite_chain(chain)?;
                }
            }
            Expression::TypedIdentifiers(_) => unimplemented!(),
            Expression::Abstract(_) => unimplemented!(),
        }
        Ok(())
    }

    fn rewrite_chain(&mut self, chain: &mut Chain) -> Result<(), AnyError> {
        for operation in &mut chain.operations {
            if operation.operator.operator == Operator::MacroCall {
                if let Some(ExpressionSpan {
                    syntactic_type: Expression::Identifier(macro_name),
                    span,
                    ..
                }) = operation.operands.first()
                {
                    if let Some(macro_expr) = self.identifiers.get(macro_name) {
                        if let ExpressionSpan {
                            syntactic_type: Expression::Function(macro_func),
                            span,
                            ..
                        } = macro_expr
                        {
                            let macro_operands = std::mem::take(&mut operation.operands);
                            let mut body = macro_func.body.clone();

                            for i in 1..macro_func.parameters.len() {
                                replace_in_chain(
                                    &macro_func.parameters[i],
                                    &macro_operands[i],
                                    &mut body,
                                );
                            }
                            self.remove_abstracts_in_chain(&mut body)?;
                            operation.operator.operator = Operator::Call;
                            let params = vec![macro_func.parameters[0].clone()];
                            let returned = TypedIdentifier::nameless_any();
                            let function = Function::new(params, returned, body);
                            let func_expr =
                                ExpressionSpan::new_typeless(Expression::Function(function), *span);
                            operation.operands = vec![func_expr];
                        } else {
                            return bug(expected_span(
                                "macro function-like definition",
                                Some(macro_expr),
                                self.sources.get_main(),
                                operation.operator.span,
                            ));
                        }
                    } else {
                        return err_span(
                            format!("Bug: Could not find macro definition '{}'", macro_name),
                            self.sources.get_main(),
                            *span,
                        );
                    }
                } else {
                    return bug(expected_span(
                        "macro identifier",
                        operation.operands.first(),
                        self.sources.get_main(),
                        operation.operator.span,
                    ));
                }
            } else {
                if operation.operator.operator == Operator::Call {
                    if let Some(ExpressionSpan {
                        syntactic_type: Expression::Identifier(callable_name),
                        ..
                    }) = operation.operands.first()
                    {
                        let func = self.identifiers.get(callable_name);
                        if let Some(ExpressionSpan {
                            syntactic_type: Expression::Function(function),
                            ..
                        }) = func
                        {
                            if function.is_macro {
                                return err_span(
                                    format!("MacroCall '{}' should be used when calling macro '{}' (instead of regular Calls '{}')", Operator::MacroCall, callable_name, Operator::Call),
                                    self.sources.get_main(),
                                    operation.operator.span,
                                );
                            }
                        }
                    }
                }
                for operand in &mut operation.operands {
                    self.rewrite_expression_span(operand)?
                }
            }
        }
        Ok(())
    }

    fn remove_abstracts(&self, expr: &mut ExpressionSpan) -> Result<(), AnyError> {
        match expr.syn_type_mut() {
            Expression::Nothing => {}
            Expression::Value(_) => {}
            Expression::Identifier(_) => {}
            Expression::Type(_) => {}
            Expression::TypedIdentifiers(_) => {}
            Expression::Chain(chain) => self.remove_abstracts_in_chain(chain)?,
            Expression::StaticList { elements } => {
                for elem in elements {
                    self.remove_abstracts(elem)?
                }
            }
            Expression::Function(function) => self.remove_abstracts_in_chain(&mut function.body)?,
            Expression::Composed(composed) => {
                for chain in composed.chains_mut() {
                    self.remove_abstracts_in_chain(chain)?
                }
            }
            Expression::Abstract(abstract_) => {
                *expr = self.remove_abstract_in_abstract(abstract_)?;
            }
        }
        Ok(())
    }

    fn remove_abstract_in_abstract(
        &self,
        abstract_: &mut Abstract,
    ) -> Result<ExpressionSpan, AnyError> {
        match abstract_ {
            Abstract::Abstract {
                name,
                span: abstract_span,
                nodes,
            } => {
                if name == Keyword::Function.name() {
                    let (body, _body_span) = self.get_chain(abstract_span, nodes.pop())?;
                    let any = TypedIdentifier::nameless_any();
                    let (params, returned) = if let Some(node) = nodes.pop() {
                        let (params, _params_span) = self.get_tis(abstract_span, node)?;
                        let returned = if let Some(node) = nodes.pop() {
                            let (tis, _returned_span) = self.get_tis(abstract_span, node)?;
                            tis.into_iter()
                                .next()
                                .unwrap_or(TypedIdentifier::nameless_any())
                        } else {
                            any.clone()
                        };
                        (params, returned)
                    } else {
                        (vec![any.clone()], any)
                    };
                    Ok(ExpressionSpan::new_typeless(
                        Expression::Function(Function::new(params, returned, body)),
                        *abstract_span,
                    ))
                } else {
                    todo!()
                }
            }
            Abstract::Expression(inner) => Ok(*inner.clone()),
        }
    }

    fn get_chain(
        &self,
        abstract_span: &mut Span,
        node: Option<Abstract>,
    ) -> Result<(Chain, Span), AnyError> {
        if let Some(Abstract::Expression(boxed_expr)) = node {
            if let ExpressionSpan {
                syntactic_type: Expression::Chain(chain),
                span,
                ..
            } = *boxed_expr
            {
                Ok((chain, span))
            } else {
                err_expected_span(
                    "abstract chain",
                    boxed_expr,
                    self.sources.get_main(),
                    *abstract_span,
                )
            }
        } else {
            err(expected_span(
                "abstract chain as last Function node",
                node,
                self.sources.get_main(),
                *abstract_span,
            ))
        }
    }
    fn get_tis(
        &self,
        abstract_span: &mut Span,
        node: Abstract,
    ) -> Result<(TypedIdentifiers, Span), AnyError> {
        if let Abstract::Expression(boxed_expr) = node {
            if let ExpressionSpan {
                syntactic_type: Expression::TypedIdentifiers(tis),
                span,
                ..
            } = *boxed_expr
            {
                Ok((tis, span))
            } else {
                err_expected_span(
                    "abstract types",
                    boxed_expr,
                    self.sources.get_main(),
                    *abstract_span,
                )
            }
        } else {
            err_expected_span(
                "abstract types",
                node,
                self.sources.get_main(),
                *abstract_span,
            )
        }
    }

    fn remove_abstracts_in_chain(&self, chain: &mut Chain) -> Result<(), AnyError> {
        for operation in &mut chain.operations {
            for operand in &mut operation.operands {
                self.remove_abstracts(operand)?
            }
        }
        Ok(())
    }
}

fn replace_in_chain(param: &TypedIdentifier, value: &ExpressionSpan, chain: &mut Chain) {
    // println!("replacing {} with {}", param, value);
    for operation in &mut chain.operations {
        for operand in &mut operation.operands {
            replace(param, value, operand);
        }
    }
}

fn replace(param: &TypedIdentifier, value: &ExpressionSpan, operand: &mut ExpressionSpan) {
    match operand.syn_type_mut() {
        Expression::Nothing => {}
        Expression::Value(_) => {}
        Expression::Identifier(name) => {
            if *name == param.name {
                *operand = value.clone();
            }
        }
        Expression::Type(_) => {}
        Expression::TypedIdentifiers(_) => {}
        Expression::Chain(chain) => {
            replace_in_chain(param, value, chain);
        }
        Expression::StaticList { elements } => {
            for elem in elements {
                replace(param, value, elem);
            }
        }
        Expression::Function(function) => replace_in_chain(param, value, &mut function.body),
        Expression::Composed(composed) => {
            for body in composed.chains_mut() {
                replace_in_chain(param, value, body);
            }
        }
        Expression::Abstract(abstract_) => {
            replace_in_abstract(param, value, abstract_);
        }
    }
}

fn replace_in_abstract(param: &TypedIdentifier, value: &ExpressionSpan, abstract_: &mut Abstract) {
    match abstract_ {
        Abstract::Abstract { nodes, .. } => {
            for node in nodes {
                replace_in_abstract(param, value, node);
            }
        }
        Abstract::Expression(expr) => replace(param, value, expr),
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
        let program = parse(code);
        let mut rewritten = unwrap_display(rewrite(program));
        unwrap_display(put_types(&mut rewritten));
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
