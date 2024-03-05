use std::collections::HashMap;

use strum::IntoEnumIterator;

use crate::common::{context, err, err_span, AnyError};
use crate::frontend::expression::display::typed_identifiers_to_str;
use crate::frontend::expression::{
    Chain, Composed, Expression, ExpressionSpan, Expressions, Function, Loop, Map, Operation,
    Replace, Type, TypedIdentifier, TypedIdentifiers,
};
use crate::frontend::location::{SourceCode, Span, NO_SPAN};
use crate::frontend::parse_type;
use crate::frontend::program::Program;
use crate::frontend::token::{Operator, OperatorSpan};
use crate::middleend::intrinsics::{builtin_types, is_builtin_type, BuiltinType, Intrinsic};
use crate::middleend::typing::cast::cast;
use crate::middleend::typing::unify::{
    all_same_type, unify, unify_typed_identifier, unify_typed_identifiers,
};

pub mod cast;
pub mod unify;

pub fn check_types(program: &Program) -> Result<(), AnyError> {
    context("Type checking", get_type(program).map(|_| ()))
}

pub fn get_type(program: &Program) -> Result<Type, AnyError> {
    let mut typer = Typer::new(program)?;
    typer.get_type(&typer.program.main().syn_type())
}

pub fn is_builtin_nested_type(name: &str) -> Option<&'static str> {
    if name == BuiltinType::Array.name() {
        Some(BuiltinType::Array.name())
    } else if name == BuiltinType::Struct.name() {
        Some(BuiltinType::Struct.name())
    } else if name == BuiltinType::Tuple.name() {
        Some(BuiltinType::Tuple.name())
    } else {
        None
    }
}

pub fn is_builtin_simple_type(name: &str) -> Option<Type> {
    if name == BuiltinType::I64.name() {
        Some(builtin_types::I64)
    } else {
        None
    }
}

type BindingsTypesStack = Vec<Type>;
struct Typer<'a> {
    program: &'a Program,
    identifier_types: HashMap<String, BindingsTypesStack>,
    current_source: Option<String>,
}

impl<'a> Typer<'a> {
    pub fn new(program: &'a Program) -> Result<Self, AnyError> {
        let identifier_types = Self::build_intrinsics();
        let mut typer = Self {
            program,
            identifier_types,
            current_source: None,
        };
        typer.setup_constants()?;
        Ok(typer)
    }

    fn build_intrinsics() -> HashMap<String, BindingsTypesStack> {
        let mut identifiers = HashMap::new();
        for intrinsic in Intrinsic::iter() {
            identifiers.insert(intrinsic.name().to_string(), vec![intrinsic.type_()]);
        }
        identifiers
    }

    fn setup_constants(&mut self) -> Result<(), AnyError> {
        let mut identifiers_vec = self.program.identifiers.keys().collect::<Vec<_>>();
        loop {
            let mut failed = Vec::<&String>::new();
            let identifiers_count_previous = identifiers_vec.len();
            let mut errors = Vec::new();
            for name in identifiers_vec {
                self.current_source = Some(Self::identifier_to_source(name));
                match context(
                    "Runtime setup",
                    self.get_type(self.program.identifiers.get(name).unwrap().syn_type()),
                ) {
                    Ok(type_) => self.bind_identifier_type(name.clone(), type_),
                    Err(e) => {
                        errors.push(e);
                        failed.push(name);
                    }
                }
            }
            if failed.len() == 0 {
                self.current_source = None;
                return Ok(());
            }
            if failed.len() == identifiers_count_previous {
                let error_intro = if failed.len() == 1 {
                    "A constant has an incorrect type"
                } else {
                    "Some constants have incorrect types. (Are there cyclic dependencies?)"
                };
                let error_messages = errors
                    .iter()
                    .map(|e| e.to_string())
                    .reduce(|accum, e| accum + "\n" + &e)
                    .unwrap();
                return err(format!("{}: {}", error_intro, error_messages));
            }
            identifiers_vec = failed;
        }
    }

    fn identifier_to_source(name: &str) -> String {
        let mut parts = name.split("/").collect::<Vec<_>>();
        parts.pop();
        let filename = parts.join("/") + ".pipes";
        filename
    }

    fn bind_typed_identifier(&mut self, TypedIdentifier { name, type_ }: TypedIdentifier) {
        self.bind_identifier_type(name, type_);
    }
    fn bind_identifier_type(&mut self, identifier: String, type_: Type) {
        self.identifier_types
            .entry(identifier)
            .or_insert(Vec::new())
            .push(type_);
    }

    fn unbind_identifier(&mut self, identifier: &String, times: usize) -> Result<(), AnyError> {
        if let Some(stack) = self.identifier_types.get_mut(identifier) {
            if times <= stack.len() {
                stack.truncate(stack.len() - times);
                Ok(())
            } else {
                err(format!(
                    "Bug: tried to unbind identifier {} {} times, but it's shadowed only {} times",
                    identifier,
                    times,
                    stack.len()
                ))
            }
        } else {
            err(format!(
                "Bug: tried to unbind non-existing identifier {}",
                identifier
            ))
        }
    }

    fn get_identifier_type(&self, name: &String) -> Result<Type, AnyError> {
        if let Some(types) = self.identifier_types.get(name) {
            if let Some(last) = types.last() {
                Ok(last.clone())
            } else {
                err(format!(
                    "Bug: Identifier '{}' is not bound to any value",
                    name
                ))
            }
        } else {
            err(format!(
                "Bug: Undefined identifier '{}'. This should have been detected by earlier stages.",
                name
            ))
        }
    }

    pub fn get_type(&mut self, expression: &Expression) -> Result<Type, AnyError> {
        match expression {
            Expression::Nothing => Ok(builtin_types::NOTHING),
            Expression::Value(_) => Ok(builtin_types::I64),
            Expression::Identifier(name) => self.check_types_identifier(name),
            Expression::Type(_) => Ok(builtin_types::TYPE),
            Expression::Chain(chain) => self.check_types_chain(chain),
            Expression::StaticList { elements } => self.check_types_list(elements),
            Expression::Function(function) => self.check_type_function(function),
            Expression::Composed(_) => {
                unimplemented!()
            }
        }
    }
    fn check_types_identifier(&mut self, name: &String) -> Result<Type, AnyError> {
        self.get_identifier_type(name)
    }

    fn check_types_chain(&mut self, chain: &Chain) -> Result<Type, AnyError> {
        let mut last_operand_span = None;
        let mut accumulated_type = if let Some(initial) = &chain.initial {
            last_operand_span = Some(initial.span);
            self.get_type(initial.syn_type())?
        } else {
            builtin_types::ANY
        };

        let mut assigned_in_this_chain = HashMap::new();
        for operation in &chain.operations {
            last_operand_span = operation.operands.last().map(|o| o.span);
            accumulated_type = self.get_operation_type(
                &accumulated_type,
                operation.operator,
                &operation.operands,
            )?;
            if let Operation {
                operator:
                    OperatorSpan {
                        operator: Operator::Assignment,
                        span: op_span,
                    },
                operands,
            } = operation
            {
                let operand = operands.first();
                if let Some(ExpressionSpan {
                    syntactic_type: Expression::Identifier(name),
                    span,
                }) = operand
                {
                    if accumulated_type == builtin_types::TYPE {
                        let span = last_operand_span.unwrap_or(*op_span).merge(span);
                        let message = format!("All type definitions need to be public, including '{}'.\nHint: to make it public do 'public <type> ={}'", name, name);
                        return err_span(message, self.get_current_source(), span);
                    }
                    *assigned_in_this_chain.entry(name.clone()).or_insert(0) += 1;
                    self.bind_identifier_type(name.clone(), accumulated_type.clone());
                }
            }
        }
        for (to_unbind, times) in assigned_in_this_chain {
            self.unbind_identifier(&to_unbind, times)?;
        }
        Ok(accumulated_type)
    }

    fn check_type_function(&mut self, function: &Function) -> Result<Type, AnyError> {
        let Function {
            parameters,
            body,
            returned,
        } = function;
        let chain_type = self.check_types_scope(parameters.clone(), body)?;
        let return_unified = unify(&chain_type, &returned.type_);
        if let Some(unified) = return_unified {
            let function = Type::function(
                parameters.clone(),
                TypedIdentifier {
                    name: returned.name.clone(),
                    type_: unified,
                },
            );
            Ok(function)
        } else {
            err_span(
                type_mismatch(
                    &Expression::Chain(body.clone()),
                    &chain_type,
                    &returned.type_,
                ),
                self.get_current_source(),
                body.content_span(),
            )
        }
    }

    fn check_types_scope(
        &mut self,
        parameters: TypedIdentifiers,
        body: &Chain,
    ) -> Result<Type, AnyError> {
        let mut to_unbind = Vec::new();
        for parameter in parameters {
            to_unbind.push(parameter.name.clone());
            self.bind_typed_identifier(parameter);
        }
        let chain_type = self.check_types_chain(body);
        for name in to_unbind {
            self.unbind_identifier(&name, 1)?;
        }
        chain_type
    }

    fn check_types_scope_single(
        &mut self,
        parameter: TypedIdentifier,
        body: &Chain,
    ) -> Result<Type, AnyError> {
        self.check_types_scope(vec![parameter], body)
    }

    fn check_types_list(&mut self, elements: &Expressions) -> Result<Type, AnyError> {
        let mut types = Vec::new();
        for e in elements {
            types.push(self.get_type(e.syn_type())?);
        }
        return if types.len() == 0 {
            Ok(Type::from_nameless(BuiltinType::Array.name(), vec![]))
        // } else if all_same_type(&types) {
        //     types.truncate(1);
        //     Ok(Type::from_nameless(BuiltinType::Array.name(), types))
        } else {
            Ok(Type::from_nameless(BuiltinType::Tuple.name(), types))
        };
    }

    fn get_operation_type(
        &mut self,
        input: &Type,
        operator: OperatorSpan,
        operands: &Expressions,
    ) -> Result<Type, AnyError> {
        let ExpressionSpan {
            syntactic_type: operand,
            span: operand_span,
        } = operands.get(0).unwrap();
        match operator.operator {
            Operator::Add
            | Operator::Substract
            | Operator::Multiply
            | Operator::Divide
            | Operator::Modulo => {
                let unified_input =
                    self.assert_type_unifies(input, &builtin_types::I64, operator)?;
                let unified_operand =
                    self.assert_expr_unifies(operand, &unified_input, operator.span)?;
                return Ok(unified_operand);
            }
            Operator::Ignore => return self.get_type(operand),
            Operator::Call => {
                return self.get_call_type(input, operands, operator.span);
            }
            Operator::Get => {
                let array = list_any();
                let unified_input = self.assert_type_unifies(&input, &array, operator)?;
                self.assert_expr_unifies(operand, &builtin_types::I64, *operand_span)?;
                if let Type::Nested { children, .. } = unified_input {
                    Ok(children[0].type_.clone())
                } else {
                    err("Bug: array should be a Type::nested at this point")
                }
            }
            Operator::Type => {
                if let Expression::Type(expected_type) = operand {
                    self.assert_type_unifies(input, expected_type, operator)
                } else {
                    err(format!(
                        "Operator ':' can only be followed by a type, not {:?}",
                        operand
                    ))
                }
            }
            Operator::Assignment | Operator::Overwrite => Ok(input.clone()),
            Operator::Concatenate => {
                let array = list_any();
                let unified_input = self.assert_type_unifies(input, &array, operator)?;
                let unified_operand =
                    self.assert_expr_unifies(operand, &unified_input, operator.span)?;
                return Ok(unified_operand);
            }
            Operator::Comparison(_) => {
                let i64 = builtin_types::I64;
                let unified_input = self.assert_type_unifies(input, &i64, operator)?;
                self.assert_expr_unifies(operand, &unified_input, operator.span)?;
                Ok(i64) // TODO: really should be bool
            }

            Operator::Field => {
                unimplemented!()
            }
        }
    }

    fn get_call_type(
        &mut self,
        input_type: &Type,
        operands: &Expressions,
        span: Span,
    ) -> Result<Type, AnyError> {
        let operator_span = OperatorSpan {
            operator: Operator::Call,
            span,
        };
        let callable = operands.get(0).unwrap();
        match &callable.syntactic_type {
            Expression::Identifier(name) => {
                let callable_type = self.get_identifier_type(name)?;
                self.check_type_callable(input_type, operands, &callable_type, span)
            }
            Expression::Chain(chain) => {
                let callable_type = self.check_types_chain(chain)?;
                self.check_type_callable(input_type, operands, &callable_type, span)
            }
            Expression::Function(function) => {
                let callable_type = self.check_type_function(function)?;
                self.check_type_callable(input_type, operands, &callable_type, span)
            }
            Expression::Nothing
            | Expression::Value(_)
            | Expression::Type(_)
            | Expression::StaticList { .. } => {
                let type_str = if let Ok(callable_type) = self.get_type(callable.syn_type()) {
                    format!(" :{}", callable_type)
                } else {
                    "".to_string()
                };
                err_span(
                    format!(
                        "Can not call this type of expression: '{}{}'",
                        callable.syn_type(),
                        type_str
                    ),
                    self.get_current_source(),
                    span,
                )
            }
            Expression::Composed(Composed::Cast(cast)) => {
                self.is_castable_to(input_type, &cast.target_type.type_, span)?;
                Ok(cast.target_type.type_.clone())
            }
            Expression::Composed(Composed::Loop(Loop { body })) => self.check_types_chain(body),
            Expression::Composed(Composed::Browse(browse)) => {
                let unified_elem =
                    self.assert_iterates_elems(input_type, &browse.iteration_elem, span)?;
                let body_type = self.check_types_scope_single(unified_elem, &browse.body)?;
                Ok(body_type)
            }
            Expression::Composed(Composed::BrowseOr(browse_or)) => {
                let unified_elem =
                    self.assert_iterates_elems(input_type, &browse_or.iteration_elem, span)?;
                let body_type = self.check_types_scope_single(unified_elem, &browse_or.body)?;
                self.assert_same_unless_nothing(&body_type, &browse_or.otherwise, span)
            }
            Expression::Composed(Composed::Times(times)) => {
                let unified_input =
                    self.assert_type_unifies(input_type, &builtin_types::I64, operator_span)?;
                let unified_elem = self.assert_typed_identifier_unifies(
                    &TypedIdentifier::nameless(unified_input.clone()),
                    &times.iteration_elem,
                    Operator::Call,
                )?;
                self.check_types_scope_single(unified_elem, &times.body)?;
                Ok(unified_input)
            }
            Expression::Composed(Composed::TimesOr(times_or)) => {
                let unified_input =
                    self.assert_type_unifies(input_type, &builtin_types::I64, operator_span)?;
                let unified_elem = self.assert_typed_identifier_unifies(
                    &TypedIdentifier::nameless(unified_input.clone()),
                    &times_or.iteration_elem,
                    Operator::Call,
                )?;
                let body_type = self.check_types_scope_single(unified_elem, &times_or.body)?;
                self.assert_same_unless_nothing(&body_type, &times_or.otherwise, span)
            }
            Expression::Composed(Composed::Replace(Replace {
                iteration_elem,
                body,
            })) => {
                let unified_elem = self.assert_iterates_elems(input_type, &iteration_elem, span)?;
                let body_type = self.check_types_scope_single(unified_elem.clone(), &body)?;
                let unified_result_elem =
                    self.assert_type_unifies(&unified_elem.type_, &body_type, operator_span)?;
                Ok(Type::from(
                    BuiltinType::Array.name(),
                    vec![TypedIdentifier::nameless(unified_result_elem)],
                ))
            }
            Expression::Composed(Composed::Map(Map {
                iteration_elem,
                body,
            })) => {
                let unified_elem = self.assert_iterates_elems(input_type, &iteration_elem, span)?;
                let body_type = self.check_types_scope_single(unified_elem, &body)?;
                Ok(Type::from(
                    BuiltinType::Array.name(),
                    vec![TypedIdentifier::nameless(body_type)],
                ))
            }
            Expression::Composed(Composed::Branch(branch)) => {
                self.assert_type_unifies(input_type, &builtin_types::I64, operator_span)?;
                let yes_type = self.check_types_chain(&branch.yes)?;
                let no_type = self.check_types_chain(&branch.no)?;
                if let Some(same) = unify(&yes_type, &no_type) {
                    Ok(same)
                } else {
                    Ok(Type::from_nameless(
                        BuiltinType::Or.name(),
                        vec![yes_type, no_type],
                    ))
                }
            }
            Expression::Composed(Composed::Something(something)) => {
                Ok(builtin_types::ANY) // TODO: implement
            }
            Expression::Composed(Composed::Inspect(_)) => Ok(input_type.clone()),
        }
    }

    fn assert_iterates_elems(
        &mut self,
        input_type: &Type,
        iteration_elem: &TypedIdentifier,
        span: Span,
    ) -> Result<TypedIdentifier, AnyError> {
        let expected_input = list_any();
        let unified_input = self.assert_type_unifies(
            input_type,
            &expected_input,
            OperatorSpan {
                operator: Operator::Call,
                span,
            },
        )?;
        let inner_input_type = unified_input.single_element()?;
        let unified_elem = self.assert_typed_identifier_unifies(
            &iteration_elem,
            &inner_input_type,
            Operator::Call,
        )?;
        Ok(unified_elem)
    }

    fn assert_same_unless_nothing(
        &mut self,
        expected: &Type,
        chain: &Chain,
        span: Span,
    ) -> Result<Type, AnyError> {
        let chain_type = self.check_types_chain(chain)?;
        if *expected != builtin_types::NOTHING {
            let unified_output = self.assert_type_unifies(
                &expected,
                &chain_type,
                OperatorSpan {
                    operator: Operator::Call,
                    span,
                },
            )?;
            Ok(unified_output)
        } else {
            Ok(chain_type)
        }
    }
    fn check_type_callable(
        &mut self,
        input_type: &Type,
        callable_and_operands: &Expressions,
        callable_type: &Type,
        span: Span,
    ) -> Result<Type, AnyError> {
        let operands = &callable_and_operands[1..];
        let mut actual_params = Vec::new();
        actual_params.push(TypedIdentifier::nameless(input_type.clone()));
        for operand in operands {
            let operand_type = self.get_type(&operand.syntactic_type)?;
            actual_params.push(TypedIdentifier::nameless(operand_type));
        }
        let expected_function = Type::function(
            actual_params.clone(),
            TypedIdentifier::nameless(builtin_types::ANY),
        );
        match unify(&expected_function, callable_type) {
            Some(Type::Function { returned, .. }) => Ok(returned.type_.clone()),
            None => {
                if let Type::Function {
                    parameters,
                    returned,
                } = callable_type
                {
                    self.assert_typed_identifiers_unify(&actual_params, parameters, span)?;
                    Ok(returned.type_.clone())
                } else {
                    let callable = callable_and_operands[0].syn_type();
                    err(type_mismatch(callable, callable_type, &expected_function))
                }
            }
            _ => err("Bug: should be either a function or a type mismatch"),
        }
    }

    fn is_castable_to(
        &mut self,
        input_type: &Type,
        target_type: &Type,
        span: Span,
    ) -> Result<Type, AnyError> {
        let expanded_input = self.expand(&input_type, span);
        let expanded_target = self.expand(target_type, span);
        let unified = cast(&expanded_input?, &expanded_target?);
        if let Some(unified) = unified {
            Ok(unified)
        } else {
            err_span(
                type_mismatch_op(Operator::Call, &input_type, target_type),
                self.get_current_source(),
                span,
            )
        }
    }

    fn expand(&self, type_: &Type, span: Span) -> Result<Type, AnyError> {
        match type_ {
            Type::Simple { type_name } => {
                if is_builtin_type(type_name.name()).is_some() {
                    Ok(Type::Simple {
                        type_name: type_name.clone(),
                    })
                } else if let Some(ExpressionSpan {
                    syntactic_type: Expression::Type(expanded),
                    ..
                }) = self.program.identifiers.get(type_name.name())
                {
                    Ok(expanded.clone())
                } else {
                    let message = format!("Definition of type '{}' not found.\nHint: add 'public' to its definition, like 'public <type> = {}'", type_name, type_name);
                    err_span(message, self.get_current_source(), span)
                }
            }
            Type::Nested {
                type_name,
                children,
            } => {
                let expanded = self.expand_children(children, span)?;
                Ok(Type::Nested {
                    type_name: type_name.clone(),
                    children: expanded,
                })
            }
            Type::Function {
                parameters,
                returned,
            } => {
                let expanded = self.expand_children(parameters, span)?;
                Ok(Type::Function {
                    parameters: expanded,
                    returned: Box::new(self.expand_typed_identifier(returned, span)?),
                })
            }
        }
    }

    fn expand_children(
        &self,
        children: &TypedIdentifiers,
        span: Span,
    ) -> Result<TypedIdentifiers, AnyError> {
        let mut expanded = Vec::new();
        for child in children {
            expanded.push(self.expand_typed_identifier(child, span)?);
        }
        Ok(expanded)
    }
    fn expand_typed_identifier(
        &self,
        type_: &TypedIdentifier,
        span: Span,
    ) -> Result<TypedIdentifier, AnyError> {
        Ok(TypedIdentifier {
            name: type_.name.clone(),
            type_: self.expand(&type_.type_, span)?,
        })
    }

    fn assert_expr_unifies(
        &mut self,
        actual_expression: &Expression,
        expected: &Type,
        span: Span,
    ) -> Result<Type, AnyError> {
        let actual_type = self.get_type(actual_expression)?;
        let unified = unify(&actual_type, expected);
        if let Some(unified) = unified {
            Ok(unified)
        } else {
            let source = self.get_current_source();
            err_span(
                type_mismatch(actual_expression, &actual_type, expected),
                source,
                span,
            )
        }
    }
    fn assert_type_unifies(
        &mut self,
        actual: &Type,
        expected: &Type,
        OperatorSpan { operator, span }: OperatorSpan,
    ) -> Result<Type, AnyError> {
        let unified = unify(&actual, expected);
        if let Some(unified) = unified {
            Ok(unified)
        } else {
            let source = self.get_current_source();
            err_span(type_mismatch_op(operator, actual, expected), source, span)
        }
    }

    fn get_current_source(&self) -> &SourceCode {
        if let Some(source_path) = &self.current_source {
            if let Some(source) = self.program.sources.get(source_path) {
                source
            } else {
                panic!("Bug: attempted to print source code '{}' but we didn't store it. Available: {:?}", source_path, self.program.sources.keys())
            }
        } else {
            &self.program.main_source
        }
    }
    fn assert_typed_identifier_unifies(
        &mut self,
        actual: &TypedIdentifier,
        expected: &TypedIdentifier,
        operator: Operator,
    ) -> Result<TypedIdentifier, AnyError> {
        let unified = unify_typed_identifier(expected, &actual);
        if let Some(unified) = unified {
            Ok(unified)
        } else {
            err(type_mismatch_op(operator, &actual.type_, &expected.type_))
        }
    }
    fn assert_typed_identifiers_unify(
        &mut self,
        actual: &TypedIdentifiers,
        expected: &TypedIdentifiers,
        span: Span,
    ) -> Result<TypedIdentifiers, AnyError> {
        let unified = unify_typed_identifiers(expected, &actual);
        if let Some(unified) = unified {
            Ok(unified)
        } else {
            err(type_mismatch_call(
                actual,
                expected,
                self.get_current_source().format_span(span),
            ))
        }
    }
}

fn list_any() -> Type {
    let expected_input = Type::from(
        BuiltinType::List.name(),
        vec![TypedIdentifier::nameless(builtin_types::ANY)],
    );
    expected_input
}

fn type_mismatch(actual_expression: &Expression, actual: &Type, expected: &Type) -> String {
    format!(
        "Type mismatch for expression '{}':\
        \n    actual:   {}\
        \n    expected: {}\n",
        actual_expression, actual, expected
    )
}
fn type_mismatch_op(operator: Operator, actual: &Type, expected: &Type) -> String {
    format!(
        "Type mismatch before operator '{}':\
        \n    actual:   {}\
        \n    expected: {}\n",
        operator, actual, expected
    )
}

fn type_mismatch_call(
    actual: &TypedIdentifiers,
    expected: &TypedIdentifiers,
    location: String,
) -> String {
    format!(
        "Type mismatch for arguments when calling function:\
        \n    actual arguments:   {}\
        \n    expected arguments: {}\n{}",
        typed_identifiers_to_str(actual, true),
        typed_identifiers_to_str(expected, true),
        location
    )
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::common::unwrap_display;
    use crate::evaluate::Runtime;
    use crate::frontend::{lex_and_parse, lex_and_parse_with_identifiers, parse_type};

    use super::*;

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
    fn assert_type_eq(code: &str, expected: &str) {
        let program = &parse(code);
        let expected_type = parse_type(expected);
        let type_ = unwrap_display(get_type(program));
        assert_eq!(type_.to_string(), expected_type.to_string());
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
    fn test_empty_function() {
        assert_eq!(
            parse_type("function"),
            Type::function(vec![], TypedIdentifier::nameless_any())
        )
    }

    #[test]
    fn test_basic_call_type() {
        assert_type_eq("function(x){4} =f ; 5 |f", "i64");
    }

    #[test]
    fn test_basic_operation() {
        assert_types_ok("4 +3 -1 |* 5 |/ 3 %2 :i64");
    }

    #[test]
    fn test_basic_array() {
        assert_types_ok("[1 2] :tuple(:i64 :i64)");
        assert_eq!(parse_type("array"), parse_type("array(:any)"));
    }
    #[test]
    fn test_basic_tuple() {
        assert_types_ok("[1 function{}] :tuple(:i64 :function)");
        assert_types_ok("[1 2] :tuple(:i64 :i64)");
    }
    #[test]
    fn test_tuples_and_arrays_not_mixed() {
        assert_types_wrong("[1 function{}] :array(:i64)");
    }

    #[test]
    fn test_empty_array() {
        let parsed = parse("[] :array(:i64)");
        let type_ = unwrap_display(get_type(&parsed));
        assert_eq!(
            type_,
            Type::from_nameless(BuiltinType::Array.name(), vec![builtin_types::I64])
        )
    }

    #[test]
    fn test_expand() {
        let coord_type = Type::simple("Coord");
        let mut program = Program::new_raw(Expression::Type(coord_type.clone()));
        let expected_type = parse_type("tuple(x :i64  y :i64)");
        program.identifiers.insert(
            coord_type.name().to_string(),
            ExpressionSpan::new_spanless(Expression::Type(expected_type.clone())),
        );
        let typer = unwrap_display(Typer::new(&program));
        let expanded = unwrap_display(typer.expand(&coord_type, program.main().span));
        assert_eq!(expanded, expected_type);
    }

    #[test]
    fn test_struct() {
        assert_types_ok("public tuple(x :i64  y :i64) =Coord");
        assert_types_ok("public tuple(x :i64  y :i64) =Coord ; function (c :Coord) { c }");
        assert_types_ok("public tuple(x :i64  y :i64) =Coord ; [3 5] |cast(:Coord) :Coord");
        assert_types_ok(
            "public tuple(x :i64  y :i64) =Coord ; [3 5] |cast(:Coord) |function (c :Coord) { c }",
        );
        assert_types_ok(
            "public tuple(x :i64  y :i64) =Coord ; [1] ++{[3 5] |cast(:Coord) |cast(:list)}",
        );
    }
    #[test]
    fn test_forbid_non_public_structs() {
        assert_types_wrong("tuple(x :i64  y :i64) =Coord");
    }

    #[test]
    fn test_cast() {
        assert_types_ok("[] |cast(:array(:i64))");
    }

    #[test]
    fn test_chain_inside_array() {
        assert_type_eq("[{5 + 4}]", "tuple(:i64)");
    }

    #[test]
    fn test_identifier() {
        let func = lex_and_parse("function(x :i64) {x+1}").unwrap();
        let identifiers = HashMap::from([("increment".to_string(), func.take().0)]);
        let lib: HashSet<String> = identifiers.keys().cloned().collect();
        let mut main = lex_and_parse_with_identifiers("4 |increment", lib.clone()).unwrap();
        main.identifiers = identifiers.clone();

        assert_ok(check_types(&main));
        let read_input: &[u8] = &[];
        let print_output = Vec::<u8>::new();
        let result = Runtime::evaluate(main, read_input, print_output);
        assert_eq!(result.unwrap(), 5);

        let mut main = lex_and_parse_with_identifiers("increment", lib).unwrap();
        main.identifiers = identifiers;
        let type_ = get_type(&main).unwrap();
        let expected = parse_type("function(x:i64) (:i64)").to_string();
        assert_eq!(type_.to_string(), expected);
    }

    #[test]
    fn test_chain() {
        assert_types_ok("[1] ++[2] #0 :i64 =n +1 =>n");
    }

    #[test]
    fn test_loop() {
        assert_type_eq("[1] |browse(e) {e}", "i64");
        assert_type_eq("[1] |browse(e) {}", "nothing");
        assert_types_wrong("1 |browse(e) {e}");
    }
    #[test]
    fn test_loop_or() {
        assert_type_eq("[1] |browse_or(e) {e} {0}", "i64");
        assert_type_eq("[1] |browse_or(e) {} {0}", "i64");
        assert_types_wrong("[1] |browse_or(e) {e} {[]}");
    }
    #[test]
    fn test_times() {
        assert_type_eq("2 |times(i) {i}", "i64");
        assert_types_wrong("[] |times(i) {i}");
        assert_types_wrong("3 |times(i :array) {i}");
        assert_types_wrong("3 |times(i) {i ++[]}");
    }
    #[test]
    fn test_times_or() {
        assert_type_eq("2 |times_or(i) {i} {0}", "i64");
        assert_type_eq("2 |times_or(i) {} {0}", "i64");
        assert_types_wrong("[] |times_or(i) {} {0}");
        assert_types_wrong("3 |times_or(i :array) {} {0}");
        assert_types_wrong("3 |times_or(i) {i ++[]} {0}");
        assert_types_wrong("3 |times_or(i) {i ++[]} {[]}");
    }
    #[test]
    fn test_branch() {
        assert_type_eq("1 |branch {1} {0}", "i64");
        assert_type_eq("1 |branch {[]} {0}", "or(:array(:any) :i64)");
        assert_type_eq("1 |branch {[0]} {0}", "or(:tuple(:i64) :i64)");
        assert_type_eq("1 |branch {} {0}", "or(:nothing :i64)");
    }

    #[test]
    fn test_comparison() {
        assert_type_eq("5 =?2", "i64");
        assert_types_wrong("5 =?[]");
    }
    #[test]
    fn test_map() {
        assert_type_eq("[1] |map(e) {e +10}", "array(:i64)");
        assert_type_eq("[1] |map(e) {[1]}", "array(:tuple(:i64))");
    }
    #[test]
    fn test_replace() {
        assert_type_eq("[1] |replace(e) {e +10}", "array(:i64)");
        assert_types_wrong("[1] |replace(e) {[1]}");
    }

    #[test]
    fn test_field_access() {
        assert_type_eq(
            "public tuple(x :i64 y :i64) =Coords; [1 2] |cast(:Coords) .x",
            "i64",
        );
    }
}
