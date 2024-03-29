use std::collections::HashMap;

use strum::IntoEnumIterator;

use crate::common::{context, err, err_span, maybe_format_span, AnyError};
use crate::frontend::expression::display::typed_identifiers_to_str;
use crate::frontend::expression::{
    Branch, Browse, BrowseOr, Chain, Composed, Expression, ExpressionSpan, Expressions, Filter,
    Function, Inspect, Loop, Map, Operation, Replace, Something, Times, TimesOr, Type,
    TypedIdentifier, TypedIdentifiers,
};
use crate::frontend::program::Program;
use crate::frontend::sources::location::{SourceCode, Span};
use crate::frontend::sources::token::{Keyword, Operator, OperatorSpan, FIELD};
use crate::frontend::sources::Sources;
use crate::middleend::intrinsics::{builtin_types, is_builtin_type, BuiltinType, Intrinsic};
use crate::middleend::typing::cast::cast;
use crate::middleend::typing::expand::{Expand, TypeView};
use crate::middleend::typing::unify::{
    is_something_or_nothing, join_or, unify, unify_typed_identifier, unify_typed_identifiers,
};

pub mod cast;
pub mod expand;
pub mod unify;

pub fn add_types(program: &Program) -> Result<Program, AnyError> {
    context("Type checking", Typer::add_types_to_program(program))
}

pub fn put_types(program: &mut Program) -> Result<(), AnyError> {
    let typed_program = context("Type checking", Typer::add_types_to_program(program))?;
    *program = typed_program;
    Ok(())
}

pub fn check_types(program: &Program) -> Result<(), AnyError> {
    add_types(program).map(|_| ())
}

pub fn get_type(program: &Program) -> Result<Type, AnyError> {
    Ok(add_types(program)?.main.take_sem_type())
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
    typed_identifiers: HashMap<String, ExpressionSpan>,
    sources: &'a Sources,
    identifier_types: HashMap<String, BindingsTypesStack>,
    current_source: Option<String>,
}

/// Setup and utils to manage state
impl<'a> Typer<'a> {
    pub fn add_types_to_program(program: &Program) -> Result<Program, AnyError> {
        let (mut typer, main) = Typer::new(program)?;
        let typed_main = typer.add_types(main)?;
        Ok(Program::new_from(
            typed_main,
            typer.typed_identifiers,
            program.sources.clone(),
        ))
    }

    pub fn new(program: &'a Program) -> Result<(Self, &'a ExpressionSpan), AnyError> {
        let identifier_types = Self::build_intrinsics();
        let Program {
            identifiers,
            sources,
            ..
        } = program;
        let mut typer = Self {
            typed_identifiers: HashMap::new(),
            sources,
            identifier_types,
            current_source: None,
        };
        typer.setup_constants(identifiers)?;
        Ok((typer, program.main()))
    }

    fn build_intrinsics() -> HashMap<String, BindingsTypesStack> {
        let mut identifiers = HashMap::new();
        for intrinsic in Intrinsic::iter() {
            identifiers.insert(intrinsic.name().to_string(), vec![intrinsic.type_()]);
        }
        identifiers
    }

    fn setup_constants(
        &mut self,
        identifiers: &HashMap<String, ExpressionSpan>,
    ) -> Result<(), AnyError> {
        let mut identifiers_vec = identifiers.iter().collect::<Vec<_>>();
        loop {
            let mut failed = Vec::new();
            let identifiers_count_previous = identifiers_vec.len();
            let mut errors = Vec::new();
            for (name, identifier_expression) in identifiers_vec {
                self.current_source = Some(Self::identifier_to_source(name));
                let typed_identifier_expression =
                    context("Typing setup", self.add_types(&identifier_expression));
                match typed_identifier_expression {
                    Ok(expr_span) => {
                        self.bind_identifier_type(name.clone(), expr_span.sem_type().clone());
                        self.typed_identifiers.insert(name.clone(), expr_span);
                    }
                    Err(e) => {
                        errors.push(e);
                        failed.push((name, identifier_expression));
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
                let mut error_messages = errors.iter().map(|e| e.to_string()).collect::<Vec<_>>();
                error_messages.sort(); // ensure same ordering across iterations of pipes-check
                let joined_error_messages = error_messages.join("\n");
                return err(format!("{}: {}", error_intro, joined_error_messages));
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
}

/// Recursive functions to add types
impl<'a> Typer<'a> {
    pub fn add_types(
        &mut self,
        expression_span: &ExpressionSpan,
    ) -> Result<ExpressionSpan, AnyError> {
        let ExpressionSpan {
            syntactic_type,
            span,
            ..
        } = expression_span;
        let clone_with_sem_type =
            |new_type: Type| ExpressionSpan::new(syntactic_type.clone(), new_type, *span);

        match &syntactic_type {
            Expression::Nothing => Ok(clone_with_sem_type(builtin_types::NOTHING)),
            Expression::Value(_) => Ok(clone_with_sem_type(builtin_types::I64)),
            Expression::Identifier(name) => self.check_types_identifier(name, *span),
            Expression::Type(_) => Ok(clone_with_sem_type(builtin_types::TYPE)),
            Expression::Chain(chain) => self.check_types_chain(chain, *span),
            Expression::StaticList { elements } => self.check_types_list(elements, *span),
            Expression::Function(function) => self.check_type_function(function, *span),
            Expression::Composed(_) => {
                unimplemented!()
            }
        }
    }
    fn check_types_identifier(
        &mut self,
        name: &String,
        span: Span,
    ) -> Result<ExpressionSpan, AnyError> {
        Ok(ExpressionSpan::new(
            Expression::Identifier(name.clone()),
            self.get_identifier_type(name)?,
            span,
        ))
    }
    fn check_types_chain(&mut self, chain: &Chain, span: Span) -> Result<ExpressionSpan, AnyError> {
        self.check_types_chain_with_input(&builtin_types::NOTHING, chain, span)
    }

    fn check_types_chain_with_input(
        &mut self,
        input_type: &Type,
        chain: &Chain,
        span: Span,
    ) -> Result<ExpressionSpan, AnyError> {
        let mut last_operand_span = None;
        let mut accumulated_type = input_type.clone();
        let mut typed_operations = Vec::new();

        let mut assigned_in_this_chain = HashMap::new();
        for operation in &chain.operations {
            let typed_operation = self.get_operation_type(
                &accumulated_type,
                operation.operator,
                &operation.operands,
            )?;
            accumulated_type = typed_operation.sem_type.clone();
            if let Operation {
                operator:
                    OperatorSpan {
                        operator: Operator::Assignment,
                        span: op_span,
                    },
                operands,
                ..
            } = &typed_operation
            {
                let operand = operands.first();
                if let Some(ExpressionSpan {
                    syntactic_type: Expression::Identifier(name),
                    span,
                    ..
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
            // typed_operation.sem_type =
            //     self.expand(&accumulated_type, last_operand_span.unwrap_or_default())?; // TODO: isn't this unnecessary?
            typed_operations.push(typed_operation);
            last_operand_span = operation.operands.last().map(|o| o.span);
        }
        for (to_unbind, times) in assigned_in_this_chain {
            self.unbind_identifier(&to_unbind, times)?;
        }
        Ok(ExpressionSpan::new(
            Expression::Chain(Chain::new(typed_operations)),
            accumulated_type,
            span,
        ))
    }

    fn check_type_function(
        &mut self,
        function: &Function,
        span: Span,
    ) -> Result<ExpressionSpan, AnyError> {
        let Function {
            parameters,
            body,
            returned,
        } = function;
        let typed_chain = self.check_types_scope(parameters.clone(), body, span)?;
        let return_unified = unify(&typed_chain.sem_type(), &returned.type_);
        let (typed_params, typed_return) = if let Some(unified) = return_unified {
            (
                parameters.clone(),
                TypedIdentifier {
                    name: returned.name.clone(),
                    type_: unified,
                },
            )
        } else {
            err(self.type_mismatch(&typed_chain, &returned.type_))?
        };
        Ok(ExpressionSpan::new(
            Expression::function(
                typed_params.clone(),
                typed_return.clone(),
                typed_chain.syntactic_type.to_chain()?,
            ),
            Type::function(typed_params, typed_return),
            span,
        ))
    }

    fn check_types_scope(
        &mut self,
        parameters: TypedIdentifiers,
        body: &Chain,
        span: Span,
    ) -> Result<ExpressionSpan, AnyError> {
        let mut to_unbind = Vec::new();
        for parameter in parameters {
            to_unbind.push(parameter.name.clone());
            self.bind_typed_identifier(parameter);
        }
        let chain = self.check_types_chain(body, span);
        for name in to_unbind {
            self.unbind_identifier(&name, 1)?;
        }
        chain
    }

    fn check_types_scope_single(
        &mut self,
        parameter: TypedIdentifier,
        body: &Chain,
        span: Span,
    ) -> Result<ExpressionSpan, AnyError> {
        self.check_types_scope(vec![parameter], body, span)
    }

    fn check_types_list(
        &mut self,
        untyped_elements: &Expressions,
        span: Span,
    ) -> Result<ExpressionSpan, AnyError> {
        let mut elements = Expressions::new();
        for e in untyped_elements {
            elements.push(self.add_types(e)?);
        }
        let type_ = if elements.len() == 0 {
            Type::from_nameless(BuiltinType::Array.name(), vec![])
            // } else if all_same_type(&types) {
            //     types.truncate(1);
            //     Ok(Type::from_nameless(BuiltinType::Array.name(), types))
        } else {
            Type::from_nameless(
                BuiltinType::Tuple.name(),
                elements.iter().map(|e| e.sem_type().clone()).collect(),
            )
        };
        Ok(ExpressionSpan::new(
            Expression::StaticList { elements },
            type_,
            span,
        ))
    }

    fn get_operation_type(
        &mut self,
        input: &Type,
        operator: OperatorSpan,
        operands: &Expressions,
    ) -> Result<Operation, AnyError> {
        let operand_expr_span = operands.get(0).unwrap();
        let ExpressionSpan {
            syntactic_type: operand,
            span: operand_span,
            ..
        } = operand_expr_span;
        match operator.operator {
            Operator::Add
            | Operator::Substract
            | Operator::Multiply
            | Operator::Divide
            | Operator::Modulo => {
                let unified_input =
                    self.assert_type_unifies(input, &builtin_types::I64, operator)?;
                let typed_operand =
                    self.assert_expr_unifies(operand_expr_span, &unified_input, operator.span)?;
                let type_ = typed_operand.semantic_type.clone();
                Ok(Operation::single(operator, typed_operand, type_))
            }
            Operator::Ignore => {
                let typed_operand = self.add_types(operand_expr_span)?;
                let type_ = typed_operand.semantic_type.clone();
                Ok(Operation::single(operator, typed_operand, type_))
            }
            Operator::Call => self.get_call_type(input, operands, operator.span),
            Operator::Get => {
                let array = list_any();
                let unified_input = self.is_castable_to(input, &array, operator)?;
                let typed_operand = self.assert_expr_unifies(
                    operand_expr_span,
                    &builtin_types::I64,
                    *operand_span,
                )?;
                Ok(Operation::single(
                    operator,
                    typed_operand,
                    unified_input.single_element()?.type_,
                ))
            }
            Operator::Type => {
                if let Expression::Type(expected_type) = operand {
                    let unified_type = self.assert_type_unifies(input, expected_type, operator)?;
                    let operand = ExpressionSpan::new(
                        Expression::Type(unified_type.clone()),
                        builtin_types::TYPE,
                        *operand_span,
                    );
                    Ok(Operation::single(operator, operand, unified_type))
                } else {
                    err(format!(
                        "Operator ':' can only be followed by a type, not {:?}",
                        operand
                    ))
                }
            }
            Operator::Assignment | Operator::Overwrite => Ok(Operation::single(
                operator,
                ExpressionSpan::new(operand.clone(), input.clone(), *operand_span),
                input.clone(),
            )),
            Operator::Concatenate => {
                let array = list_any();
                let mut unified_input = self.assert_type_unifies(input, &array, operator)?;
                let mut typed_operands = Vec::new();
                for operand in operands {
                    let typed_operand =
                        self.assert_expr_unifies(operand, &unified_input, operator.span)?;
                    unified_input = typed_operand.semantic_type.clone();
                    typed_operands.push(typed_operand);
                }
                Ok(Operation::several(operator, typed_operands, unified_input))
            }
            Operator::Comparison(_) => {
                let unified_input =
                    self.assert_type_unifies(input, &builtin_types::I64, operator)?;
                let typed_operand =
                    self.assert_expr_unifies(operand_expr_span, &unified_input, operator.span)?;
                let type_ = typed_operand.semantic_type.clone(); // TODO: really should be bool
                Ok(Operation::single(operator, typed_operand, type_))
            }
            Operator::Field => self.get_type_field(input, operator, operand_expr_span),
        }
    }

    fn get_call_type(
        &mut self,
        input_type: &Type,
        operands: &Expressions,
        span: Span,
    ) -> Result<Operation, AnyError> {
        let operator_span = OperatorSpan {
            operator: Operator::Call,
            span,
        };
        let mut callable = operands.get(0).unwrap().clone();
        match &callable.syntactic_type {
            Expression::Identifier(name) => {
                callable.semantic_type = self.get_identifier_type(name)?;
                self.check_type_callable(input_type, callable, &operands[1..], operator_span)
            }
            Expression::Chain(chain) => {
                let typed_callable = self.check_types_chain(chain, span)?;
                self.check_type_callable(input_type, typed_callable, &operands[1..], operator_span)
            }
            Expression::Function(function) => {
                let typed_callable = self.check_type_function(function, span)?;
                self.check_type_callable(input_type, typed_callable, &operands[1..], operator_span)
            }
            Expression::Composed(composed) => {
                self.add_types_composed(input_type, composed, operator_span, callable.span)
            }
            Expression::Nothing
            | Expression::Value(_)
            | Expression::Type(_)
            | Expression::StaticList { .. } => {
                let type_str = if let Ok(callable_type) = self.add_types(&callable) {
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
        }
    }
    fn add_types_composed(
        &mut self,
        input_type: &Type,
        composed: &Composed,
        operator_span: OperatorSpan,
        composed_span: Span,
    ) -> Result<Operation, AnyError> {
        let span = operator_span.span;
        let to_chain_and_type = |typed_body: ExpressionSpan| -> Result<(Chain, Type), AnyError> {
            let op_type = typed_body.semantic_type;
            let body = typed_body.syntactic_type.to_chain()?;
            Ok((body, op_type))
        };
        let (typed_composed, operation_type) = match composed {
            Composed::Cast(cast) => {
                // note that we ignore the unified op_type! we are casting to something that might
                // need being expanded to be unified, but we don't want the final type to be expanded
                let _operation_type =
                    self.is_castable_to(input_type, &cast.target_type.type_, operator_span)?;
                (Composed::Cast(cast.clone()), cast.target_type.type_.clone())
            }
            Composed::Loop(Loop { body }) => {
                let typed_body = self.check_types_chain(body, span)?;
                let (body, op_type) = to_chain_and_type(typed_body)?;
                let op_type = if let Some(something) = is_something_or_nothing(&op_type) {
                    something.type_
                } else {
                    op_type
                };
                (Composed::Loop(Loop { body }), op_type)
            }
            Composed::Browse(browse) => {
                let unified_elem =
                    self.assert_iterates_elems(input_type, &browse.iteration_elem, span)?;
                let typed_body =
                    self.check_types_scope_single(unified_elem.clone(), &browse.body, span)?;
                let (body, op_type) = to_chain_and_type(typed_body)?;
                (
                    Composed::Browse(Browse {
                        iteration_elem: unified_elem,
                        body,
                    }),
                    op_type,
                )
            }
            Composed::BrowseOr(browse_or) => {
                let unified_elem =
                    self.assert_iterates_elems(input_type, &browse_or.iteration_elem, span)?;
                let typed_body =
                    self.check_types_scope_single(unified_elem.clone(), &browse_or.body, span)?;
                let (typed_otherwise, op_type) = self.assert_same_unless_nothing(
                    &typed_body.sem_type(),
                    &browse_or.otherwise,
                    span,
                )?;
                (
                    Composed::BrowseOr(BrowseOr {
                        iteration_elem: unified_elem,
                        body: typed_body.syntactic_type.to_chain()?,
                        otherwise: typed_otherwise.syntactic_type.to_chain()?,
                    }),
                    op_type,
                )
            }
            Composed::Times(times) => {
                let unified_input =
                    self.assert_type_unifies(input_type, &builtin_types::I64, operator_span)?;
                let unified_elem = self.assert_typed_identifier_unifies(
                    &TypedIdentifier::nameless(unified_input.clone()),
                    &times.iteration_elem,
                    Operator::Call,
                )?;
                let typed_body =
                    self.check_types_scope_single(unified_elem.clone(), &times.body, span)?;
                (
                    Composed::Times(Times {
                        iteration_elem: unified_elem,
                        body: typed_body.syntactic_type.to_chain()?,
                    }),
                    unified_input,
                )
            }
            Composed::TimesOr(times_or) => {
                let unified_input =
                    self.assert_type_unifies(input_type, &builtin_types::I64, operator_span)?;
                let unified_elem = self.assert_typed_identifier_unifies(
                    &TypedIdentifier::nameless(unified_input.clone()),
                    &times_or.iteration_elem,
                    Operator::Call,
                )?;
                let typed_body =
                    self.check_types_scope_single(unified_elem.clone(), &times_or.body, span)?;

                let (typed_otherwise, op_type) = self.assert_same_unless_nothing(
                    &typed_body.sem_type(),
                    &times_or.otherwise,
                    span,
                )?;
                (
                    Composed::TimesOr(TimesOr {
                        iteration_elem: unified_elem,
                        body: typed_body.syntactic_type.to_chain()?,
                        otherwise: typed_otherwise.syntactic_type.to_chain()?,
                    }),
                    op_type,
                )
            }
            Composed::Replace(Replace {
                iteration_elem,
                body,
            }) => {
                let unified_elem = self.assert_iterates_elems(input_type, &iteration_elem, span)?;
                let typed_body =
                    self.check_types_scope_single(unified_elem.clone(), &body, span)?;
                let unified_result_elem = self.assert_type_unifies(
                    &unified_elem.type_,
                    typed_body.sem_type(),
                    operator_span,
                )?;
                let output_list_type = Type::from(
                    BuiltinType::Array.name(),
                    vec![TypedIdentifier::nameless(unified_result_elem)],
                );
                (
                    Composed::Replace(Replace {
                        iteration_elem: unified_elem,
                        body: typed_body.syntactic_type.to_chain()?,
                    }),
                    output_list_type,
                )
            }
            Composed::Map(Map {
                iteration_elem,
                body,
            }) => {
                let unified_elem = self.assert_iterates_elems(input_type, &iteration_elem, span)?;
                let typed_body =
                    self.check_types_scope_single(unified_elem.clone(), &body, span)?;
                let output_list_type = Type::from(
                    BuiltinType::Array.name(),
                    vec![TypedIdentifier::nameless(typed_body.semantic_type)],
                );
                (
                    Composed::Map(Map {
                        iteration_elem: unified_elem,
                        body: typed_body.syntactic_type.to_chain()?,
                    }),
                    output_list_type,
                )
            }
            Composed::Filter(Filter {
                iteration_elem,
                body,
            }) => {
                let unified_elem = self.assert_iterates_elems(input_type, &iteration_elem, span)?;
                let typed_body =
                    self.check_types_scope_single(unified_elem.clone(), &body, span)?;
                let _unified_body = self.assert_type_unifies(
                    typed_body.sem_type(),
                    &builtin_types::I64,
                    operator_span,
                )?;
                let output_list_type = Type::from(
                    BuiltinType::Array.name(),
                    vec![TypedIdentifier::nameless(unified_elem.type_.clone())],
                );
                (
                    Composed::Filter(Filter {
                        iteration_elem: unified_elem,
                        body: typed_body.syntactic_type.to_chain()?,
                    }),
                    output_list_type,
                )
            }
            Composed::Branch(branch) => {
                self.assert_type_unifies(input_type, &builtin_types::I64, operator_span)?;
                let yes = self.check_types_chain(&branch.yes, span)?;
                let no = self.check_types_chain(&branch.no, span)?;
                let op_type = Self::unify_or_group_as_or(yes.sem_type(), no.sem_type());
                (
                    Composed::Branch(Branch {
                        yes: yes.syntactic_type.to_chain()?,
                        no: no.syntactic_type.to_chain()?,
                    }),
                    op_type,
                )
            }
            Composed::Something(Something {
                elem,
                something,
                nothing,
            }) => {
                let expected_input = join_or(&builtin_types::ANY, &builtin_types::NOTHING);
                let unified_elem =
                    self.assert_type_unifies(input_type, &expected_input, operator_span)?;
                let typed_something = if let Some(some) = is_something_or_nothing(&unified_elem) {
                    let unified_elem =
                        self.assert_typed_identifier_unifies(&some, elem, Operator::Call)?;
                    self.check_types_scope_single(unified_elem, &something, span)
                } else {
                    err(format!(
                        "Bug: the input of a |{} must be {}",
                        Keyword::Something.name(),
                        expected_input
                    ))
                }?;
                let typed_nothing = self.check_types_chain(&nothing, span)?;

                let op_type = Self::unify_or_group_as_or(
                    typed_something.sem_type(),
                    typed_nothing.sem_type(),
                );
                (
                    Composed::Something(Something {
                        elem: TypedIdentifier::new(elem.name.clone(), unified_elem),
                        something: typed_something.syntactic_type.to_chain()?,
                        nothing: typed_nothing.syntactic_type.to_chain()?,
                    }),
                    op_type,
                )
            }
            Composed::Inspect(inspect) => {
                let unified_elem =
                    self.assert_type_unifies(input_type, &inspect.elem.type_, operator_span)?;
                (
                    Composed::Inspect(Inspect {
                        elem: TypedIdentifier::new(inspect.elem.name.clone(), unified_elem.clone()),
                        body: inspect.body.clone(),
                    }),
                    unified_elem,
                )
            }
        };
        let operands = ExpressionSpan::new(
            Expression::Composed(typed_composed),
            builtin_types::COMPOSED,
            composed_span,
        );
        Ok(Operation::single(operator_span, operands, operation_type))
    }

    fn unify_or_group_as_or(first: &Type, second: &Type) -> Type {
        let op_type = if let Some(same) = unify(first, second) {
            same
        } else {
            join_or(first, second)
        };
        op_type
    }
    fn get_type_field(
        &mut self,
        input: &Type,
        operator: OperatorSpan,
        operand: &ExpressionSpan,
    ) -> Result<Operation, AnyError> {
        if let Expression::Identifier(used_field) = &operand.syntactic_type {
            let expanded = if is_builtin_type(input.name()).is_some() {
                input.clone()
            } else {
                self.expand(input, operator.span)?
            };
            if let Type::Nested {
                type_name,
                children,
            } = expanded
            {
                if type_name.name() == BuiltinType::Tuple.name() {
                    for existing_field in &children {
                        if existing_field.name == *used_field {
                            let typed_operand = ExpressionSpan::new(
                                Expression::Identifier(used_field.clone()),
                                existing_field.type_.clone(),
                                operand.span,
                            );
                            return Ok(Operation::single(
                                operator,
                                typed_operand,
                                existing_field.type_.clone(),
                            ));
                        }
                    }
                }
                let fields = typed_identifiers_to_str(&children, true);
                let message = format!(
                    "Field '{}' doesn't exist in type '{}'. Existing fields: {}",
                    operand, input, fields
                );
                err_span(message, self.get_current_source(), operator.span)
            } else {
                let message = format!(
                    "Field '{}' doesn't exist in non-nested type '{}'",
                    operand, input
                );
                err_span(message, self.get_current_source(), operator.span)
            }
        } else {
            let message = format!(
                "Bug: After the field access operator '{}' only identifiers can appear, not {}",
                FIELD, operand
            );
            err_span(message, self.get_current_source(), operator.span)
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
    ) -> Result<(ExpressionSpan, Type), AnyError> {
        let typed_chain = self.check_types_chain(chain, span)?;
        let unified_type = if *expected != builtin_types::NOTHING {
            let chain_is_nothing = *typed_chain.sem_type() == builtin_types::NOTHING;
            if let Some(something) = is_something_or_nothing(expected) {
                if chain_is_nothing {
                    join_or(&something.type_, &builtin_types::NOTHING)
                } else {
                    let unified_output = self.assert_type_unifies(
                        &something.type_,
                        &typed_chain.sem_type(),
                        OperatorSpan {
                            operator: Operator::Call,
                            span,
                        },
                    )?;
                    unified_output
                }
            } else if chain_is_nothing {
                join_or(expected, typed_chain.sem_type())
            } else {
                let unified_output = self.assert_type_unifies(
                    &expected,
                    &typed_chain.sem_type(),
                    OperatorSpan {
                        operator: Operator::Call,
                        span,
                    },
                )?;
                unified_output
            }
        } else {
            typed_chain.sem_type().clone()
        };
        Ok((typed_chain, unified_type))
    }

    /// returns the type of the callable and the typed operands
    fn check_type_callable(
        &mut self,
        input_type: &Type,
        mut typed_callable: ExpressionSpan,
        operands: &[ExpressionSpan],
        span: OperatorSpan,
    ) -> Result<Operation, AnyError> {
        let mut actual_params = Vec::new();
        if *input_type != builtin_types::NOTHING {
            actual_params.push(TypedIdentifier::nameless(input_type.clone()));
        }
        let mut typed_operands = Vec::new();
        typed_operands.push(ExpressionSpan::new_spanless(Expression::Nothing)); // to replace with callable for optimization reasons
        for operand in operands {
            let typed_operand = self.add_types(&operand)?;
            actual_params.push(TypedIdentifier::nameless(typed_operand.sem_type().clone()));
            typed_operands.push(typed_operand);
        }
        let expected_function = Type::function(
            actual_params.clone(),
            TypedIdentifier::nameless(builtin_types::ANY),
        );
        match unify(&expected_function, typed_callable.sem_type()) {
            Some(func_type) => {
                let returned = match &func_type {
                    Type::Function { returned, .. } => returned,
                    _ => err("Bug: should be either a function or a type mismatch")?,
                };
                typed_callable.semantic_type = func_type.clone();
                typed_operands[0] = typed_callable;
                Ok(Operation::several(
                    span,
                    typed_operands,
                    returned.type_.clone(),
                ))
            }
            None => {
                if let Type::Function { parameters, .. } = typed_callable.sem_type() {
                    self.assert_typed_identifiers_unify(&actual_params, parameters, span.span)?;
                    unreachable!()
                } else {
                    err(self.type_mismatch(&typed_callable, &expected_function))
                }
            }
        }
    }
}

/// Low level functions and assertions for actually checking the types
impl<'a> Typer<'a> {
    fn is_castable_to(
        &mut self,
        input_type: &Type,
        target_type: &Type,
        OperatorSpan { operator, span }: OperatorSpan,
    ) -> Result<Type, AnyError> {
        let expanded_input = self.expand(&input_type, span);
        let expanded_target = self.expand(target_type, span);
        let unified = cast(&expanded_input?, &expanded_target?);
        if let Some(unified) = unified {
            Ok(unified)
        } else {
            err_span(
                type_mismatch_op(operator, &input_type, target_type),
                self.get_current_source(),
                span,
            )
        }
    }

    fn expand(&self, type_: &Type, span: Span) -> Result<Type, AnyError> {
        Expand::expand(
            type_,
            &TypingTypeView::new(&self.typed_identifiers, &self.sources),
            span,
        )
    }

    fn assert_expr_unifies(
        &mut self,
        actual_expression: &ExpressionSpan,
        expected: &Type,
        span: Span,
    ) -> Result<ExpressionSpan, AnyError> {
        let mut typed_actual_expression = self.add_types(actual_expression)?;
        let unified = unify(&typed_actual_expression.sem_type(), expected);
        if let Some(unified) = unified {
            typed_actual_expression.semantic_type = unified;
            Ok(typed_actual_expression)
        } else {
            err(self.type_mismatch_span(&typed_actual_expression, expected, span))
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
        get_source(self.sources, &self.current_source)
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
    fn type_mismatch(&self, actual_expression: &ExpressionSpan, expected: &Type) -> String {
        self.type_mismatch_span(actual_expression, expected, actual_expression.span)
    }
    fn type_mismatch_span(
        &self,
        actual_expression: &ExpressionSpan,
        expected: &Type,
        span: Span,
    ) -> String {
        format!(
            "Type mismatch for expression '{}':\
            \n    actual:   {}\
            \n    expected: {}\n{}\n",
            actual_expression.syntactic_type,
            actual_expression.semantic_type,
            expected,
            self.get_current_source().format_span(span)
        )
    }
}

fn get_source<'a>(sources: &'a Sources, current_source: &Option<String>) -> &'a SourceCode {
    if let Some(source_path) = current_source.as_ref() {
        if let Some(source) = sources.get(source_path) {
            return source;
        } else if let Some(source) = &sources.get_main().file {
            if source.to_string_lossy() == *source_path {
                return &sources.get_main();
            }
        }
        println!("Bug: attempted to print source code '{}' but we didn't store it. Assuming it's the main source file. Available: {:?}", source_path, sources.keys());
        &sources.get_main()
    } else {
        &sources.get_main()
    }
}

fn list_any() -> Type {
    let expected_input = Type::from(
        BuiltinType::List.name(),
        vec![TypedIdentifier::nameless(builtin_types::ANY)],
    );
    expected_input
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

struct TypingTypeView<'a> {
    typed_identifiers: &'a HashMap<String, ExpressionSpan>,
    _sources: &'a Sources,
}

impl<'a> TypingTypeView<'a> {
    pub fn new(
        typed_identifiers: &'a HashMap<String, ExpressionSpan>,
        _sources: &'a Sources,
    ) -> Self {
        Self {
            typed_identifiers,
            _sources,
        }
    }
}

impl<'a> TypeView for TypingTypeView<'a> {
    fn get_type(&self, name: &str, span: Span) -> Result<&Type, AnyError> {
        if let Some(ExpressionSpan {
            syntactic_type: Expression::Type(type_),
            ..
        }) = self.typed_identifiers.get(name)
        {
            Ok(type_)
        } else {
            let message = format!(
                "Definition of type '{}' not found.\n\
            Hint: add 'public' to its definition, like 'public <type> = {}'{}",
                name,
                name,
                maybe_format_span(self.get_source(name), span)
            );
            err(message)
        }
    }

    fn get_source(&self, _identifier_name: &str) -> Option<&SourceCode> {
        None
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::backend::Runtime;
    use crate::common::unwrap_display;
    use crate::frontend::{lex_and_parse, lex_and_parse_with_identifiers, parse_type};

    use super::*;

    fn parse(code: &str) -> Program {
        unwrap_display(lex_and_parse(code))
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
        );
        assert_type_eq("{} |function {0}", "i64");
    }
    #[test]
    fn test_or_function() {
        assert_type_eq(
            "function {0|branch {} {0}}",
            "function()(:or(:nothing :i64))",
        );
    }

    #[test]
    fn test_basic_call_type() {
        assert_type_eq("function(x){4} =f ; 5 |f", "i64");
    }

    #[test]
    fn test_noop_function() {
        assert_type_eq("5 |function(x){}", "i64");
    }

    #[test]
    fn test_basic_operation() {
        assert_types_ok("4 +3 -1 |* 5 |/ 3 %2 :i64");
    }

    #[test]
    fn test_basic_array() {
        assert_types_ok("[1 2] :tuple(:i64 :i64)");
        assert_type_eq("[1 2]", "tuple(:i64 :i64)");
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
        let span = program.main().span;
        let (typer, _) = unwrap_display(Typer::new(&program));
        let expanded = unwrap_display(typer.expand(&coord_type, span));
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

        assert_ok(put_types(&mut main));
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
    fn test_get() {
        assert_types_wrong("[[1 2] [3]] #1");
        assert_type_eq("[[1 2] [3]] |cast(:list(:list)) #1", "list");
    }
    #[test]
    fn test_concat() {
        // the important thing here is that concat works with tuples of different cardinality
        assert_type_eq("[1 2] ++[3]", "list(:i64)");
    }
    #[test]
    fn test_browse() {
        assert_type_eq("[1] |browse(e) {e}", "i64");
        assert_type_eq("[1] |browse(e) {}", "nothing");
        assert_types_wrong("1 |browse(e) {e}");
    }
    #[test]
    fn test_browse_or() {
        assert_type_eq(
            "[0] |browse_or(condition) {condition |branch {} {0}} {} ",
            "or(:i64 :nothing)",
        );
        assert_type_eq(
            "[0] |browse_or(condition) {condition |branch {} {0}} {1} ",
            "i64",
        );
        assert_type_eq(
            "[0] |browse_or(condition) {condition |branch {1} {0}} {} ",
            "or(:i64 :nothing)",
        );
        assert_type_eq("[1] |browse_or(e) {e} {0}", "i64");
        assert_type_eq("[1] |browse_or(e) {} {0}", "i64");
        assert_type_eq("[1] |browse_or(e) {} {}", "nothing");
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
        assert_type_eq("2 |times_or(i) {0} {}", "or(:i64 :nothing)");
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
    fn test_nested_branch() {
        assert_type_eq("1 |branch {1 |branch { }{0}} { }", "or(:i64 :nothing)");
        assert_type_eq("1 |branch {1 |branch {0}{0}} { }", "or(:i64 :nothing)");
        assert_type_eq("1 |branch {1 |branch { }{0}} {0}", "or(:i64 :nothing)");
        assert_type_eq("1 |branch {1 |branch {0}{0}} {0}", "i64");
    }

    #[test]
    fn test_comparison() {
        assert_type_eq("5 =?2", "i64");
        assert_types_wrong("5 =?[]");
    }
    #[test]
    fn test_inner_type_of_operand() {
        let code = "1<2";
        let expected_type = "i64";
        let program = &parse(code);
        let typed_program = unwrap_display(add_types(program));
        let chain = typed_program.main.syntactic_type.to_chain().unwrap();
        let inner_type = &chain.operations[0].operands[0].semantic_type;
        assert_eq!(inner_type.to_string(), expected_type.to_string());
    }
    #[test]
    fn test_map() {
        assert_type_eq("[1] |map(e) {e +10}", "array(:i64)");
        assert_type_eq("[1] |map(e) {[1]}", "array(:tuple(:i64))");
        assert_type_eq(
            "public tuple(x:i64 y:i64) =coords ;[{[3 4] |cast(:coords)}] |map(c) {c .y}",
            "array(:i64)",
        );
        assert_type_eq(
            "public tuple(x:i64 y:i64) =coords ;[{[3 4] |cast(:coords)}] |map(c) {c}",
            "array(:coords)",
        );
    }
    #[test]
    fn test_filter() {
        assert_type_eq(
            "[[1] [1 2 3] [1 2]] |cast(:list(:list)) |filter(e :list(:i64)) {e |size <=2}",
            "array(:list(:i64))",
        );
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
