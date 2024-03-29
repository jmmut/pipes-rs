use crate::backend::{FunctionOrIntrinsic, Runtime};
use crate::common::{context, err, maybe_format_span, AnyError};
use crate::frontend::expression::{
    Browse, BrowseOr, Chain, Composed, Expression, ExpressionSpan, Expressions, Filter, Function,
    Inspect, Loop, Map, Operation, Replace, Something, Times, TimesOr, Type, TypedIdentifier,
    TypedIdentifiers,
};
use crate::frontend::program::Program;
use crate::frontend::sources::location::{SourceCode, Span, NO_SPAN};
use crate::frontend::sources::token::{Comparison, Operator, FIELD};
use crate::frontend::sources::Sources;
use crate::middleend::intrinsics::{builtin_types, Intrinsic};
use crate::middleend::typing::expand::{Expand, TypeView};
use std::collections::HashMap;
use std::io::{Read, Write};
use std::rc::Rc;
use strum::IntoEnumIterator;

pub type ListPointer = i64;
pub type FunctionPointer = i64;
pub type GenericValue = i64;
pub type BindingsStack = Vec<GenericValue>;

pub const NOTHING: i64 = i64::MIN;

#[derive(Clone)]
pub struct Closure {
    captured_identifiers: HashMap<String, GenericValue>,
}

impl Closure {
    #[allow(unused)]
    pub fn new() -> Closure {
        Self {
            captured_identifiers: HashMap::new(),
        }
    }
    pub fn new_from_current_scope(identifiers: &HashMap<String, BindingsStack>) -> Closure {
        let mut captured_identifiers = HashMap::new();
        for (name, stack) in identifiers {
            if let Some(last) = stack.last() {
                captured_identifiers.insert(name.clone(), *last);
            }
        }
        Self {
            captured_identifiers,
        }
    }
    #[allow(unused)]
    pub fn add_to(&self, all_identifiers: &mut HashMap<String, BindingsStack>) {
        for (captured_name, captured_value) in &self.captured_identifiers {
            all_identifiers
                .entry(captured_name.clone())
                .or_insert(Vec::new())
                .push(*captured_value);
        }
    }
    pub fn to_identifiers(self) -> HashMap<String, BindingsStack> {
        let mut identifiers = HashMap::new();
        for (name, value) in self.captured_identifiers {
            let mut stack = BindingsStack::new();
            stack.push(value);
            identifiers.insert(name, stack);
        }
        identifiers
    }
}

impl<R: Read, W: Write> Runtime<R, W> {
    /// Executes a given program with a given standard input and output.
    ///
    /// To use the regular stdin and stdout of the interpreter process, use:
    /// ```no_run
    /// use pipes_rs::{backend::Runtime, frontend::{program::Program, lex_and_parse}};
    /// let program = lex_and_parse(r#""Hello World!" |print"#).unwrap();
    /// Runtime::evaluate(program, std::io::stdin(), std::io::stdout()).unwrap();
    /// ```
    /// To use a in-memory buffers (useful for providing input or capturing output in tests),
    /// you can do this, because `&[u8]` implements Read and `&mut Vec<u8>` implements Write:
    /// ```no_run
    /// use pipes_rs::{backend::Runtime, frontend::{program::Program, lex_and_parse}};
    /// let expression = lex_and_parse("'5' |print_char ;0 |read_char").unwrap();
    /// let into = "7".as_bytes();
    /// let mut out = Vec::<u8>::new();
    /// let result = Runtime::evaluate(expression, into, &mut out);
    /// assert_eq!(result.unwrap() as u8, '7' as u8);
    /// assert_eq!(out, "5".as_bytes());
    /// ```
    pub fn evaluate(
        program: Program,
        read_input: R,
        print_output: W,
    ) -> Result<GenericValue, AnyError> {
        let (main, identifiers, sources) = program.take();
        let mut runtime = Self::new(read_input, print_output, identifiers, sources)?;
        context("Runtime", runtime.evaluate_recursive(&main))
    }

    fn new(
        read_input: R,
        print_output: W,
        identifiers: HashMap<String, ExpressionSpan>,
        sources: Sources,
    ) -> Result<Runtime<R, W>, AnyError> {
        let (static_identifiers, functions) = Self::build_intrinsics();
        let mut runtime = Runtime {
            lists: HashMap::new(),
            functions,
            identifiers: HashMap::new(),
            types: HashMap::new(),
            static_identifiers,
            read_input,
            print_output,
            _sources: sources,
            extra_inputs: HashMap::new(),
        };
        runtime.setup_constants(identifiers)?;
        Ok(runtime)
    }

    fn build_intrinsics() -> (
        HashMap<String, Vec<GenericValue>>,
        Vec<Rc<FunctionOrIntrinsic>>,
    ) {
        let mut identifiers = HashMap::new();
        let mut functions = Vec::new();
        let mut i = 0;
        for intrinsic in Intrinsic::iter() {
            functions.push(Rc::new(FunctionOrIntrinsic::Intrinsic(intrinsic)));
            identifiers.insert(intrinsic.name().to_string(), vec![i]);
            i += 1;
        }
        (identifiers, functions)
    }

    fn setup_constants(
        &mut self,
        identifiers: HashMap<String, ExpressionSpan>,
    ) -> Result<(), AnyError> {
        let mut identifiers_vec = identifiers.into_iter().collect::<Vec<_>>();
        loop {
            let mut failed = Vec::new();
            let mut errors = Vec::new();
            let identifiers_count_previous = identifiers_vec.len();
            for (name, expression) in identifiers_vec {
                if let ExpressionSpan {
                    syntactic_type: Expression::Type(type_),
                    ..
                } = expression
                {
                    self.types.insert(name.clone(), vec![type_]);
                    self.bind_static_identifier(name, NOTHING);
                } else {
                    match context("Runtime setup", self.evaluate_recursive(&expression)) {
                        Ok(value) => self.bind_static_identifier(name, value),
                        Err(e) => {
                            errors.push(e);
                            failed.push((name, expression));
                        }
                    }
                }
            }
            if failed.len() == 0 {
                return Ok(());
            }
            if failed.len() == identifiers_count_previous {
                let failed_names = failed.iter().map(|(n, _e)| n).collect::<Vec<_>>();
                let error_intro = format!(
                    "it seems there are constants with cyclic dependencies. \
                Program initialization failed for identifiers: {:?}",
                    failed_names
                );

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

    pub fn evaluate_recursive(
        &mut self,
        expression: &ExpressionSpan,
    ) -> Result<GenericValue, AnyError> {
        match expression.syn_type() {
            Expression::Nothing => Ok(NOTHING),
            Expression::Value(n) => Ok(*n),
            Expression::Identifier(name) => self.get_identifier(name),
            Expression::Chain(chain) => self.evaluate_chain(chain),
            Expression::StaticList { elements } => self.evaluate_list(elements),
            Expression::Function(function) => self.allocate_function(
                function.clone(),
                Closure::new_from_current_scope(&self.identifiers),
            ),
            _ => err(format!("Can't evaluate expression {:?}", expression))?,
        }
    }

    fn get_identifier(&self, name: &String) -> Result<GenericValue, AnyError> {
        let value_opt = self
            .identifiers
            .get(name)
            .and_then(|binding_stack| binding_stack.last());
        if let Some(value) = value_opt {
            Ok(*value)
        } else {
            self.static_identifiers.get(name)
                .ok_or_else(|| {
                    // put here your breakpoints
                    format!("Bug: Undefined identifier '{}'. This should have been detected by earlier stages.", name)
                })?
                .last()
                .cloned()
                .ok_or_else(|| format!("Bug: Identifier '{}' is not bound to any value", name).into())
        }
    }

    fn get_list(&self, list_pointer: ListPointer) -> Result<&Vec<GenericValue>, AnyError> {
        self.lists
            .get(&list_pointer)
            .ok_or_else(|| format!("Pointer {} is not a valid array", list_pointer).into())
    }

    fn evaluate_chain(&mut self, chain: &Chain) -> Result<i64, AnyError> {
        self.evaluate_chain_passing_value(NOTHING, &builtin_types::NOTHING, chain)
    }

    fn evaluate_chain_passing_value(
        &mut self,
        initial: GenericValue,
        initial_sem_type: &Type,
        Chain {
            // initial,
            operations,
        }: &Chain,
    ) -> Result<i64, AnyError> {
        let mut identifiers = HashMap::<String, usize>::new();
        let mut accumulated = initial;
        let mut previous_sem_type = initial_sem_type;
        // let mut previous_sem_type = initial
        //     .as_ref()
        //     .map(|i| &i.semantic_type)
        //     .unwrap_or(&builtin_types::UNKNOWN);
        for Operation {
            operator,
            operands,
            sem_type,
        } in operations
        {
            let operand = operands.first().unwrap();
            match &operator.operator {
                Operator::Add => accumulated += self.evaluate_recursive(operand)?,
                Operator::Substract => accumulated -= self.evaluate_recursive(operand)?,
                Operator::Multiply => accumulated *= self.evaluate_recursive(operand)?,
                Operator::Divide => accumulated /= self.evaluate_recursive(operand)?,
                Operator::Modulo => accumulated %= self.evaluate_recursive(operand)?,
                Operator::Ignore => accumulated = self.evaluate_recursive(operand)?,
                Operator::Call => accumulated = self.call_callable(accumulated, operands)?,
                Operator::Get => accumulated = self.get_list_element(accumulated, operand)?,
                Operator::Type => {}
                Operator::Assignment => {
                    self.evaluate_assignment(accumulated, operand, &mut identifiers)?
                }
                Operator::Overwrite => self.evaluate_overwrite(accumulated, operand)?,
                Operator::Concatenate => {
                    accumulated = self.evaluate_concatenate(accumulated, operands)?
                }
                Operator::Comparison(comparison) => {
                    accumulated = self.evaluate_compare(accumulated, *comparison, operand)?
                }
                Operator::Field => {
                    accumulated = self.evaluate_field(accumulated, previous_sem_type, operand)?
                }
            }

            previous_sem_type = sem_type;
        }
        for (identifier, times_redefined_in_this_chain) in identifiers {
            if times_redefined_in_this_chain > 1 {
                return err(format!("Identifier {} was defined multiple times. Maybe you want to use the overwrite operator '=>'.",identifier));
            }
            self.unbind_identifier(&identifier, times_redefined_in_this_chain)?;
        }
        Ok(accumulated)
    }

    fn unbind_identifier(&mut self, identifier: &String, times: usize) -> Result<(), AnyError> {
        let option = self.identifiers.get_mut(identifier);
        let stack = option.ok_or_else(|| {
            format!(
                "Bug: tried to unbind non-existing identifier {}",
                identifier
            )
        })?;
        if times > stack.len() {
            return err(format!(
                "Bug: tried to unbind identifier {} {} times, but it's shadowed only {} times",
                identifier,
                times,
                stack.len()
            ));
        } else {
            stack.truncate(stack.len() - times);
            Ok(())
        }
    }

    fn bind_identifier(&mut self, identifier: String, value: GenericValue) {
        self.identifiers
            .entry(identifier)
            .or_insert(Vec::new())
            .push(value);
    }
    fn bind_static_identifier(&mut self, identifier: String, value: GenericValue) {
        self.static_identifiers
            .entry(identifier)
            .or_insert(Vec::new())
            .push(value);
    }

    fn call_callable(&mut self, argument: i64, operands: &Expressions) -> Result<i64, AnyError> {
        let callable = operands.first().unwrap();
        let mut evaluated_arguments = vec![argument];
        for operand in &operands[1..] {
            evaluated_arguments.push(self.evaluate_recursive(operand)?);
        }
        match callable.syn_type() {
            Expression::Identifier(function_name) => {
                let function_ptr = self.get_identifier(function_name)?;
                self.call_function_pointer(&evaluated_arguments, function_ptr)
                    .map_err(|err| {
                        format!(
                            "Bug: Identifier '{}' is not a valid function. Details: {}",
                            function_name, err
                        )
                        .into()
                    })
            }
            Expression::Function(function) => self.call_function_expression(
                &evaluated_arguments,
                function,
                &Closure::new_from_current_scope(&self.identifiers),
            ),
            Expression::Composed(Composed::Loop(Loop { body })) => self.call_loop_expression(body),
            Expression::Composed(Composed::Browse(Browse {
                iteration_elem,
                body,
            })) => self.call_browse_expression(argument, iteration_elem, body),
            Expression::Composed(Composed::BrowseOr(browse_or)) => {
                self.call_browse_or_expression(argument, browse_or)
            }
            Expression::Composed(Composed::Times(browse)) => {
                self.call_times_expression(argument, browse)
            }
            Expression::Composed(Composed::TimesOr(times_or)) => {
                self.call_times_or_expression(argument, times_or)
            }
            Expression::Composed(Composed::Replace(replace)) => {
                self.call_replace_expression(argument, replace)
            }
            Expression::Composed(Composed::Map(map)) => self.call_map_expression(argument, map),
            Expression::Composed(Composed::Filter(filter)) => {
                self.call_filter_expression(argument, filter)
            }
            Expression::Composed(Composed::Branch(branch)) => {
                self.evaluate_chain(if argument != 0 {
                    &branch.yes
                } else {
                    &branch.no
                })
            }
            Expression::Composed(Composed::Something(something)) => {
                self.call_something_expression(argument, something)
            }
            Expression::Composed(Composed::Inspect(inspect)) => {
                self.call_inspect_expression(argument, inspect)
            }
            Expression::Composed(Composed::Cast(_)) => Ok(argument),
            Expression::Chain(chain) => {
                let function_pointer = self.evaluate_chain(chain)?;
                self.call_function_pointer(&evaluated_arguments, function_pointer)
            }
            Expression::Nothing
            | Expression::Value(_)
            | Expression::Type(_)
            | Expression::StaticList { .. } => err(format!(
                "Can not use expression as a function: {:?}",
                callable
            ))?,
        }
    }

    fn call_function_pointer(
        &mut self,
        arguments: &[i64],
        function_ptr: GenericValue,
    ) -> Result<i64, AnyError> {
        if let Some(function_expr) = self.functions.get(function_ptr as usize).cloned() {
            match function_expr.as_ref() {
                FunctionOrIntrinsic::Function(function, closure) => {
                    self.call_function_expression(arguments, function, closure)
                }
                FunctionOrIntrinsic::Intrinsic(intrinsic) => {
                    self.call_intrinsic(arguments, *intrinsic)
                }
            }
        } else {
            err(format!("invalid function pointer {}", function_ptr))
        }
    }
    fn call_function_expression(
        &mut self,
        arguments: &[i64],
        Function {
            parameters, body, ..
        }: &Function,
        closure: &Closure,
    ) -> Result<i64, AnyError> {
        let mut identifiers_inside = closure.clone().to_identifiers();
        std::mem::swap(&mut self.identifiers, &mut identifiers_inside);
        for (parameter, argument) in parameters.iter().zip(arguments.iter()) {
            self.bind_identifier(parameter.name.clone(), *argument);
        }

        let result = self.evaluate_chain_passing_value(
            arguments.get(0).cloned().unwrap_or(NOTHING),
            &builtin_types::UNKNOWN,
            body,
        )?;

        std::mem::swap(&mut self.identifiers, &mut identifiers_inside);
        Ok(result)
    }
    fn call_loop_expression(&mut self, body: &Chain) -> Result<i64, AnyError> {
        loop {
            let result = self.evaluate_chain(&body)?;
            if result != NOTHING {
                return Ok(result);
            }
        }
    }
    fn call_browse_expression(
        &mut self,
        argument: i64,
        iteration_elem: &TypedIdentifier,
        body: &Chain,
    ) -> Result<i64, AnyError> {
        let list = self.get_list(argument)?.clone(); // TODO remove clone?
        let default_result = NOTHING;
        for value in list {
            self.bind_identifier(iteration_elem.name.clone(), value);
            let result = self.evaluate_chain(&body)?;
            // let result = self.evaluate_chain_passing_value(value, &builtin_types::UNKNOWN, &body)?;
            self.unbind_identifier(&iteration_elem.name, 1)?;
            if result != NOTHING {
                return Ok(result);
            }
        }
        Ok(default_result)
    }
    fn call_browse_or_expression(
        &mut self,
        argument: i64,
        BrowseOr {
            iteration_elem,
            body,
            otherwise,
        }: &BrowseOr,
    ) -> Result<i64, AnyError> {
        let result = self.call_browse_expression(argument, iteration_elem, body)?;
        if result == NOTHING {
            self.evaluate_chain(otherwise)
        } else {
            Ok(result)
        }
    }
    fn call_times_expression(
        &mut self,
        argument: i64,
        Times {
            iteration_elem,
            body,
        }: &Times,
    ) -> Result<i64, AnyError> {
        for value in 0..argument {
            self.bind_identifier(iteration_elem.name.clone(), value);
            let _unused_result = self.evaluate_chain(&body)?;
            self.unbind_identifier(&iteration_elem.name, 1)?;
        }
        Ok(argument)
    }

    fn call_times_or_expression(
        &mut self,
        argument: i64,
        TimesOr {
            iteration_elem,
            body,
            otherwise,
        }: &TimesOr,
    ) -> Result<i64, AnyError> {
        for value in 0..argument {
            self.bind_identifier(iteration_elem.name.clone(), value);
            let result = self.evaluate_chain(&body)?;
            self.unbind_identifier(&iteration_elem.name, 1)?;
            if result != NOTHING {
                return Ok(result);
            }
        }
        self.evaluate_chain(otherwise)
    }

    fn call_replace_expression(
        &mut self,
        argument: i64,
        Replace {
            iteration_elem,
            body,
        }: &Replace,
    ) -> Result<i64, AnyError> {
        let mut list = self.get_list(argument)?.clone(); // TODO remove clone?
        for value in &mut list {
            self.bind_identifier(iteration_elem.name.clone(), *value);
            *value = self.evaluate_chain(&body)?;
            self.unbind_identifier(&iteration_elem.name, 1)?;
        }
        self.lists.insert(argument, list);
        Ok(argument)
    }

    fn call_map_expression(
        &mut self,
        argument: i64,
        Map {
            iteration_elem,
            body,
        }: &Map,
    ) -> Result<i64, AnyError> {
        let mut list = self.get_list(argument)?.clone(); // TODO remove clone?
        let mut new_list = Vec::new();
        for value in &mut list {
            self.bind_identifier(iteration_elem.name.clone(), *value);
            new_list.push(self.evaluate_chain(&body)?);
            self.unbind_identifier(&iteration_elem.name, 1)?;
        }
        Ok(self.allocate_list(new_list))
    }
    fn call_filter_expression(
        &mut self,
        argument: i64,
        Filter {
            iteration_elem,
            body,
        }: &Filter,
    ) -> Result<i64, AnyError> {
        let mut list = self.get_list(argument)?.clone(); // TODO remove clone?
        let mut new_list = Vec::new();
        for value in &mut list {
            self.bind_identifier(iteration_elem.name.clone(), *value);
            if self.evaluate_chain(&body)? != 0 {
                new_list.push(*value);
            }
            self.unbind_identifier(&iteration_elem.name, 1)?;
        }
        Ok(self.allocate_list(new_list))
    }

    fn call_something_expression(
        &mut self,
        argument: i64,
        Something {
            elem,
            something,
            nothing,
        }: &Something,
    ) -> Result<i64, AnyError> {
        if argument != NOTHING {
            self.bind_identifier(elem.name.clone(), argument);
            let result = self.evaluate_chain(&something)?;
            self.unbind_identifier(&elem.name, 1)?;
            Ok(result)
        } else {
            Ok(self.evaluate_chain(&nothing)?)
        }
    }

    fn call_inspect_expression(
        &mut self,
        argument: i64,
        Inspect { elem, body }: &Inspect,
    ) -> Result<i64, AnyError> {
        self.bind_identifier(elem.name.clone(), argument);
        self.evaluate_chain(&body)?;
        self.unbind_identifier(&elem.name, 1)?;
        Ok(argument)
    }

    fn call_intrinsic(
        &mut self,
        arguments: &[i64],
        intrinsic: Intrinsic,
    ) -> Result<GenericValue, AnyError> {
        let argument = arguments[0];
        match intrinsic {
            Intrinsic::PrintChar => {
                write!(self.print_output, "{}", argument as u8 as char)?;
                Ok(argument)
            }
            Intrinsic::ReadChar => {
                let one_byte_buffer: &mut [u8] = &mut [0; 1];
                self.read_input.read_exact(one_byte_buffer)?;
                Ok(one_byte_buffer[0] as GenericValue)
            }
            Intrinsic::Print => {
                match self.lists.get(&argument) {
                    Some(list) => {
                        let s = String::from_utf8(list.iter().map(|b| *b as u8).collect())?;
                        writeln!(self.print_output, "{}", s)?;
                    }
                    None => err(format!(
                        "\"print\" was called with an invalid array {}",
                        argument
                    ))?,
                }
                Ok(argument)
            }
            Intrinsic::ReadLines => {
                let mut all_lines = "".to_string();
                self.read_input.read_to_string(&mut all_lines)?;
                let mut list_of_lists: Vec<GenericValue> = all_lines
                    .split("\n")
                    .map(|str_line| {
                        self.allocate_list(str_line.bytes().map(|x| x as i64).collect())
                    })
                    .collect();

                if let Some(last) = list_of_lists.last() {
                    if self.get_list(*last).unwrap().len() == 0 {
                        list_of_lists.pop();
                    }
                }
                let list_ptr = self.allocate_list(list_of_lists);
                Ok(list_ptr)
            }
            Intrinsic::ToStr => {
                let string = format!("{}", argument);
                let bytes = string.bytes().map(|b| b as i64).collect();
                Ok(self.allocate_list(bytes))
            }
            Intrinsic::NewArray => {
                let mut new_vec = Vec::<GenericValue>::new();
                new_vec.resize(argument as usize, 0);
                Ok(self.allocate_list(new_vec))
            }
            Intrinsic::Size => {
                let list = self.get_list(argument)?;
                Ok(list.len() as GenericValue)
            }
            Intrinsic::Breakpoint => {
                self.debugger_prompt(arguments)?;
                Ok(argument)
            }
        }
    }

    fn get_list_element(
        &mut self,
        list_pointer: ListPointer,
        operand: &ExpressionSpan,
    ) -> Result<GenericValue, AnyError> {
        let index = self.evaluate_recursive(operand)?;
        let list = self.lists.get(&list_pointer).ok_or_else(|| {
            Into::<AnyError>::into(format!(
                "Attempted accessing element '{}' of array '{}' \
                         which is not a valid array pointer",
                index, list_pointer,
            ))
        })?;
        list.get(index as usize).cloned().ok_or_else(|| {
            format!(
                "Index out of bounds. Index: {}, list ({} elements): {:?}",
                index,
                list.len(),
                list
            )
            .into()
        })
    }
    fn evaluate_assignment(
        &mut self,
        accumulated: GenericValue,
        operand: &ExpressionSpan,
        identifiers: &mut HashMap<String, usize>,
    ) -> Result<(), AnyError> {
        match operand.syn_type() {
            Expression::Identifier(name) => {
                self.bind_identifier(name.clone(), accumulated);
                *identifiers.entry(name.clone()).or_insert(0) += 1;
                Ok(())
            }
            _ => err(format!(
                "Can only assign to identifiers, not to a {:?}",
                operand
            )),
        }
    }
    fn evaluate_overwrite(
        &mut self,
        accumulated: GenericValue,
        operand: &ExpressionSpan,
    ) -> Result<(), AnyError> {
        match operand.syn_type() {
            Expression::Identifier(name) => {
                if let Some(value) = self.identifiers.get_mut(name) {
                    *value.last_mut().unwrap() = accumulated;
                    Ok(())
                } else {
                    err(format!("Can not overwrite identifier {} because it has not been defined. Use '=' to define it.", name))
                }
            }
            _ => err(format!(
                "Can only overwrite identifiers, not a {:?}",
                operand
            )),
        }
    }
    fn evaluate_compare(
        &mut self,
        accumulated: GenericValue,
        operator: Comparison,
        operand: &ExpressionSpan,
    ) -> Result<GenericValue, AnyError> {
        let value = self.evaluate_recursive(operand)?;
        let compared = match operator {
            Comparison::Equals => accumulated == value,
            Comparison::Different => accumulated != value,
            Comparison::LessThan => accumulated < value,
            Comparison::GreaterThan => accumulated > value,
            Comparison::LessThanEquals => accumulated <= value,
            Comparison::GreaterThanEquals => accumulated >= value,
        };
        Ok(compared as i64)
    }

    fn evaluate_field(
        &mut self,
        accumulated: GenericValue,
        accumulated_sem_type: &Type,
        operand: &ExpressionSpan,
    ) -> Result<GenericValue, AnyError> {
        if let Type::Simple { .. } = accumulated_sem_type {
            let expanded = Expand::expand(
                accumulated_sem_type,
                &RuntimeTypeView::new(&self.types, &self._sources),
                NO_SPAN,
            )?;
            let accumulated_sem_type = &expanded;
            self.evaluate_field_expanded(accumulated, accumulated_sem_type, operand)
        } else {
            self.evaluate_field_expanded(accumulated, accumulated_sem_type, operand)
        }
    }

    fn evaluate_field_expanded(
        &mut self,
        accumulated: GenericValue,
        accumulated_sem_type: &Type,
        operand: &ExpressionSpan,
    ) -> Result<GenericValue, AnyError> {
        let err = |message| err(format!("{} at unknown file {}", message, operand.span));
        if let Type::Nested { children, .. } = accumulated_sem_type {
            if let ExpressionSpan {
                syntactic_type: Expression::Identifier(name),
                ..
            } = operand
            {
                if let Ok(list) = self.get_list(accumulated) {
                    let index = get_field_index(accumulated_sem_type, children, name)?;
                    if let Some(elem) = list.get(index) {
                        Ok(*elem)
                    } else {
                        err(format!("Bug: accumulated value {} is a pointer to a tuple with only {} fields. \
                            Not enough to access its field {}.{} with field index {}",
                                    accumulated, list.len(), accumulated_sem_type, name, index))
                    }
                } else {
                    err(format!(
                        "Bug: accumulated value {} is not a pointer to a tuple. \
                        Required to access its field {}.{}",
                        accumulated, accumulated_sem_type, name
                    ))
                }
            } else {
                err(format!(
                    "Bug: the field operator '{}' can only be followed by a field identifier, \
                    but was '{}'",
                    FIELD as char, operand
                ))
            }
        } else {
            err(format!(
                "Bug: field access '{}{}' can only be applied to nested types, \
                but appears after a '{}'",
                FIELD as char, operand, accumulated_sem_type
            ))
        }
    }

    fn evaluate_concatenate(
        &mut self,
        accumulated: GenericValue,
        operands: &Expressions,
    ) -> Result<ListPointer, AnyError> {
        let first_elems = self.get_list(accumulated)?;
        let mut new_list = first_elems.clone();
        for (i, operand) in operands.iter().enumerate() {
            let second_pointer = match operand.syn_type() {
                Expression::StaticList { elements } => self.evaluate_list(&elements),
                Expression::Identifier(name) => self.get_identifier(name),
                Expression::Chain(chain) => self.evaluate_chain(chain),
                _ => err(format!(
                    "Expected to concatenate two or more lists, operand #{} is {:?}",
                    i + 1,
                    operand,
                )),
            }?;
            let second_elems = self.get_list(second_pointer)?;
            new_list.append(&mut second_elems.clone());
        }
        Ok(self.allocate_list(new_list))
    }

    fn evaluate_list(&mut self, elements: &Expressions) -> Result<ListPointer, AnyError> {
        let mut to_allocate = Vec::with_capacity(elements.len());
        for e in elements {
            to_allocate.push(self.evaluate_recursive(e)?);
        }
        Ok(self.allocate_list(to_allocate))
    }

    fn allocate_list(&mut self, to_allocate: Vec<GenericValue>) -> ListPointer {
        let new_pointer = self.lists.len() as i64;
        self.lists.insert(new_pointer, to_allocate);
        new_pointer
    }

    fn allocate_function(
        &mut self,
        function: Function,
        closure: Closure,
    ) -> Result<FunctionPointer, AnyError> {
        self.functions
            .push(Rc::new(FunctionOrIntrinsic::Function(function, closure)));
        Ok((self.functions.len() - 1) as i64)
    }
}

fn get_field_index(
    accumulated_sem_type: &Type,
    children: &TypedIdentifiers,
    name: &String,
) -> Result<usize, AnyError> {
    for (i, child) in children.iter().enumerate() {
        if child.name == *name {
            return Ok(i);
        }
    }
    err(format!(
        "Bug: field '{}' is not present in type '{}'",
        name, accumulated_sem_type
    ))
}

pub struct RuntimeTypeView<'a> {
    typed_identifiers: &'a HashMap<String, Vec<Type>>,
    _sources: &'a Sources,
}

impl<'a> RuntimeTypeView<'a> {
    pub fn new(typed_identifiers: &'a HashMap<String, Vec<Type>>, _sources: &'a Sources) -> Self {
        Self {
            typed_identifiers,
            _sources,
        }
    }
}

impl<'a> TypeView for RuntimeTypeView<'a> {
    fn get_type(&self, name: &str, span: Span) -> Result<&Type, AnyError> {
        if let Some(types_stack) = self.typed_identifiers.get(name) {
            if let Some(type_) = types_stack.last() {
                Ok(type_)
            } else {
                let message = format!(
                    "Bug: Definition of type '{}' exists but there are no bindings. \
                Current bindings: {:?}\nType usage{}",
                    name,
                    self.typed_identifiers,
                    maybe_format_span(self.get_source(name), span)
                );
                err(message)
            }
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
        None // TODO
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use crate::backend::evaluate::*;
    use crate::common::{assert_mentions, unwrap_display};
    use crate::frontend::lex_and_parse;
    use crate::frontend::sources::location::SourceCode;
    use crate::middleend::typing::put_types;

    fn interpret<S: Into<SourceCode>>(code_text: S) -> GenericValue {
        let mut program = unwrap_display(lex_and_parse(code_text));
        unwrap_display(put_types(&mut program));
        // for ident in &program.identifiers {
        //     println!("{:#?}", ident);
        // }
        // println!("{:#?}", program.main);
        let result = Runtime::evaluate(program, std::io::stdin(), std::io::stdout());
        unwrap_display(result)
    }
    fn interpret_fallible(code_text: &str) -> Result<GenericValue, AnyError> {
        let expression = lex_and_parse(code_text)?;
        let result = Runtime::evaluate(expression, std::io::stdin(), std::io::stdout());
        result
    }
    fn interpret_io(code_text: &str, read_input: &str) -> (GenericValue, String) {
        let read_input = read_input.bytes().collect::<Vec<u8>>();
        let print_output = Vec::<u8>::new();
        let mut program = unwrap_display(lex_and_parse(code_text));
        unwrap_display(put_types(&mut program));
        let (main, identifiers, sources) = program.take();
        let mut runtime = unwrap_display(Runtime::new(
            read_input.as_slice(),
            print_output,
            identifiers,
            sources,
        ));
        let result = runtime.evaluate_recursive(&main);
        (
            result.unwrap(),
            String::from_utf8(runtime.print_output).unwrap(),
        )
    }

    #[test]
    fn test_addition() {
        assert_eq!(interpret("22 + 5 + 1000"), 1027);
    }

    #[test]
    fn test_substraction() {
        assert_eq!(interpret("22 - 5 + 1000 - 10"), 1007);
    }

    #[test]
    fn test_arithmetic() {
        assert_eq!(interpret("3 =ident ;5 +7 |*12 |/ident -4 %20"), 4);
    }

    #[test]
    fn test_ignore() {
        assert_eq!(interpret("22 + 5 ; 1000"), 1000);
    }

    #[test]
    fn test_get_element() {
        assert_eq!(interpret("[5 6 7] #1"), 6);
        assert_eq!(interpret("[5 6 7] #{1+1}"), 7);
    }
    #[test]
    fn test_get_non_existing_element() {
        let err = interpret_fallible("[5 6 7] #3").expect_err("should have failed");
        assert_mentions(err, &["index", "3", "out of bounds"])
    }
    #[test]
    fn test_get_on_non_array() {
        let err = interpret_fallible("4 #3").expect_err("should have failed");
        assert_mentions(err, &["array", "4"])
    }
    #[test]
    fn test_nested_array_operations() {
        assert_eq!(interpret("[{5 - 6} 7] #0"), -1);
    }
    #[test]
    fn test_evaluate_nothing() {
        assert_eq!(interpret("{}"), NOTHING);
    }
    #[test]
    fn test_function() {
        assert_eq!(interpret("5 |function(x) {x}"), 5);
        assert_eq!(interpret("5 |function(x) {+1}"), 6);
        let err = interpret_fallible("4|3").expect_err("should have failed");
        assert_mentions(err, &["as a function", "3"])
    }
    #[test]
    fn test_function_parameters() {
        assert_eq!(
            interpret("5 |function(x  y :i64  z) {x |*10 +y |*10 +z} 6 7"),
            567
        );
    }
    #[test]
    fn test_function_closure() {
        assert_eq!(
            interpret(
                r#"
        3 |{
            5 |function(x){
                function(y){
                    x-y
                }
            }
        }"#
            ),
            2
        );
    }
    #[test]
    fn test_function_callsite_closure() {
        let _ = interpret_fallible("
            function (x) { asdf }
            =test_grab_context
            ;5=asdf
            ;0|test_grab_context")
            .expect_err("should fail because identifiers in the call site should not be accessible to the function body");
    }
    #[test]
    fn test_pass_function() {
        assert_eq!(
            interpret("function(x) {x +1} | function(increment) {6 |increment}"),
            7
        );
    }
    #[test]
    fn test_branch() {
        assert_eq!(interpret("1 |branch {5} {7}"), 5);
        assert_eq!(interpret("0 |branch {5} {7}"), 7);
    }

    #[test]
    #[ignore] // for now
    fn test_pass_branch() {
        assert_eq!(interpret("branch {5} {7} |function(f) {0 |f}"), 7)
    }

    #[test]
    fn test_name_function() {
        assert_eq!(interpret("function(x) {x +1} =increment ;6 |increment"), 7);
    }
    #[test]
    fn test_overwrite() {
        assert_eq!(interpret("4 =i +1 =>i"), 5);
    }
    #[test]
    fn test_duplicate_definition() {
        let _ = interpret_fallible("4 =i +1 =i").expect_err("should fail: duplicate definition");
    }
    #[test]
    fn test_deshadow_identifier() {
        let code = "
            function (x) { x + 1 }
            =increment
            |function(inc) {
                function(x) { x - 1 }
                =increment // this is really decrement, but shadows the outer increment
                ;5
                |increment // really, decrement to 4
            }
            |increment // this should be the real increment, so put back to 5
            ";
        assert_eq!(interpret(code), 5);
    }
    #[test]
    fn test_deshadow_parameter() {
        let code = "
            5
            |function(x) {
                8
                |function(x) { x + 1 }
                ;x
            }";
        assert_eq!(interpret(code), 5);
    }
    #[test]
    fn test_intrinsics() {
        let mut out = Vec::<u8>::new();
        let into = "72".as_bytes();
        let expression = lex_and_parse("'5' |print_char; 0|read_char").unwrap();
        let result = Runtime::evaluate(expression, into, &mut out);
        assert_eq!(result.unwrap() as u8, '7' as u8);
        assert_eq!(out, "5".as_bytes());
    }
    #[test]
    fn test_read_lines() {
        let (result, print_output) = interpret_io(
            "0 |read_lines |function(lines) {lines #1 |print |size |to_str |print;lines |size}",
            &"asdf\nqwer\nzxcv\n",
        );
        assert_eq!(result, 3);
        assert_eq!(print_output, "qwer\n4\n");
    }
    #[test]
    fn test_to_str() {
        let result = interpret("123 |to_str #1");
        assert_eq!(result, b'2' as i64);
    }
    #[test]
    fn test_print_wrong_array() {
        let err = interpret_fallible("0|print").expect_err("should have failed");
        assert_mentions(err, &["print", "invalid array"])
    }
    #[test]
    fn test_pass_intrinsics() {
        let code = " print_char |function(f) { 5 +48 |f }";
        let (result, print_output) = interpret_io(code, "");
        assert_eq!(result, 53);
        assert_eq!(print_output, "5");
    }
    #[test]
    fn test_deshadow_intrinsics() {
        let code = "
            {
                function(x) {x+1}
                =print_char // shadowing print_char with an increment function
                ;5 + 48
                |print_char // 53 -> 54
            }
            |print_char     // print 54: '6'
            ";
        let (result, print_output) = interpret_io(code, "");
        assert_eq!(result, 54);
        assert_eq!(print_output, "6");
    }

    #[test]
    fn test_wrong_assignment() {
        let err = interpret_fallible("4+1=5").expect_err("should have failed");
        assert_mentions(err, &["assign", "5"]);
    }
    #[test]
    fn test_concat() {
        assert_eq!(interpret("[10 11] ++[12 13] #2"), 12);
        assert_eq!(interpret("[10 11] |function(list) {list ++[12 13] #2}"), 12);
        assert_eq!(interpret("[10 11] |function(list) {[12 13] ++list #2}"), 10);
        assert_eq!(interpret("[10 11] ++{[12] ++[13]} #2"), 12);
    }
    #[test]
    fn test_concat_several() {
        assert_eq!(interpret("[10 11] ++[12 13] [14] [15 16] #6"), 16);
    }

    #[test]
    fn test_loop() {
        let (_result, print_output) = interpret_io("{}|loop {\" \"|print}", "");
        assert_eq!(print_output, " \n")
    }
    #[test]
    fn test_loop_3_times() {
        let (result, print_output) = interpret_io(
            "0 =i;{}|loop {i +1 =>i |inspect(x) {x |to_str |print} =? 3 |branch {i}{}} :i64",
            "",
        );
        assert_eq!(result, 3);
        assert_eq!(print_output, "1\n2\n3\n")
    }
    #[test]
    fn test_browse() {
        let (result, print_output) = interpret_io("[10 11] |browse(n :i64) {n|to_str |print;}", "");
        assert_eq!(result, NOTHING);
        assert_eq!(print_output, "10\n11\n");
        let (result, print_output) = interpret_io("[10 11] |browse(n :i64) {|to_str |print;}", "");
        assert_eq!(result, NOTHING);
        assert_eq!(print_output, "10\n11\n");
    }

    #[test]
    fn test_browse_broken() {
        let (result, print_output) =
            interpret_io("[10 11] |browse(n :i64) {n |to_str |print; 5}", "");
        assert_eq!(result, 5);
        assert_eq!(print_output, "10\n")
    }

    #[test]
    fn test_browse_or() {
        let result = interpret("[10 11] |browse_or(n :i64) {n;} {5}");
        assert_eq!(result, 5);
    }

    #[test]
    fn test_browse_or_broken() {
        let result = interpret("[10 11] |browse_or(n :i64) {n} {5}");
        assert_eq!(result, 10);
        let result = interpret("[10 11] |browse_or(n :i64) {} {5}");
        assert_eq!(result, 10);
    }

    #[test]
    fn test_times() {
        let result = interpret("0=n ;4 |times (i) {n +10 => n}");
        assert_eq!(result, 4);
    }
    #[test]
    fn test_times_or() {
        let result = interpret("0=n ;4 |times_or(i) {n +10 =>n;} {5} +n");
        assert_eq!(result, 45);
    }
    #[test]
    fn test_times_or_broken() {
        let result = interpret("0=n ;4 |times_or(i) {n +10 =>n ;100} {5} +n");
        assert_eq!(result, 110);
        let result = interpret("0=n ;4 |times_or(i) {} {5}");
        assert_eq!(result, 0);
    }

    #[test]
    fn test_replace() {
        assert_eq!(interpret("[10 11] |replace(e :i64) {e +100} #1"), 111);
        assert_eq!(
            interpret("[10 11] =initial |replace(e :i64) {e +100}; initial #1"),
            111
        );
        assert_eq!(
            interpret("[10 11] =initial |replace(e :i64) {+100}; initial #1"),
            111
        );
    }
    #[test]
    fn test_map() {
        assert_eq!(interpret("[10 11] =initial |map(e) {e +100} #1"), 111);
        assert_eq!(
            interpret("[10 11] =initial |map(e) {e +100} ;initial #1"),
            11
        );
        assert_eq!(interpret("[10 11] =initial |map(e) {+100} #1"), 11);
        assert_eq!(
            interpret(
                "public tuple(x:i64 y:i64) =coords ;[{[3 4] |cast(:coords)}] |map(c) {c .x} #0"
            ),
            3
        );
    }
    #[test]
    fn test_filter() {
        assert_eq!(interpret("[ 1 2 3 4 5 6] |filter(e) {|/2 |*2 ==e} #2"), 6);
    }
    #[test]
    fn test_comparison() {
        assert_eq!(interpret("5 <9"), 1);
        assert_eq!(interpret("5 <4"), 0);
        assert_eq!(interpret("5 <5"), 0);

        assert_eq!(interpret("5 <=9"), 1);
        assert_eq!(interpret("5 <=4"), 0);
        assert_eq!(interpret("5 <=5"), 1);

        assert_eq!(interpret("5 >9"), 0);
        assert_eq!(interpret("5 >4"), 1);
        assert_eq!(interpret("5 >5"), 0);

        assert_eq!(interpret("5 >=9"), 0);
        assert_eq!(interpret("5 >=4"), 1);
        assert_eq!(interpret("5 >=5"), 1);

        assert_eq!(interpret("5 =?9"), 0);
        assert_eq!(interpret("5 =?4"), 0);
        assert_eq!(interpret("5 =?5"), 1);

        assert_eq!(interpret("5 ==9"), 0);
        assert_eq!(interpret("5 ==4"), 0);
        assert_eq!(interpret("5 ==5"), 1);

        assert_eq!(interpret("5 !=9"), 1);
        assert_eq!(interpret("5 !=4"), 1);
        assert_eq!(interpret("5 !=5"), 0);
    }

    #[test]
    fn test_complex_comparisons() {
        assert_eq!(interpret("5 =?{5}"), 1);
    }

    #[test]
    fn test_evaluate_import() {
        let main_path = PathBuf::from("./pipes_programs/demos/reusing_functions.pipes");
        let code = SourceCode::new(main_path).unwrap();
        assert_eq!(interpret(code), 6);
    }
    #[test]
    fn test_evaluate_import_fileless() {
        assert_eq!(
            interpret("5 |pipes_programs/demos/some_namespace/reusable_functions/add_1"),
            6
        );
    }
    #[test]
    fn test_nested_constants() {
        assert_eq!(interpret("public 4 =A ;public {A +1} =B ;B"), 5);
        assert_eq!(
            interpret("public 4 =A ;public {A +1} =B ;public {B +1} =C ;C"),
            6
        );
    }

    #[test]
    fn test_something() {
        assert_eq!(interpret("0 |branch{3}{} |something(n) {n} {5}"), 5);
        assert_eq!(interpret("1 |branch{3}{} |something(n) {n} {5}"), 3);
        assert_eq!(interpret("1 |branch{3}{} |something(n) {} {5}"), 3);
    }
    #[test]
    fn test_inspect() {
        assert_eq!(interpret("3 |inspect(n) {n+1}"), 3);
        assert_eq!(
            interpret_io("3 |inspect(n) {+1 |to_str |print}", ""),
            (3, "4".to_string())
        );
    }

    #[test]
    fn test_field_access() {
        assert_eq!(
            interpret("public tuple(x :i64 y :i64) =Coords; [1 2] |cast(:Coords) .y"),
            2
        );
        assert_eq!(
            interpret("public tuple(x :i64 y :i64) =Coords; function {[1 2] |cast(:Coords)} =f; {} |function {0 |branch {{} |f .x} {{} |f .y}}"),
            2
        );
        assert_eq!(
            interpret("public tuple(x :i64 y :i64) =Coords; public function {[1 2] |cast(:Coords)} =f; {} |public function {0 |branch {{} |f .x} {{} |f .y}} =g"),
            2
        );
    }
}
