use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

use strum::IntoEnumIterator;

use crate::common::{context, err, AnyError};
use crate::frontend::expression::{
    Branch, Browse, BrowseOr, Chain, Composed, Comptime, Expression, ExpressionSpan, Filter,
    Function, Inspect, Loop, Map, Operation, Replace, Something, Times, TimesOr, Type, TypeName,
    TypedIdentifier,
};
use crate::frontend::parser::reverse_iterative_parser::{parse_tokens_cached_inner, Parser};
use crate::frontend::parser::root::qualify;
use crate::frontend::sources::lexer::lex;
use crate::frontend::sources::location::{SourceCode, Span};
use crate::frontend::sources::token::{Operator, OperatorSpan};
use crate::middleend::intrinsics;
use crate::middleend::intrinsics::is_builtin_type;

const CORELIB_PATH: &'static str = ".local/share/pipes/core/";
const PIPES_EXTENSION: &'static str = "pipes";

/// Adds imported identifiers to the parser.identifiers parameter
pub fn import(
    main: &mut ExpressionSpan,
    parser: &mut Parser,
) -> Result<(HashMap<String, ExpressionSpan>, HashMap<String, SourceCode>), AnyError> {
    let context_str = if let Some(path) = parser.source.file.as_ref() {
        format!("Import dependencies for {}", path.to_string_lossy())
    } else {
        "Import".to_string()
    };
    let mut tmp_source_code = SourceCode::new_fileless("".to_string());
    std::mem::swap(&mut tmp_source_code, &mut parser.source);
    let mut import_state = ImportState::new(tmp_source_code, parser.root.clone());
    import_state.available = parser.exported.keys().cloned().collect();
    import_state
        .available
        .extend(parser.available.iter().cloned());
    for (_name, public_exported) in &mut parser.exported {
        let _ = context(
            context_str.clone(),
            track_identifiers_recursive(public_exported, &mut import_state), // TODO: why this?
        )?;
    }
    let _ = context(
        context_str.clone(),
        track_identifiers_recursive(main, &mut import_state),
    )?;
    parser.source = import_state.source;
    // println!("after importing: {:#?}", import_state.imported);
    Ok((import_state.imported, import_state.other_sources))
}

struct ImportState {
    parameter_stack: Vec<String>,
    intrinsic_names: Vec<String>,
    assignments: Vec<String>,
    imported: HashMap<String, ExpressionSpan>,
    available: HashSet<String>,
    project_root: Option<PathBuf>,
    source: SourceCode,
    other_sources: HashMap<String, SourceCode>,
}

impl ImportState {
    pub fn new(source: SourceCode, root: Option<PathBuf>) -> Self {
        Self::new_with_identifiers(source, root, HashMap::new())
    }
    fn new_with_identifiers(
        source: SourceCode,
        project_root: Option<PathBuf>,
        imported: HashMap<String, ExpressionSpan>,
    ) -> Self {
        Self {
            parameter_stack: Vec::new(),
            intrinsic_names: intrinsics::Intrinsic::iter()
                .map(|i| i.name().to_string())
                .collect(),
            assignments: Vec::new(),
            imported,
            available: HashSet::new(),
            project_root,
            source,
            other_sources: HashMap::new(),
        }
    }
}
fn track_identifiers_recursive(
    expression: &mut ExpressionSpan,
    import_state: &mut ImportState,
) -> Result<(), AnyError> {
    let span = expression.span;
    match expression.syn_type_mut() {
        Expression::Nothing => Ok(()),
        Expression::Value(_) => Ok(()),
        Expression::Identifier(name) => check_identifier(name, import_state, span),
        Expression::Type(type_) => track_identifiers_recursive_type(type_, import_state, span),
        Expression::Chain(chain) => track_identifiers_recursive_chain(import_state, chain),
        Expression::StaticList { elements } => {
            for element in elements {
                track_identifiers_recursive(element, import_state)?;
            }
            Ok(())
        }
        Expression::Function(Function {
            parameters,
            returned,
            body,
        }) => {
            track_identifiers_recursive_scope(import_state, parameters.iter_mut(), body, span)?;
            check_user_defined_type(&mut returned.type_, import_state, span)
        }
        Expression::Composed(Composed::Loop(Loop { body })) => {
            track_identifiers_recursive_scope(import_state, [].iter_mut(), body, span)
        }
        Expression::Composed(Composed::Browse(Browse {
            iteration_elem,
            body,
        })) => track_identifiers_recursive_scope(
            import_state,
            vec![iteration_elem].into_iter(),
            body,
            span,
        ),
        Expression::Composed(Composed::BrowseOr(BrowseOr {
            iteration_elem,
            body,
            otherwise,
        })) => {
            track_identifiers_recursive_scope(
                import_state,
                vec![iteration_elem].into_iter(),
                body,
                span,
            )?;
            track_identifiers_recursive_chain(import_state, otherwise)
        }
        Expression::Composed(Composed::Times(Times {
            iteration_elem,
            body,
        })) => track_identifiers_recursive_scope(
            import_state,
            vec![iteration_elem].into_iter(),
            body,
            span,
        ),
        Expression::Composed(Composed::TimesOr(TimesOr {
            iteration_elem,
            body,
            otherwise,
        })) => {
            track_identifiers_recursive_scope(
                import_state,
                vec![iteration_elem].into_iter(),
                body,
                span,
            )?;
            track_identifiers_recursive_chain(import_state, otherwise)
        }
        #[rustfmt::skip]
        Expression::Composed(Composed::Replace(Replace { iteration_elem, body}))
        | Expression::Composed(Composed::Map(Map { iteration_elem, body}))
        | Expression::Composed(Composed::Filter(Filter { iteration_elem, body})) => {
            track_identifiers_recursive_scope(
                import_state,
                vec![iteration_elem].into_iter(),
                body,
span,
            )
        },
        Expression::Composed(Composed::Branch(Branch { yes, no })) => {
            track_identifiers_recursive_chain(import_state, yes)?;
            track_identifiers_recursive_chain(import_state, no)
        }
        Expression::Composed(Composed::Something(Something {
            elem,
            something,
            nothing,
        })) => {
            track_identifiers_recursive_scope(
                import_state,
                vec![elem].into_iter(),
                something,
                span,
            )?;
            track_identifiers_recursive_chain(import_state, nothing)
        }
        Expression::Composed(Composed::Inspect(Inspect { elem, body })) => {
            track_identifiers_recursive_scope(import_state, vec![elem].into_iter(), body, span)
        }
        Expression::Composed(Composed::Cast(cast)) => {
            check_user_defined_type(&mut cast.target_type.type_, import_state, span)
        }
        Expression::Composed(Composed::Comptime(Comptime { body })) => {
            track_identifiers_recursive_chain(import_state, body)
        }
    }
}

fn track_identifiers_recursive_type(
    type_: &mut Type,
    import_state: &mut ImportState,
    span: Span,
) -> Result<(), AnyError> {
    check_user_defined_type(type_, import_state, span)
}

fn check_user_defined_type(
    type_: &mut Type,
    import_state: &mut ImportState,
    span: Span,
) -> Result<(), AnyError> {
    let mut cast_to = type_.name().to_string();
    if is_builtin_type(&cast_to).is_none() {
        let result = check_identifier(&mut cast_to, import_state, span);
        *type_ = Type::Simple {
            type_name: TypeName::UserDefined(cast_to),
        };
        result
    } else {
        Ok(())
    }
}

fn check_identifier(
    identifier: &mut String,
    import_state: &mut ImportState,
    span: Span,
) -> Result<(), AnyError> {
    if !import_state.parameter_stack.contains(identifier)
        && !import_state.intrinsic_names.contains(identifier)
    {
        let assignment_nested_count = import_state
            .assignments
            .iter()
            .filter(|e| *e == identifier)
            .count();
        if assignment_nested_count == 0
            && !import_state.imported.contains_key(identifier)
            && !import_state.available.contains(identifier)
        {
            import_identifier(identifier, import_state, span)?;
            if !import_state.imported.contains_key(identifier)
                && !import_state.available.contains(identifier)
            {
                err_undefined_identifier(identifier, import_state, span)
            } else {
                Ok(())
            }
        } else if assignment_nested_count == 1 {
            if let (Some(root), Some(file)) = (
                import_state.project_root.as_ref(),
                import_state.source.file.as_ref(),
            ) {
                let qualified = qualify(identifier, root, file)?;
                if import_state.imported.contains_key(&qualified) {
                    *identifier = qualified;
                }
            }
            Ok(())
        } else {
            Ok(())
        }
    } else {
        Ok(())
    }
}

/// Adds imported identifiers into import_state.imported
fn import_identifier(
    identifier: &mut String,
    import_state: &mut ImportState,
    span: Span,
) -> Result<(), AnyError> {
    let path_parts = identifier.bytes().filter(|c| *c == b'/').count();
    if path_parts < 1 {
        if let (Some(root), Some(file)) = (
            import_state.project_root.as_ref(),
            import_state.source.file.as_ref(),
        ) {
            // assuming the un-qualified name was defined in the same file as where the identifier was used.
            // no need to import in that case
            *identifier = qualify(identifier, root, file)?;
            Ok(())
        } else {
            err_undefined_identifier(identifier, import_state, span)
        }
    } else {
        #[cfg(not(unix))] // can't read files in wasm
        return err_undefined_identifier(identifier, import_state, span);

        #[cfg(unix)]
        {
            let (relative_path_to_import, source_code) =
                find_source_code(identifier, &import_state, span)?;
            let tokens = lex(source_code)?;
            let mut available: HashSet<String> = import_state.imported.keys().cloned().collect();
            available.extend(import_state.available.clone());
            let parser = Parser::new_with_available(
                tokens.source_code,
                available,
                import_state.project_root.clone(),
            );
            let mut program = parse_tokens_cached_inner(tokens.tokens, parser)?;
            import_state
                .imported
                .extend(std::mem::take(&mut program.exported));
            import_state.other_sources.extend(
                program
                    .sources
                    .take_all(relative_path_to_import.to_string_lossy().to_string()),
            );
            Ok(())
        }
    }
}

fn find_source_code(
    identifier: &str,
    import_state: &ImportState,
    span: Span,
) -> Result<(PathBuf, SourceCode), AnyError> {
    let relative_path_to_import = get_relative_path_to_import(identifier);
    let path_to_import = get_project_location(&relative_path_to_import, import_state);
    let source_code_res = SourceCode::new(path_to_import.clone());
    let source_code = match source_code_res {
        Ok(s) => s,
        Err(_e) => {
            let corelib_path = get_corelib_location(&relative_path_to_import);
            match SourceCode::new(corelib_path.clone()) {
                Ok(s) => s,
                Err(_e_core) => err_undefined_identifier_from_expected(
                    identifier,
                    &import_state,
                    span,
                    vec![path_to_import, corelib_path],
                )?,
            }
        }
    };
    Ok((relative_path_to_import, source_code))
}

fn get_relative_path_to_import(identifier: &str) -> PathBuf {
    let mut namespace_parts = identifier
        .split('/')
        .map(|s| s.to_string())
        .collect::<Vec<_>>();
    let _function_name = namespace_parts.pop().unwrap();
    let imported_path = PathBuf::from_iter(namespace_parts.into_iter());
    let mut relative_path_to_import = imported_path;
    relative_path_to_import.set_extension(PIPES_EXTENSION);
    relative_path_to_import
}

fn get_project_location(relative_path_to_import: &PathBuf, import_state: &ImportState) -> PathBuf {
    if let Some(mut root) = import_state.project_root.clone() {
        root.push(&relative_path_to_import);
        root
    } else {
        relative_path_to_import.clone()
    }
}

fn get_corelib_location(relative_path_to_import: &PathBuf) -> PathBuf {
    let mut corelib_path = get_corelib_path();
    corelib_path.push(&relative_path_to_import);
    corelib_path
}

pub fn get_corelib_path() -> PathBuf {
    let home = std::env::var("HOME").unwrap(); // TODO: support windows
    let corelib_path = PathBuf::from(format!("{}/{}", home, CORELIB_PATH));
    corelib_path
}

#[cfg(not(unix))] // can't read files in wasm
fn err_undefined_identifier<T>(
    identifier: &str,
    import_state: &ImportState,
    span: Span,
) -> Result<T, AnyError> {
    err_undefined_identifier_from_expected(identifier, import_state, span, vec![])
}
#[cfg(unix)]
fn err_undefined_identifier<T>(
    identifier: &str,
    import_state: &ImportState,
    span: Span,
) -> Result<T, AnyError> {
    let relative_path_to_import = get_relative_path_to_import(identifier);
    let path_to_import = get_project_location(&relative_path_to_import, import_state);
    let corelib_path = get_corelib_location(&relative_path_to_import);
    err_undefined_identifier_from_expected(
        identifier,
        import_state,
        span,
        vec![path_to_import, corelib_path],
    )
}
fn err_undefined_identifier_from_expected<T>(
    identifier: &str,
    import_state: &ImportState,
    span: Span,
    expected_location: Vec<PathBuf>,
) -> Result<T, AnyError> {
    let mut imported = import_state.imported.keys().collect::<Vec<_>>();
    imported.sort_unstable();
    let mut available = import_state.available.iter().collect::<Vec<_>>();
    available.sort_unstable();
    err(format!(
        "Undefined identifier '{}'{}{}\nAvailable identifiers:\n  Parameters: {:?}\n  Intrinsics: {:?}\n  \
                    Assignments: {:?}\n  Available to this file: {:?}\n  Imported by this file: {:?}",
        identifier,
        import_state.source.format_span(span),
        format!("Expected in any of: {:?}", expected_location),
        import_state.parameter_stack,
        import_state.intrinsic_names,
        import_state.assignments,
        available,
        imported
    ))
}

fn track_identifiers_recursive_scope<'a, T: Iterator<Item = &'a mut TypedIdentifier>>(
    import_state: &mut ImportState,
    parameters: T,
    body: &mut Chain,
    span: Span,
) -> Result<(), AnyError> {
    let mut params = 0;
    for parameter in parameters {
        check_user_defined_type(&mut parameter.type_, import_state, span)?;
        import_state.parameter_stack.push(parameter.name.clone());
        params += 1;
    }
    let res = track_identifiers_recursive_chain(import_state, body);
    for _ in 0..params {
        import_state.parameter_stack.pop();
    }
    res
}

fn track_identifiers_recursive_chain(
    import_state: &mut ImportState,
    chain: &mut Chain,
) -> Result<(), AnyError> {
    let mut identifiers_defined_in_this_chain = Vec::new();
    for Operation {
        operator, operands, ..
    } in &mut chain.operations
    {
        for operand in operands {
            if let (
                OperatorSpan {
                    operator: Operator::Assignment,
                    ..
                },
                Expression::Identifier(name),
            ) = (&operator, operand.syn_type().clone())
            {
                identifiers_defined_in_this_chain.push(name.clone());
                import_state.assignments.push(name)
            }
            if let (
                OperatorSpan {
                    operator: Operator::Field,
                    ..
                },
                Expression::Identifier(_name),
            ) = (&operator, operand.syn_type().clone())
            { // field access should not be imported
            } else {
                track_identifiers_recursive(operand, import_state)?;
            }
        }
    }

    // unregister assignments done by this chain
    for assignment in identifiers_defined_in_this_chain.into_iter().rev() {
        let mut index = import_state.assignments.len() - 1;
        for imported_assignment in import_state.assignments.iter().rev() {
            if assignment == *imported_assignment {
                import_state.assignments.swap_remove(index);
                break;
            }
            index -= 1;
        }
    }
    Ok(())
}
