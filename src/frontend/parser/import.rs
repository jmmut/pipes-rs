use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

use strum::IntoEnumIterator;

use crate::common::{context, err, AnyError};
use crate::frontend::expression::{
    Branch, Browse, BrowseOr, Chain, Composed, Expression, ExpressionSpan, Function, Inspect, Loop,
    Map, Operation, Replace, Something, Times, TimesOr, Type, TypedIdentifier,
};
use crate::frontend::parser::reverse_iterative_parser::{parse_tokens_cached_inner, Parser};
use crate::frontend::parser::root::qualify;
use crate::frontend::sources::lexer::lex;
use crate::frontend::sources::location::SourceCode;
use crate::frontend::sources::token::{Operator, OperatorSpan};
use crate::middleend::intrinsics;

/// Adds imported identifiers to the parser.identifiers parameter
pub fn import(
    main: &mut ExpressionSpan,
    // main: &mut ExpressionSpan,
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
            track_identifiers_recursive(public_exported, &mut import_state),
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
    match expression.syn_type_mut() {
        Expression::Nothing => Ok(()),
        Expression::Value(_) => Ok(()),
        Expression::Identifier(name) => check_identifier(name, import_state),
        Expression::Type(type_) => track_identifiers_recursive_type(type_, import_state),
        Expression::Chain(chain) => track_identifiers_recursive_chain(import_state, chain),
        Expression::StaticList { elements } => {
            for element in elements {
                track_identifiers_recursive(element, import_state)?;
            }
            Ok(())
        }
        Expression::Function(Function {
            parameters, body, ..
        }) => track_identifiers_recursive_scope(import_state, parameters.iter(), body),
        Expression::Composed(Composed::Loop(Loop { body })) => {
            track_identifiers_recursive_scope(import_state, [].iter(), body)
        }
        Expression::Composed(Composed::Browse(Browse {
            iteration_elem,
            body,
        })) => track_identifiers_recursive_scope(
            import_state,
            vec![&*iteration_elem].into_iter(),
            body,
        ),
        Expression::Composed(Composed::BrowseOr(BrowseOr {
            iteration_elem,
            body,
            otherwise,
        })) => {
            track_identifiers_recursive_scope(
                import_state,
                vec![&*iteration_elem].into_iter(),
                body,
            )?;
            track_identifiers_recursive_chain(import_state, otherwise)
        }
        Expression::Composed(Composed::Times(Times {
            iteration_elem,
            body,
        })) => track_identifiers_recursive_scope(
            import_state,
            vec![&*iteration_elem].into_iter(),
            body,
        ),
        Expression::Composed(Composed::TimesOr(TimesOr {
            iteration_elem,
            body,
            otherwise,
        })) => {
            track_identifiers_recursive_scope(
                import_state,
                vec![&*iteration_elem].into_iter(),
                body,
            )?;
            track_identifiers_recursive_chain(import_state, otherwise)
        }
        Expression::Composed(Composed::Replace(Replace {
            iteration_elem,
            body,
        })) => track_identifiers_recursive_scope(
            import_state,
            vec![&*iteration_elem].into_iter(),
            body,
        ),
        Expression::Composed(Composed::Map(Map {
            iteration_elem,
            body,
        })) => track_identifiers_recursive_scope(
            import_state,
            vec![&*iteration_elem].into_iter(),
            body,
        ),
        Expression::Composed(Composed::Branch(Branch { yes, no })) => {
            track_identifiers_recursive_chain(import_state, yes)?;
            track_identifiers_recursive_chain(import_state, no)
        }
        Expression::Composed(Composed::Something(Something {
            elem,
            something,
            nothing,
        })) => {
            track_identifiers_recursive_scope(import_state, vec![&*elem].into_iter(), something)?;
            track_identifiers_recursive_chain(import_state, nothing)
        }
        Expression::Composed(Composed::Inspect(Inspect { elem, body })) => {
            track_identifiers_recursive_scope(import_state, vec![&*elem].into_iter(), body)
        }
        Expression::Composed(Composed::Cast(_)) => Ok(()), // TODO: might need to import user-defined types
    }
}

fn track_identifiers_recursive_type(
    _type_: &Type,
    _import_state: &mut ImportState,
) -> Result<(), AnyError> {
    //todo!()
    Ok(())
}

fn check_identifier(
    identifier: &mut String,
    import_state: &mut ImportState,
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
            import_identifier(identifier, import_state)?;
            if !import_state.imported.contains_key(identifier)
                && !import_state.available.contains(identifier)
            {
                let mut imported = import_state.imported.keys().collect::<Vec<_>>();
                imported.sort_unstable();
                let mut available = import_state.available.iter().collect::<Vec<_>>();
                available.sort_unstable();
                err(format!(
                    "identifier '{}' not found in scope for file {:?}. Available:\n  Parameters: {:?}\n  Intrinsics: {:?}\n  \
                        Assignments: {:?}\n  Available to this file: {:?}\n  Imported by this file: {:?}",
                    identifier,
                    import_state.source.file,
                    import_state.parameter_stack,
                    import_state.intrinsic_names,
                    import_state.assignments,
                    available,
                    imported
                ))
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
) -> Result<(), AnyError> {
    // let root = import_state.project_root?;
    let mut paths = identifier
        .split('/')
        .map(|s| s.to_string())
        .collect::<Vec<_>>();
    if paths.len() < 2 {
        if let (Some(root), Some(file)) = (
            import_state.project_root.as_ref(),
            import_state.source.file.as_ref(),
        ) {
            // assuming the un-qualified name was defined in the same file as where the identifier was used.
            // no need to import in that case
            *identifier = qualify(identifier, root, file)?;
            Ok(())
        } else {
            err(format!("undefined identifier '{}'", identifier))
        }
    } else {
        let _function_name = paths.pop().unwrap();
        let imported_path = PathBuf::from_iter(paths.into_iter());
        // let mut path_to_import = root.join(imported_path);
        let mut relative_path_to_import = imported_path.clone();
        relative_path_to_import.set_extension("pipes");
        let path_to_import = if let Some(mut root) = import_state.project_root.clone() {
            root.push(&relative_path_to_import);
            root
        } else {
            relative_path_to_import.clone()
        };
        let source_code = SourceCode::new(path_to_import.clone())?;
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

fn track_identifiers_recursive_scope<'a, T: Iterator<Item = &'a TypedIdentifier>>(
    import_state: &mut ImportState,
    parameters: T,
    body: &mut Chain,
) -> Result<(), AnyError> {
    let mut params = 0;
    for parameter in parameters {
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
    if let Some(initial) = &mut chain.initial {
        track_identifiers_recursive(initial.as_mut(), import_state)?;
    }
    let mut identifiers_defined_in_this_chain = Vec::new();
    for Operation {
        operator,
        operands,
        sem_type,
    } in &mut chain.operations
    {
        let operand = operands.get_mut(0);
        if let Some(operand) = operand {
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
                Expression::Identifier(name),
            ) = (operator, operand.syn_type().clone())
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
