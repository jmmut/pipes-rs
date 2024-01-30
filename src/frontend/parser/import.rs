use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use strum::IntoEnumIterator;

use crate::common::{context, err, AnyError};
use crate::evaluate::intrinsics;
use crate::frontend::expression::{
    Branch, Chain, Expression, Function, Loop, LoopOr, Map, Replace, Times, TimesOr,
    Transformation, Type, TypedIdentifier,
};
use crate::frontend::lexer::{lex, Operator};
use crate::frontend::location::SourceCode;
use crate::frontend::parser::reverse_iterative_parser::{
    parse_tokens_cached, parse_tokens_cached_inner, qualify, Parser,
};

/// Adds imported identifiers to the parser.identifiers parameter
pub fn import(
    main: &mut Expression,
    parser: &mut Parser,
) -> Result<HashMap<String, Expression>, AnyError> {
    let context_str = if let Some(path) = parser.file.as_ref() {
        format!("Import dependencies for {}", path.to_string_lossy())
    } else {
        "Import".to_string()
    };
    let mut import_state = ImportState::new(parser.file.clone(), parser.root.clone());
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
    // println!("after importing: {:#?}", import_state.imported);
    Ok(import_state.imported)
}

struct ImportState {
    parameter_stack: Vec<String>,
    intrinsic_names: Vec<String>,
    assignments: Vec<String>,
    imported: HashMap<String, Expression>,
    available: HashSet<String>,
    project_root: Option<PathBuf>,
    file: Option<PathBuf>,
}

impl ImportState {
    pub fn new(file: Option<PathBuf>, root: Option<PathBuf>) -> Self {
        Self::new_with_identifiers(file, root, HashMap::new())
    }
    fn new_with_identifiers(
        file: Option<PathBuf>,
        project_root: Option<PathBuf>,
        imported: HashMap<String, Expression>,
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
            file,
        }
    }
}
fn track_identifiers_recursive(
    expression: &mut Expression,
    import_state: &mut ImportState,
) -> Result<(), AnyError> {
    match expression {
        Expression::Nothing => Ok(()),
        Expression::Value(_) => Ok(()),
        Expression::Identifier(name) => check_identifier(name, import_state),
        Expression::Type(type_) => track_identifiers_recursive_type(type_, import_state),
        Expression::Chain(chain) => track_identifiers_recursive_chain(chain, import_state),
        Expression::StaticList { elements } => {
            for element in elements {
                track_identifiers_recursive(element, import_state)?;
            }
            Ok(())
        }
        Expression::Function(Function { parameter, body }) => {
            track_identifiers_recursive_scope(import_state, parameter, body)
        }
        Expression::Loop(Loop {
            iteration_elem,
            body,
        }) => track_identifiers_recursive_scope(import_state, iteration_elem, body),
        Expression::LoopOr(LoopOr {
            iteration_elem,
            body,
            otherwise,
        }) => {
            track_identifiers_recursive_scope(import_state, iteration_elem, body)?;
            track_identifiers_recursive_scope(import_state, iteration_elem, otherwise)
        }
        Expression::Times(Times {
            iteration_elem,
            body,
        }) => track_identifiers_recursive_scope(import_state, iteration_elem, body),
        Expression::TimesOr(TimesOr {
            iteration_elem,
            body,
            otherwise,
        }) => {
            track_identifiers_recursive_scope(import_state, iteration_elem, body)?;
            track_identifiers_recursive_scope(import_state, iteration_elem, otherwise)
        }
        Expression::Replace(Replace {
            iteration_elem,
            body,
        }) => track_identifiers_recursive_scope(import_state, iteration_elem, body),
        Expression::Map(Map {
            iteration_elem,
            body,
        }) => track_identifiers_recursive_scope(import_state, iteration_elem, body),
        Expression::Branch(Branch { yes, no }) => {
            track_identifiers_recursive_chain(yes, import_state)?;
            track_identifiers_recursive_chain(no, import_state)
        }
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
                err(format!(
                    "identifier '{}' not found in scope for file {:?}. Available:\n  Parameters: {:?}\n  Intrinsics: {:?}\n  \
                        Assignments: {:?}\n  Exported from this file: {:?}\n  Imported from other files: {:?}",
                    identifier,
                    import_state.file,
                    import_state.parameter_stack,
                    import_state.intrinsic_names,
                    import_state.assignments,
                    import_state.available,
                    import_state.imported.keys()
                ))
            } else {
                Ok(())
            }
        } else if assignment_nested_count == 1 {
            if let (Some(root), Some(file)) = (
                import_state.project_root.as_ref(),
                import_state.file.as_ref(),
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
            import_state.file.as_ref(),
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
        let mut path_to_import = imported_path.clone();
        path_to_import.set_extension("pipes");
        if let Some(mut root) = import_state.project_root.clone() {
            root.push(path_to_import);
            path_to_import = root;
        }
        let source_code = SourceCode::new(path_to_import)?;
        let file = source_code.file.clone();
        let tokens = lex(source_code)?;
        let mut available: HashSet<String> = import_state.imported.keys().cloned().collect();
        available.extend(import_state.available.clone());
        let parser = Parser::new_with_available(file, available, import_state.project_root.clone());
        let mut program = parse_tokens_cached_inner(tokens.tokens, parser)?;
        import_state
            .imported
            .extend(std::mem::take(&mut program.exported));
        Ok(())
    }
}

fn track_identifiers_recursive_scope(
    import_state: &mut ImportState,
    parameter: &TypedIdentifier,
    body: &mut Chain,
) -> Result<(), AnyError> {
    import_state.parameter_stack.push(parameter.name.clone());
    let res = track_identifiers_recursive_chain(body, import_state);
    import_state.parameter_stack.pop();
    res
}

fn track_identifiers_recursive_chain(
    chain: &mut Chain,
    import_state: &mut ImportState,
) -> Result<(), AnyError> {
    track_identifiers_recursive(chain.initial.as_mut(), import_state)?;
    let mut identifiers_defined_in_this_chain = Vec::new();
    for Transformation { operator, operand } in &mut chain.transformations {
        if let (Operator::Assignment, Expression::Identifier(name)) = (operator, operand.clone()) {
            identifiers_defined_in_this_chain.push(name.clone());
            import_state.assignments.push(name)
        }
        track_identifiers_recursive(operand, import_state)?;
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
