use std::collections::HashMap;
use std::path::PathBuf;

use crate::common::{context, err, AnyError};
use crate::evaluate::intrinsics;
use crate::frontend::expression::{
    Branch, Chain, Expression, Function, Loop, LoopOr, Map, Replace, Times, Transformation, Type,
    TypedIdentifier,
};
use crate::frontend::lexer::{lex, Operator};
use crate::frontend::location::SourceCode;
use crate::frontend::parser::reverse_iterative_parser::{parse_tokens_cached, Parser};

/// Adds imported identifiers to the parser.identifiers parameter
pub fn import(main: &Expression, parser: &mut Parser) -> Result<(), AnyError> {
    let mut import_state = ImportState::new(parser.file.clone());
    std::mem::swap(&mut import_state.imported, &mut parser.exported);
    let context_str = if let Some(path) = parser.file.as_ref() {
        format!("Import dependencies for {}", path.to_string_lossy())
    } else {
        "Import".to_string()
    };
    let _ = context(
        context_str,
        track_identifiers_recursive(&main, &mut import_state),
    )?;
    std::mem::swap(&mut parser.exported, &mut import_state.imported);
    Ok(())
}

struct ImportState {
    parameter_stack: Vec<String>,
    intrinsic_names: Vec<String>,
    assignments: Vec<String>,
    imported: HashMap<String, Expression>,
    project_root: Option<PathBuf>,
    file: Option<PathBuf>,
}

impl ImportState {
    pub fn new(file: Option<PathBuf>) -> Self {
        Self::new_with_identifiers(file, HashMap::new())
    }
    pub fn new_with_identifiers(
        file: Option<PathBuf>,
        imported: HashMap<String, Expression>,
    ) -> Self {
        Self {
            parameter_stack: Vec::new(),
            intrinsic_names: intrinsics::INTRINSICS
                .iter()
                .map(|i| i.name().to_string())
                .collect(),
            assignments: Vec::new(),
            imported,
            project_root: None,
            file,
        }
    }
}
fn track_identifiers_recursive(
    expression: &Expression,
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

fn check_identifier(identifier: &String, import_state: &mut ImportState) -> Result<(), AnyError> {
    if !import_state.parameter_stack.contains(identifier)
        && !import_state.intrinsic_names.contains(identifier)
        && !import_state.assignments.contains(identifier)
        && !import_state.imported.contains_key(identifier)
    {
        import_identifier(identifier, import_state)?;
        if !import_state.imported.contains_key(identifier) {
            err(format!(
                "identifier '{}' not found. Available: {:?}",
                identifier,
                import_state.imported.keys()
            ))
        } else {
            Ok(())
        }
    } else {
        Ok(())
    }
}

/// Adds imported identifiers into import_state.imported
fn import_identifier(identifier: &String, import_state: &mut ImportState) -> Result<(), AnyError> {
    let root = get_project_root(import_state)?;
    let mut paths = identifier.split('/').collect::<Vec<_>>();
    if paths.len() < 2 {
        err(format!("undefined identifier '{}'", identifier))
    } else {
        let _function_name = paths.pop().unwrap();
        let imported_path = PathBuf::from_iter(paths.into_iter());
        let mut path_to_import = root.join(imported_path);
        path_to_import.set_extension("pipes");
        let source_code = SourceCode::new(path_to_import)?;
        let file = source_code.file.clone();
        let tokens = lex(source_code)?;
        let parser =
            Parser::new_with_exports(file, std::mem::take(&mut import_state.imported), Some(root));
        let mut program = parse_tokens_cached(tokens.tokens, parser)?;
        import_state.imported = std::mem::take(&mut program.identifiers);
        Ok(())
    }
}

const PIPES_ROOT_FILENAME: &'static str = "pipes_root.toml";

fn get_project_root(import_state: &mut ImportState) -> Result<PathBuf, AnyError> {
    if let Some(root) = &import_state.project_root {
        Ok(root.clone())
    } else if let Some(mut current_file) = import_state.file.clone() {
        current_file = current_file.canonicalize()?;
        let mut root_opt = None;
        while current_file.pop() {
            current_file.push(PIPES_ROOT_FILENAME);
            let exists = current_file.exists();
            current_file.pop();
            if exists {
                root_opt = Some(current_file);
                break;
            }
        }
        if let Some(root) = root_opt {
            Ok(root)
        } else {
            err(format!(
                "File '{}' not found in a parent folder from '{}'. Needed to import identifiers",
                PIPES_ROOT_FILENAME,
                import_state.file.as_ref().unwrap().to_string_lossy()
            ))
        }
    } else {
        Ok(PathBuf::from("."))
    }
}

fn track_identifiers_recursive_scope(
    import_state: &mut ImportState,
    parameter: &TypedIdentifier,
    body: &Chain,
) -> Result<(), AnyError> {
    import_state.parameter_stack.push(parameter.name.clone());
    let res = track_identifiers_recursive_chain(body, import_state);
    import_state.parameter_stack.pop();
    res
}

fn track_identifiers_recursive_chain(
    chain: &Chain,
    import_state: &mut ImportState,
) -> Result<(), AnyError> {
    track_identifiers_recursive(chain.initial.as_ref(), import_state)?;
    let mut identifiers_defined_in_this_chain = Vec::new();
    for Transformation { operator, operand } in &chain.transformations {
        if let (Operator::Assignment, Expression::Identifier(name)) = (operator, operand) {
            identifiers_defined_in_this_chain.push(name.clone());
            import_state.assignments.push(name.clone())
        }
        track_identifiers_recursive(operand, import_state)?;
    }
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
