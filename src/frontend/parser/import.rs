use crate::common::{context, AnyError};
use crate::evaluate::intrinsics;
use crate::frontend::expression::{
    Branch, Chain, Expression, Function, Loop, LoopOr, Map, Replace, Times, Transformation, Type,
    TypedIdentifier,
};
use crate::frontend::parser::reverse_iterative_parser::Parser;
use crate::frontend::program::Program;
use std::collections::HashMap;
use std::path::PathBuf;

pub fn import(main: Expression, parser: Parser) -> Result<Program, AnyError> {
    let mut import_state = ImportState::new();
    let (main, identifiers_opt) = if parser.unresolved_identifiers.is_empty() {
        (main, parser.identifiers)
    } else {
        let _ = context(
            "Import",
            track_identifiers_recursive(&main, &mut import_state),
        )?;
        (main, parser.identifiers)
    };
    let identifiers = identifiers_opt
        .into_iter()
        .filter_map(|(name, opt_expr)| opt_expr.map(|expr| (name, expr)))
        .collect();
    Ok(Program { main, identifiers })
}

struct ImportState {
    parameter_stack: Vec<String>,
    intrinsic_names: Vec<String>,
    imported: HashMap<String, Expression>,
    project_root: Option<PathBuf>,
}

impl ImportState {
    pub fn new() -> Self {
        Self {
            parameter_stack: Vec::new(),
            intrinsic_names: intrinsics::INTRINSICS
                .iter()
                .map(|i| i.name().to_string())
                .collect(),

            imported: HashMap::new(),
            project_root: None,
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
    type_: &Type,
    import_state: &mut ImportState,
) -> Result<(), AnyError> {
    todo!()
}

fn check_identifier(identifier: &String, import_state: &mut ImportState) -> Result<(), AnyError> {
    if !import_state.parameter_stack.contains(identifier)
        && !import_state.intrinsic_names.contains(identifier)
        && !import_state.imported.contains_key(identifier)
    {
        let imported = import_identifier(identifier, import_state)?;
        import_state.imported.insert(identifier.clone(), imported);
    }

    Ok(())
}

fn import_identifier(
    identifier: &String,
    import_state: &mut ImportState,
) -> Result<Expression, AnyError> {
    let root = get_project_root(import_state)?;
    let paths = identifier.split('/').collect::<Vec<_>>();
    if paths.len() < 2 {
        Err(format!("undefined identifier '{}'", identifier).into())
    } else {
        unimplemented!()
    }
}

fn get_project_root(import_state: &mut ImportState) -> Result<PathBuf, AnyError> {
    if let Some(root) = &import_state.project_root {
        Ok(root.clone())
    } else {
        unimplemented!()
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
    for Transformation { operator, operand } in &chain.transformations {
        track_identifiers_recursive(operand, import_state)?;
    }
    Ok(())
}
