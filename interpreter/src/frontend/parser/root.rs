use std::path::PathBuf;

use crate::common::{context, AnyError};
use crate::frontend::parser::import::get_corelib_path;

const PIPES_ROOT_FILENAME: &'static str = "pipes_root.toml";

pub fn get_project_root(
    root: &Option<PathBuf>,
    current_file: &Option<PathBuf>,
) -> Result<PathBuf, AnyError> {
    if let Some(root) = root {
        Ok(root.clone())
    } else {
        let mut current_file_abs = current_file.clone().unwrap_or(PathBuf::from("."));
        current_file_abs = current_file_abs.canonicalize()?;
        let mut current_file_abs_copy = current_file_abs.clone();
        let mut root_opt = None;
        loop {
            current_file_abs.push(PIPES_ROOT_FILENAME);
            let exists = current_file_abs.exists();
            current_file_abs.pop();
            if exists {
                root_opt = Some(current_file_abs);
                break;
            }
            if !current_file_abs.pop() {
                break;
            }
        }
        if let Some(root) = root_opt {
            Ok(root)
        } else {
            if current_file_abs_copy.is_file() {
                current_file_abs_copy.pop();
            }
            Ok(current_file_abs_copy)
        }
    }
}

/// Both PathBuf parameters must be pre-canonicalized, or have enough parent folders to cover the
/// project root.
#[allow(unused)]
pub fn maybe_qualify(
    bare_name: &str,
    containing_file: &Option<PathBuf>,
    root_folder: &Option<PathBuf>,
) -> Result<String, AnyError> {
    Ok(match (containing_file, root_folder) {
        (Some(file), Some(root)) => qualify(bare_name, root, file)?,
        (Some(file), None) => {
            let namespace = file.file_stem().unwrap();
            format!("{}/{}", namespace.to_string_lossy(), bare_name)
        }
        (None, Some(_)) | (None, None) => bare_name.to_string(),
    })
}

pub fn qualify(identifier: &str, root: &PathBuf, file: &PathBuf) -> Result<String, AnyError> {
    let mut file_copy = if file.is_absolute() {
        file.clone()
    } else {
        context(
            format!("canonicalizing path '{}'", file.to_string_lossy()),
            file.canonicalize(),
        )?
    };
    file_copy.set_extension("");
    let root_canonical = context(
        format!("canonicalizing path '{}'", file.to_string_lossy()),
        root.canonicalize(),
    )?;
    let namespace_res = file_copy.strip_prefix(root_canonical).or_else(|_e| {
        let corelib_canonical = get_corelib_path();
        file_copy.strip_prefix(corelib_canonical)
    });
    let namespace = context(
        format!("stripping prefix from {:?}", file_copy),
        namespace_res,
    )?;
    let namespace_str = namespace.to_string_lossy();
    let qualified = format!("{}/{}", namespace_str, identifier);
    Ok(qualified)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add_namespace_no_file_no_root() {
        let bare_name = "func";
        let containing_file = None;
        let root = None;
        let namespaced_name = maybe_qualify(bare_name, &containing_file, &root);
        assert_eq!(namespaced_name.unwrap(), bare_name);
    }

    #[test]
    fn test_add_namespace_with_file_no_root() {
        let bare_name = "func";
        let containing_file = Some(PathBuf::from("some_folder/some_file.pipes"));
        let root = None;
        let namespaced_name = maybe_qualify(bare_name, &containing_file, &root);
        assert_eq!(
            namespaced_name.unwrap(),
            format!("{}/{}", "some_file", bare_name)
        );
    }

    #[test]
    fn test_add_namespace_no_file_with_root() {
        let bare_name = "func";
        let containing_file = None;
        let root = Some(PathBuf::from("some_folder/project/"));
        let namespaced_name = maybe_qualify(bare_name, &containing_file, &root);
        assert_eq!(namespaced_name.unwrap(), bare_name);
    }

    #[test]
    fn test_add_namespace_with_file_with_root() {
        let bare_name = "func";
        let containing_file = Some(PathBuf::from(
            "../pipes_programs/demos/some_namespace/reusable_functions.pipes",
        ));
        let root = Some(PathBuf::from("../pipes_programs/demos"));
        let namespaced_name = maybe_qualify(bare_name, &containing_file, &root);
        assert_eq!(
            namespaced_name.unwrap(),
            format!("{}/{}", "some_namespace/reusable_functions", bare_name)
        );
    }

    // TODO: test don't change already namespaced name
}
