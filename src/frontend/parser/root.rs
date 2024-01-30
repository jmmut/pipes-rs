use crate::common::{err, AnyError};
use std::path::PathBuf;

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
        let current_file_abs_copy = current_file_abs.clone();
        let mut root_opt = None;
        while current_file_abs.pop() {
            current_file_abs.push(PIPES_ROOT_FILENAME);
            let exists = current_file_abs.exists();
            current_file_abs.pop();
            if exists {
                root_opt = Some(current_file_abs);
                break;
            }
        }
        if let Some(root) = root_opt {
            Ok(root)
        } else {
            err(format!(
                "File '{}' not found in a parent folder from '{}'. Needed to import identifiers",
                PIPES_ROOT_FILENAME,
                current_file_abs_copy.to_string_lossy()
            ))
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
    let mut file_copy = file.clone();
    file_copy.set_extension("");
    let namespace = file_copy.strip_prefix(root)?;
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
        let containing_file = Some(PathBuf::from("parent/project/subfolder/some_file.pipes"));
        let root = Some(PathBuf::from("parent/project/"));
        let namespaced_name = maybe_qualify(bare_name, &containing_file, &root);
        assert_eq!(
            namespaced_name.unwrap(),
            format!("{}/{}", "subfolder/some_file", bare_name)
        );
    }

    // TODO: test don't change already namespaced name
}
