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

pub fn add_namespace(
    bare_name: &str,
    containing_file: &Option<String>,
    root: &Option<String>,
) -> String {
    bare_name.to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add_namespace_no_file_no_root() {
        let bare_name = "func";
        let containing_file = None;
        let root = None;
        let namespaced_name = add_namespace(bare_name, &containing_file, &root);
        assert_eq!(namespaced_name, bare_name);
    }

    #[test]
    fn test_add_namespace_with_file_no_root() {
        let bare_name = "func";
        let containing_file = Some("some_folder/some_file.pipes".to_string());
        let root = None;
        let namespaced_name = add_namespace(bare_name, &containing_file, &root);
        assert_eq!(namespaced_name, bare_name);
    }
    //
    // #[test]
    // fn test_add_namespace_with_file_no_root() {
    //     let bare_name = "func";
    //     let containing_file = Some("some_folder/some_file.pipes".to_string());
    //     let root = None;
    //     let namespaced_name = add_namespace(bare_name, &containing_file, &root);
    //     assert_eq!(namespaced_name, format!("{}/{}", "some_folder/some_file", bare_name));
    // }
}
