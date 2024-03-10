use std::collections::hash_map::Keys;
use std::collections::HashMap;

use crate::frontend::sources::location::SourceCode;

pub mod lexer;
pub mod location;
pub mod token;

#[derive(Debug, Clone)]
pub struct Sources {
    main_source: SourceCode,
    other_sources: HashMap<String, SourceCode>,
}

impl Default for Sources {
    fn default() -> Self {
        Self {
            main_source: SourceCode::new_fileless("".to_string()),
            other_sources: HashMap::new(),
        }
    }
}
impl Sources {
    pub fn new(main_source: SourceCode, other_sources: HashMap<String, SourceCode>) -> Self {
        Self {
            main_source,
            other_sources,
        }
    }

    pub fn take_all(
        mut self,
        name_for_main_source: String,
    ) -> impl IntoIterator<Item = (String, SourceCode)> {
        self.other_sources
            .insert(name_for_main_source, self.main_source);
        self.other_sources.into_iter()
    }
    pub fn get(&self, path: &str) -> Option<&SourceCode> {
        self.other_sources.get(path)
    }
    pub fn get_main(&self) -> &SourceCode {
        &self.main_source
    }
    pub fn keys(&self) -> Keys<String, SourceCode> {
        self.other_sources.keys()
    }
}
