use std::collections::{HashMap, HashSet};

use crate::frontend::expression::Expression;
use crate::frontend::location::SourceCode;

#[derive(PartialEq, Debug, Clone)]
pub struct Program {
    pub main: Expression,
    pub identifiers: HashMap<String, Expression>,
    pub main_source: SourceCode,
    pub sources: HashMap<String, SourceCode>,
}

impl Program {
    pub fn new(expression: Expression) -> Self {
        Self {
            main: expression,
            identifiers: HashMap::new(),
            main_source: SourceCode::new_fileless("".to_string()),
            sources: HashMap::new(),
        }
    }
}

impl From<IncompleteProgram> for Program {
    fn from(incomplete: IncompleteProgram) -> Self {
        Self {
            main: incomplete.main,
            identifiers: incomplete.exported,
            main_source: incomplete.main_source,
            sources: incomplete.sources,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct IncompleteProgram {
    pub main: Expression,
    pub exported: HashMap<String, Expression>,
    pub available: HashSet<String>,
    pub main_source: SourceCode,
    pub sources: HashMap<String, SourceCode>,
}
