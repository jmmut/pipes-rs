use std::collections::{HashMap, HashSet};

use crate::frontend::expression::{Expression, ExpressionSpan};
use crate::frontend::location::SourceCode;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub main: ExpressionSpan,
    pub identifiers: HashMap<String, ExpressionSpan>,
    pub main_source: SourceCode,
    pub sources: HashMap<String, SourceCode>,
}

impl Program {
    pub fn new_from(
        main: ExpressionSpan,
        identifiers: HashMap<String, ExpressionSpan>,
        main_source: SourceCode,
        sources: HashMap<String, SourceCode>,
    ) -> Self {
        Self {
            main,
            identifiers,
            main_source,
            sources,
        }
    }

    pub fn new(expression: ExpressionSpan) -> Self {
        Self {
            main: expression,
            identifiers: HashMap::new(),
            main_source: SourceCode::new_fileless("".to_string()),
            sources: HashMap::new(),
        }
    }
    pub fn new_raw(expression: Expression) -> Self {
        Self::new(ExpressionSpan::new_spanless(expression))
    }
    pub fn main(&self) -> &ExpressionSpan {
        &self.main
    }
    pub fn take(
        self,
    ) -> (
        ExpressionSpan,
        HashMap<String, ExpressionSpan>,
        SourceCode,
        HashMap<String, SourceCode>,
    ) {
        (self.main, self.identifiers, self.main_source, self.sources)
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
    pub main: ExpressionSpan,
    pub exported: HashMap<String, ExpressionSpan>,
    pub available: HashSet<String>,
    pub main_source: SourceCode,
    pub sources: HashMap<String, SourceCode>,
}
