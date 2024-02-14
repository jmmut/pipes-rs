use std::collections::{HashMap, HashSet};

use crate::frontend::expression::{Expression, ExpressionSpan};
use crate::frontend::location::SourceCode;

#[derive(PartialEq, Debug, Clone)]
pub struct Program {
    main: ExpressionSpan,
    pub identifiers: HashMap<String, Expression>,
    pub main_source: SourceCode,
    pub sources: HashMap<String, SourceCode>,
}

impl Program {
    pub fn new(expression: Expression) -> Self {
        Self {
            main: ExpressionSpan::new_spanless(expression),
            identifiers: HashMap::new(),
            main_source: SourceCode::new_fileless("".to_string()),
            sources: HashMap::new(),
        }
    }
    pub fn main(&self) -> &Expression {
        self.main.syn_type()
    }
    pub fn take(
        self,
    ) -> (
        ExpressionSpan,
        HashMap<String, Expression>,
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
    pub exported: HashMap<String, Expression>,
    pub available: HashSet<String>,
    pub main_source: SourceCode,
    pub sources: HashMap<String, SourceCode>,
}
