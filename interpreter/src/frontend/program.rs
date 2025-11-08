use std::collections::HashMap;

use crate::frontend::expression::{Expression, ExpressionSpan};
use crate::frontend::sources::Sources;

pub type Identifiers = HashMap<String, ExpressionSpan>;

#[derive(Debug, Clone)]
pub struct Program {
    pub main: ExpressionSpan,
    pub identifiers: Identifiers,
    pub sources: Sources,
}

impl Program {
    pub fn new_from(main: ExpressionSpan, identifiers: Identifiers, sources: Sources) -> Self {
        Self {
            main,
            identifiers,
            sources,
        }
    }

    pub fn new(expression: ExpressionSpan) -> Self {
        Self {
            main: expression,
            identifiers: HashMap::new(),
            sources: Sources::default(),
        }
    }
    pub fn new_raw(expression: Expression) -> Self {
        Self::new(ExpressionSpan::new_spanless(expression))
    }
    pub fn main(&self) -> &ExpressionSpan {
        &self.main
    }
    pub fn take(self) -> (ExpressionSpan, Identifiers, Sources) {
        (self.main, self.identifiers, self.sources)
    }
}

impl From<IncompleteProgram> for Program {
    fn from(incomplete: IncompleteProgram) -> Self {
        Self {
            main: incomplete.main,
            identifiers: incomplete.exported,
            sources: incomplete.sources,
        }
    }
}

#[derive(Debug, Clone)]
pub struct IncompleteProgram {
    pub main: ExpressionSpan,
    pub exported: Identifiers,
    pub available: Identifiers,
    pub sources: Sources,
}
