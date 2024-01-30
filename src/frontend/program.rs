use std::collections::{HashMap, HashSet};

use crate::frontend::expression::Expression;

#[derive(PartialEq, Debug, Clone)]
pub struct Program {
    pub main: Expression,
    pub identifiers: HashMap<String, Expression>,
}

impl Program {
    pub fn new(expression: Expression) -> Self {
        Self {
            main: expression,
            identifiers: HashMap::new(),
        }
    }
}

impl From<IncompleteProgram> for Program {
    fn from(incomplete: IncompleteProgram) -> Self {
        Self {
            main: incomplete.main,
            identifiers: incomplete.exported,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct IncompleteProgram {
    pub main: Expression,
    pub exported: HashMap<String, Expression>,
    pub available: HashSet<String>,
}
