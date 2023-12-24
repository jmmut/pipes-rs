use std::collections::HashMap;

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
