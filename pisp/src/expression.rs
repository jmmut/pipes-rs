use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub struct Expression {
    pub elements: Vec<Expression>,
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.elements)
    }
}

