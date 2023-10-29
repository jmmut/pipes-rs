use crate::frontend::lexer::Operator;

#[derive(PartialEq, Debug)]
pub enum Expression {
    Nothing,
    Value(i64),
    Identifier(String),
    // Operator {
    //     operator: Operator,
    // },
    // Operation(Transformation),
    // Transformations {
    //     transformations: Transformations,
    // },
    Chain {
        initial: Box<Expression>,
        transformations: Transformations,
    },
    StaticList(StaticList),
}

// pub type Identifier = String;

#[derive(PartialEq, Debug)]
pub struct Transformation {
    pub operator: Operator,
    pub operand: Expression, // TODO: list of expressions?
}

#[derive(PartialEq, Debug)]
pub struct StaticList {
    pub elements: Expressions,
}

pub type Expressions = Vec<Expression>;
pub type Transformations = Vec<Transformation>;
