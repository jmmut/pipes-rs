use crate::frontend::lexer::Operator;

#[derive(PartialEq, Debug)]
pub enum Expression {
    Nothing,
    Value(i64),
    Identifier(String),
    Type(Type),
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
pub struct Type {
    pub name: String,
    pub children_types: ChildrenTypes,
}

#[allow(unused)]
#[derive(PartialEq, Debug)]
pub enum ChildrenTypes {
    None,
    Single(Box<Type>),
    Several(Vec<Type>),
}

#[allow(unused)]
impl Type {
    pub fn simple(name: String) -> Type {
        Type {
            name,
            children_types: ChildrenTypes::None,
        }
    }
    pub fn child(name: String, child: Box<Type>) -> Type {
        Type {
            name,
            children_types: ChildrenTypes::Single(child),
        }
    }
    pub fn children(name: String, children: Vec<Type>) -> Type {
        Type {
            name,
            children_types: ChildrenTypes::Several(children),
        }
    }
}

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
