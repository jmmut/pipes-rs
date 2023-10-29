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
    pub type_name: String,
    pub value_name: Option<String>,
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
    pub fn simple(type_name: String) -> Type {
        Type {
            type_name,
            value_name: None,
            children_types: ChildrenTypes::None,
        }
    }
    pub fn child(type_name: String, child: Box<Type>) -> Type {
        Type {
            type_name,
            value_name: None,
            children_types: ChildrenTypes::Single(child),
        }
    }
    pub fn children(type_name: String, children: Vec<Type>) -> Type {
        Type {
            type_name,
            value_name: None,
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
