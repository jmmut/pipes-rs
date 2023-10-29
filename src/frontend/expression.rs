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
    StaticList {
        elements: Expressions,
    },
}

// pub type Identifier = String;

#[derive(PartialEq, Debug)]
pub enum Type {
    Simple {
        type_name: String,
    },
    NestedSingle {
        type_name: String,
        child: Box<Type>,
    },
    NestedSeveral {
        type_name: String,
        children: Vec<Type>,
    },
}

// #[allow(unused)]
// #[derive(PartialEq, Debug)]
// pub enum ChildrenTypes {
//     None,
//     Single(Box<Type>),
//     Several(Vec<Type>),
// }

#[allow(unused)]
impl Type {
    pub fn simple(type_name: String) -> Type {
        Type::Simple { type_name }
    }
    pub fn child(type_name: String, child: Box<Type>) -> Type {
        Type::NestedSingle { type_name, child }
    }
    pub fn children(type_name: String, children: Vec<Type>) -> Type {
        Type::NestedSeveral {
            type_name,
            children,
        }
    }
    pub fn from(parent: String, mut children: Vec<Type>) -> Type {
        if children.is_empty() {
            Type::simple(parent)
        } else if children.len() == 1 {
            Type::child(parent, Box::new(children.pop().unwrap()))
        } else {
            Type::children(parent, children)
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct Transformation {
    pub operator: Operator,
    pub operand: Expression, // TODO: list of expressions?
}

pub type Expressions = Vec<Expression>;
pub type Transformations = Vec<Transformation>;
