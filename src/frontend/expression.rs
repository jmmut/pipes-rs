use crate::frontend::lexer::Operator;

#[derive(PartialEq, Debug, Clone)]
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
    Chain(Chain),
    StaticList { elements: Expressions },
    Function(Function),
    Branch(Branch),
}

impl Expression {
    pub fn empty_chain() -> Self {
        Self::Chain(Chain::empty())
    }
    #[allow(unused)]
    pub fn chain(initial: Box<Expression>, transformations: Transformations) -> Self {
        Self::Chain(Chain {
            initial,
            transformations,
        })
    }
    #[allow(unused)]
    pub fn function(parameter: TypedIdentifier, body: Chain) -> Self {
        Self::Function(Function { parameter, body })
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Type {
    Unknown,
    // Any,
    Builtin {
        type_name: &'static str,
    },
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
    // Function?
}

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
    pub fn nothing() -> Type {
        builtin_types::NOTHING
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

#[derive(PartialEq, Debug, Clone)]
pub struct Chain {
    pub initial: Box<Expression>,
    pub transformations: Transformations,
    // pub type_: Type,
    // pub identifiers
}

impl Chain {
    pub fn new(initial: Box<Expression>, transformations: Transformations) -> Self {
        Self {
            initial,
            transformations,
        }
    }
    pub fn empty() -> Self {
        Self::new(Box::new(Expression::Nothing), Vec::new())
    }
}
#[derive(PartialEq, Debug, Clone)]
pub struct Transformation {
    pub operator: Operator,
    pub operand: Expression, // TODO: list of expressions?
}

#[derive(PartialEq, Debug, Clone)]
pub struct Function {
    pub parameter: TypedIdentifier, // TODO: Vec<TypedIdentifier> ?
    pub body: Chain,
}

#[derive(PartialEq, Debug, Clone)]
pub struct TypedIdentifier {
    pub name: String,
    pub type_: Type,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Branch {
    yes: Chain,
    no: Chain,
}
pub type Expressions = Vec<Expression>;
pub type Transformations = Vec<Transformation>;

pub mod type_names {
    #[allow(unused)]
    pub const I64: &'static str = "i64";
    pub const FUNCTION: &'static str = "function";
    #[allow(unused)]
    pub const TUPLE: &'static str = "tuple";
    #[allow(unused)]
    pub const ARRAY: &'static str = "array";
    #[allow(unused)]
    pub const STRUCT: &'static str = "struct";
    #[allow(unused)]
    pub const TYPE: &'static str = "type";
}

pub mod builtin_types {
    use crate::frontend::expression::Type;

    pub const NOTHING: Type = Type::Builtin {
        type_name: "Nothing",
    };
    #[allow(unused)]
    pub const I64: Type = Type::Builtin { type_name: "i64" };
}
