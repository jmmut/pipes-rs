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
    Loop(Loop),
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
    #[allow(unused)]
    pub fn loop_(elem: TypedIdentifier, body: Chain) -> Self {
        Self::Loop(Loop {
            iteration_elem: elem,
            body,
        })
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
        child: Box<TypedIdentifier>,
    },
    NestedSeveral {
        type_name: String,
        children: TypedIdentifiers,
    },
    // Function?
}

#[allow(unused)]
pub type Types = Vec<Type>;

#[allow(unused)]
impl Type {
    pub fn simple(type_name: String) -> Type {
        Type::Simple { type_name }
    }
    pub fn nameless_child(type_name: String, child: Box<Type>) -> Type {
        Type::NestedSingle {
            type_name,
            child: Box::new(TypedIdentifier::nameless(*child)),
        }
    }
    pub fn nameless_children(type_name: String, children: Vec<Type>) -> Type {
        Type::NestedSeveral {
            type_name,
            children: children
                .into_iter()
                .map(TypedIdentifier::nameless)
                .collect(),
        }
    }
    pub fn child(type_name: String, child: Box<TypedIdentifier>) -> Type {
        Type::NestedSingle { type_name, child }
    }
    pub fn children(type_name: String, children: TypedIdentifiers) -> Type {
        Type::NestedSeveral {
            type_name,
            children,
        }
    }
    pub fn nothing() -> Type {
        builtin_types::NOTHING
    }
    pub fn from_nameless(parent: String, mut children: Vec<Type>) -> Type {
        if children.is_empty() {
            Type::simple(parent)
        } else if children.len() == 1 {
            Type::nameless_child(parent, Box::new(children.pop().unwrap()))
        } else {
            Type::nameless_children(parent, children)
        }
    }
    pub fn from(parent: String, mut children: Vec<TypedIdentifier>) -> Type {
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

pub type TypedIdentifiers = Vec<TypedIdentifier>;

impl TypedIdentifier {
    pub fn nothing() -> Self {
        Self {
            name: "".to_string(),
            type_: Type::nothing(),
        }
    }
    pub fn unknown_type(name: String) -> Self {
        Self {
            name,
            type_: Type::Unknown,
        }
    }
    pub fn nameless(type_: Type) -> Self {
        Self {
            name: "".to_string(),
            type_,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Loop {
    pub iteration_elem: TypedIdentifier,
    pub body: Chain,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Branch {
    pub yes: Chain,
    pub no: Chain,
}

pub type Expressions = Vec<Expression>;
pub type Transformations = Vec<Transformation>;

pub mod keywords {
    pub const FUNCTION: &'static str = "function";
    pub const LOOP: &'static str = "loop";
    pub const BRANCH: &'static str = "branch";
}
pub mod type_names {
    #[allow(unused)]
    pub const I64: &'static str = "i64";
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
