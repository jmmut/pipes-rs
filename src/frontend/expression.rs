use crate::common::{err, AnyError};
use crate::frontend::lexer::Operator;
use crate::middleend::intrinsics::{builtin_types, is_builtin_type, BuiltinType};
use crate::middleend::typing::{is_builtin_nested_type, is_builtin_simple_type};
use strum::AsStaticRef;

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
    Composed(Composed),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Composed {
    Loop(Loop),
    LoopOr(LoopOr),
    Times(Times),
    TimesOr(TimesOr),
    Replace(Replace),
    Map(Map),
    Branch(Branch),
    Something(Something),
    Inspect(Inspect),
    Cast(Cast),
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
        Self::Composed(Composed::Loop(Loop {
            iteration_elem: elem,
            body,
        }))
    }
    #[allow(unused)]
    pub fn loop_or(elem: TypedIdentifier, body: Chain, otherwise: Chain) -> Self {
        Self::Composed(Composed::LoopOr(LoopOr {
            iteration_elem: elem,
            body,
            otherwise,
        }))
    }
    #[allow(unused)]
    pub fn times(elem: TypedIdentifier, body: Chain) -> Self {
        Self::Composed(Composed::Times(Times {
            iteration_elem: elem,
            body,
        }))
    }
    #[allow(unused)]
    pub fn times_or(elem: TypedIdentifier, body: Chain, otherwise: Chain) -> Self {
        Self::Composed(Composed::TimesOr(TimesOr {
            iteration_elem: elem,
            body,
            otherwise,
        }))
    }
    #[allow(unused)]
    pub fn replace(elem: TypedIdentifier, body: Chain) -> Self {
        Self::Composed(Composed::Replace(Replace {
            iteration_elem: elem,
            body,
        }))
    }
    #[allow(unused)]
    pub fn map(elem: TypedIdentifier, body: Chain) -> Self {
        Self::Composed(Composed::Map(Map {
            iteration_elem: elem,
            body,
        }))
    }
    #[allow(unused)]
    pub fn something(elem: TypedIdentifier, something: Chain, nothing: Chain) -> Self {
        Self::Composed(Composed::Something(Something {
            elem,
            something,
            nothing,
        }))
    }
    #[allow(unused)]
    pub fn inspect(elem: TypedIdentifier, body: Chain) -> Self {
        Self::Composed(Composed::Inspect(Inspect { elem, body }))
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum TypeName {
    // None,
    Builtin(&'static str),
    UserDefined(String),
}

impl TypeName {
    /// `S: Into<String> + AsRef<str>`: take a String by value and move it if it's
    /// a user defined name, or use it as a &str without copying if it's a builtin name.
    /// I'm not sure the generic constraint works as expected.
    pub fn new<S: Into<String> + AsRef<str>>(name: S) -> Self {
        if let Some(name) = is_builtin_type(name.as_ref()) {
            Self::Builtin(name)
        } else {
            Self::UserDefined(name.into())
        }
    }
    pub fn name(&self) -> &str {
        match self {
            // TypeName::None => {""}
            TypeName::Builtin(n) => n,
            TypeName::UserDefined(n) => n,
        }
    }
}

impl<S: Into<String> + AsRef<str>> From<S> for TypeName {
    fn from(value: S) -> Self {
        TypeName::new(value)
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Type {
    Simple {
        type_name: TypeName,
    },
    Nested {
        type_name: TypeName,
        children: TypedIdentifiers,
    },
    Function {
        parameter: Box<TypedIdentifier>,
        returned: Box<TypedIdentifier>,
    },
}

#[allow(unused)]
pub type Types = Vec<Type>;

impl Type {
    pub fn from<S: Into<String> + AsRef<str>>(typename: S, mut children: TypedIdentifiers) -> Type {
        if children.is_empty() {
            Type::simple(typename)
        } else {
            Type::children(typename, children)
        }
    }
    pub fn from_nameless<S: Into<String> + AsRef<str>>(typename: S, mut children: Types) -> Type {
        Self::from(
            typename,
            children
                .into_iter()
                .map(|t| TypedIdentifier::nameless(t))
                .collect(),
        )
    }
    pub fn simple<S: Into<String> + AsRef<str>>(type_name: S) -> Type {
        let type_name = TypeName::new(type_name);
        Type::Simple { type_name }
    }
    pub fn children<S: Into<String> + AsRef<str>>(
        type_name: S,
        children: TypedIdentifiers,
    ) -> Type {
        let type_name = TypeName::new(type_name);
        Type::Nested {
            type_name,
            children,
        }
    }
    pub fn nothing() -> Type {
        builtin_types::NOTHING
    }
    pub fn function(parameter: TypedIdentifier, returned: TypedIdentifier) -> Type {
        Type::Function {
            parameter: Box::new(parameter),
            returned: Box::new(returned),
        }
    }
}

impl Type {
    pub fn name(&self) -> &str {
        match self {
            Type::Simple { type_name } | Type::Nested { type_name, .. } => type_name.name(),
            Type::Function { .. } => BuiltinType::Function.name(),
        }
    }

    pub fn as_function(&self) -> Result<Option<(Type, Type)>, AnyError> {
        if let Type::Function {
            parameter,
            returned,
        } = self
        {
            Ok(Some((parameter.type_.clone(), returned.type_.clone())))
        } else {
            Ok(None)
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
    pub fn empty() -> Self {
        Self {
            initial: Box::new(Expression::Nothing),
            transformations: Vec::new(),
        }
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
    pub fn unknown() -> Self {
        Self {
            name: "".to_string(),
            type_: builtin_types::UNKNOWN,
        }
    }
    pub fn unknown_type(name: String) -> Self {
        Self {
            name,
            type_: builtin_types::UNKNOWN,
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
pub struct LoopOr {
    pub iteration_elem: TypedIdentifier,
    pub body: Chain,
    pub otherwise: Chain,
}
#[derive(PartialEq, Debug, Clone)]
pub struct Times {
    pub iteration_elem: TypedIdentifier,
    pub body: Chain,
}
#[derive(PartialEq, Debug, Clone)]
pub struct TimesOr {
    pub iteration_elem: TypedIdentifier,
    pub body: Chain,
    pub otherwise: Chain,
}
#[derive(PartialEq, Debug, Clone)]
pub struct Replace {
    pub iteration_elem: TypedIdentifier,
    pub body: Chain,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Map {
    pub iteration_elem: TypedIdentifier,
    pub body: Chain,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Branch {
    pub yes: Chain,
    pub no: Chain,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Something {
    pub elem: TypedIdentifier,
    pub something: Chain,
    pub nothing: Chain,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Inspect {
    pub elem: TypedIdentifier,
    pub body: Chain,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Cast {
    pub target_type: TypedIdentifier,
}

pub type Expressions = Vec<Expression>;
pub type Transformations = Vec<Transformation>;
