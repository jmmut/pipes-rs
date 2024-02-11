use crate::common::AnyError;
use crate::frontend::lexer::Operator;
use crate::middleend::intrinsics::{builtin_types, is_builtin_type, BuiltinType};

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

#[derive(Debug, Clone)]
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
    pub fn from<S: Into<String> + AsRef<str>>(typename: S, children: TypedIdentifiers) -> Type {
        if children.is_empty() {
            Type::simple(typename)
        } else {
            Type::children(typename, children)
        }
    }
    pub fn from_nameless<S: Into<String> + AsRef<str>>(typename: S, children: Types) -> Type {
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

/// Structural equality, useful for tests in this project.
/// In summary, equality needs to be custom to be able to ignore nested names except for structs.
/// This custom comparison can not be done just for TypedIdentifier because it doesn't know if
/// it's being used in a struct or in some other type.
/// For semantic equality, see [crate::middleend::typing::unify].
impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        if self.name() != other.name() {
            return false;
        }
        return match (self, other) {
            (Type::Simple { .. }, Type::Simple { .. }) => true,
            #[rustfmt::skip]
            (
                Type::Nested { children: children_1, .. },
                Type::Nested { children: children_2, .. },
            ) => {
                if children_1.len() != children_2.len() {
                    false
                } else if self.name() == BuiltinType::Struct.name() {
                    children_1 == children_2 // this comparison includes the name of the typed identifiers
                } else {
                    // if not structs, only compare the types, not the names
                    for (child_1, child_2) in children_1.iter().zip(children_2) {
                        if child_1.type_ != child_2.type_ {
                            return false;
                        }
                    }
                    true
                }
            }
            #[rustfmt::skip]
            (
                Type::Function { parameter: param_1, returned: ret_1},
                Type::Function { parameter: param_2, returned: ret_2},
            ) => param_1.type_ == param_2.type_ && ret_1.type_ == ret_2.type_,
            (Type::Simple { .. }, Type::Nested { .. }) => false,
            (Type::Simple { .. }, Type::Function { .. }) => false,

            (Type::Nested { .. }, Type::Simple { .. }) => false,
            (Type::Nested { .. }, Type::Function { .. }) => false,

            (Type::Function { .. }, Type::Simple { .. }) => false,
            (Type::Function { .. }, Type::Nested { .. }) => false,
        };
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
    pub fn any(name: String) -> Self {
        Self {
            name,
            type_: builtin_types::ANY,
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
