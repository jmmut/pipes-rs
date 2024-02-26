mod display;

use crate::common::{err, AnyError};
use crate::frontend::location::{Span, NO_SPAN};
use crate::frontend::token::OperatorSpan;
use crate::middleend::intrinsics::{builtin_types, is_builtin_type, BuiltinType};

#[derive(Debug, Clone)]
pub struct ExpressionSpan {
    pub syntactic_type: Expression,
    pub span: Span,
}

impl ExpressionSpan {
    pub fn new(syntactic_type: Expression, span: Span) -> Self {
        Self {
            syntactic_type,
            span,
        }
    }
    // TODO: most of the usages of this function should be removed to use a proper span
    pub fn new_spanless(syntactic_type: Expression) -> Self {
        Self {
            syntactic_type,
            span: NO_SPAN,
        }
    }
    /// syntactic type, as opposed to the semantic type. E.g.: the syntactic type of '{3+5}' is
    /// `ExpressionType::Chain`, and the semantic type is `Type::simple("i64")`.
    pub fn syn_type(&self) -> &Expression {
        &self.syntactic_type
    }
    pub fn syn_type_mut(&mut self) -> &mut Expression {
        &mut self.syntactic_type
    }
    pub fn span(&self) -> Span {
        self.span
    }
    pub fn take(self) -> (Expression, Span) {
        (self.syntactic_type, self.span)
    }
}
impl PartialEq for ExpressionSpan {
    fn eq(&self, other: &Self) -> bool {
        self.syntactic_type == other.syntactic_type
    }
}

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

impl Expression {
    pub fn new(expression: Expression, span: Span) -> Self {
        expression
    }
    pub fn new_spanless(expression: Expression) -> Self {
        Self::new(expression, NO_SPAN)
    }
    pub fn syn_type(&self) -> &Expression {
        self
    }
    pub fn syn_type_mut(&mut self) -> &mut Expression {
        self
    }
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

    pub fn chain(initial: Box<ExpressionSpan>, operations: Operations) -> Self {
        Self::Chain(Chain {
            initial: Some(initial),
            operations,
        })
    }
    #[allow(unused)]
    pub fn function_single(parameter: TypedIdentifier, body: Chain) -> Self {
        Self::Function(Function {
            parameters: vec![parameter],
            body,
        })
    }
    pub fn function(parameters: TypedIdentifiers, body: Chain) -> Self {
        Self::Function(Function { parameters, body })
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
        parameters: TypedIdentifiers,
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
        if type_name.as_ref() == BuiltinType::Array.name() {
            Type::children(
                type_name,
                vec![TypedIdentifier::nameless(builtin_types::ANY)],
            )
        } else {
            let type_name = TypeName::new(type_name);
            Type::Simple { type_name }
        }
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
    pub fn function_single(parameter: TypedIdentifier, returned: TypedIdentifier) -> Type {
        Type::Function {
            parameters: vec![parameter],
            returned: Box::new(returned),
        }
    }
    pub fn function(parameters: TypedIdentifiers, returned: TypedIdentifier) -> Type {
        Type::Function {
            parameters,
            returned: Box::new(returned),
        }
    }

    fn compare_typed_identifiers(
        children_1: &TypedIdentifiers,
        children_2: &TypedIdentifiers,
    ) -> bool {
        if children_1.len() == children_2.len() {
            for (child_1, child_2) in children_1.iter().zip(children_2) {
                if child_1.type_ != child_2.type_ {
                    return false;
                }
            }
            true
        } else {
            false
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
    //
    // pub fn as_function(&self) -> Result<Option<(Type, Type)>, AnyError> {
    //     if let Type::Function {
    //         parameter,
    //         returned,
    //     } = self
    //     {
    //         Ok(Some((parameter.type_.clone(), returned.type_.clone())))
    //     } else {
    //         Ok(None)
    //     }
    // }
    pub fn array_element(&self) -> Result<TypedIdentifier, AnyError> {
        if let Type::Nested {
            type_name,
            children,
        } = self
        {
            if type_name.name() == BuiltinType::Array.name() && children.len() == 1 {
                if let Some(elem) = children.last() {
                    return Ok(elem.clone());
                }
            }
        }
        err(format!("Bug: expected type to be an array: {:?}", self))
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
                    Self::compare_typed_identifiers(children_1, children_2)
                }
            }
            #[rustfmt::skip]
            (
                Type::Function { parameters: param_1, returned: ret_1},
                Type::Function { parameters: param_2, returned: ret_2},
            ) => Self::compare_typed_identifiers(param_1, param_2) && ret_1.type_ == ret_2.type_,
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
    pub initial: Option<Box<ExpressionSpan>>,
    pub operations: Operations,
}

impl Chain {
    pub fn empty() -> Self {
        Self {
            initial: Some(Box::new(ExpressionSpan::new(Expression::Nothing, NO_SPAN))),
            operations: Vec::new(),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Operation {
    pub operator: OperatorSpan,
    pub operands: Expressions,
}
impl Operation {
    pub fn single(operator: OperatorSpan, operand: ExpressionSpan) -> Operation {
        Operation {
            operator,
            operands: vec![operand],
        }
    }
    pub fn several(operator: OperatorSpan, operands: Expressions) -> Operation {
        Operation { operator, operands }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Function {
    pub parameters: TypedIdentifiers,
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

pub type Expressions = Vec<ExpressionSpan>;
pub type Operations = Vec<Operation>;

pub fn take_single(mut expressions: Expressions) -> Option<ExpressionSpan> {
    if expressions.len() != 1 {
        None
    } else {
        expressions.pop()
    }
}
