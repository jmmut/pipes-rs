use crate::common::{err, AnyError};
use crate::frontend::sources::location::{Span, NO_SPAN};
use crate::frontend::sources::token::{Keyword, Operator, OperatorSpan};
use crate::middleend::intrinsics::{builtin_types, is_builtin_type, BuiltinType};

pub mod display;

#[derive(Clone)]
pub struct ExpressionSpan {
    pub syntactic_type: Expression,
    pub semantic_type: Type,
    pub span: Span,
}

impl ExpressionSpan {
    pub fn new_typeless(syntactic_type: Expression, span: Span) -> Self {
        Self::new(syntactic_type, builtin_types::UNKNOWN, span)
    }
    pub fn new(syntactic_type: Expression, semantic_type: Type, span: Span) -> Self {
        Self {
            syntactic_type,
            semantic_type,
            span,
        }
    }
    // TODO: most of the usages of this function should be removed to use a proper span
    pub fn new_spanless(syntactic_type: Expression) -> Self {
        Self::new_typeless(syntactic_type, NO_SPAN)
    }
    /// syntactic type, as opposed to the semantic type. E.g.: the syntactic type of '{3+5}' is
    /// `ExpressionType::Chain`, and the semantic type is `Type::simple("i64")`.
    pub fn syn_type(&self) -> &Expression {
        &self.syntactic_type
    }
    pub fn syn_type_mut(&mut self) -> &mut Expression {
        &mut self.syntactic_type
    }

    /// semantic type, as opposed to the syntactic type. E.g.: the syntactic type of '{3+5}' is
    /// `ExpressionType::Chain`, and the semantic type is `Type::simple("i64")`.
    pub fn sem_type(&self) -> &Type {
        &self.semantic_type
    }
    pub fn sem_type_mut(&mut self) -> &mut Type {
        &mut self.semantic_type
    }
    pub fn take_sem_type(self) -> Type {
        self.semantic_type
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
    Browse(Browse),
    BrowseOr(BrowseOr),
    Times(Times),
    TimesOr(TimesOr),
    Replace(Replace),
    Map(Map),
    Filter(Filter),
    Branch(Branch),
    Something(Something),
    Inspect(Inspect),
    Cast(Cast),
}
impl Composed {
    pub fn name(&self) -> &'static str {
        match self {
            Composed::Loop(_) => Keyword::Loop.name(),
            Composed::Browse(_) => Keyword::Browse.name(),
            Composed::BrowseOr(_) => Keyword::BrowseOr.name(),
            Composed::Times(_) => Keyword::Times.name(),
            Composed::TimesOr(_) => Keyword::TimesOr.name(),
            Composed::Replace(_) => Keyword::Replace.name(),
            Composed::Map(_) => Keyword::Map.name(),
            Composed::Filter(_) => Keyword::Filter.name(),
            Composed::Branch(_) => Keyword::Branch.name(),
            Composed::Something(_) => Keyword::Something.name(),
            Composed::Inspect(_) => Keyword::Inspect.name(),
            Composed::Cast(_) => Keyword::Cast.name(),
        }
    }
}

impl Expression {
    pub fn empty_chain() -> Self {
        Self::Chain(Chain::empty())
    }

    pub fn chain_initial(initial: ExpressionSpan, operations: Operations) -> Self {
        Self::Chain(Chain::new_initial(initial, operations))
    }
    pub fn chain(operations: Operations) -> Self {
        Self::Chain(Chain::new(operations))
    }
    pub fn list(elements: Expressions) -> Self {
        Self::StaticList { elements }
    }
    #[allow(unused)]
    pub fn function_single(parameter: TypedIdentifier, body: Chain) -> Self {
        Self::Function(Function::any_return(vec![parameter], body))
    }
    pub fn function_any_return(parameters: TypedIdentifiers, body: Chain) -> Self {
        Self::Function(Function::any_return(parameters, body))
    }
    pub fn function(parameters: TypedIdentifiers, returned: TypedIdentifier, body: Chain) -> Self {
        Self::Function(Function::new(parameters, returned, body))
    }
    pub fn loop_(body: Chain) -> Self {
        Self::Composed(Composed::Loop(Loop { body }))
    }
    #[allow(unused)]
    pub fn browse(elem: TypedIdentifier, body: Chain) -> Self {
        Self::Composed(Composed::Browse(Browse {
            iteration_elem: elem,
            body,
        }))
    }
    #[allow(unused)]
    pub fn browse_or(elem: TypedIdentifier, body: Chain, otherwise: Chain) -> Self {
        Self::Composed(Composed::BrowseOr(BrowseOr {
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
    pub fn filter(elem: TypedIdentifier, body: Chain) -> Self {
        Self::Composed(Composed::Filter(Filter {
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

    pub fn to_chain(self) -> Result<Chain, AnyError> {
        if let Expression::Chain(chain) = self {
            Ok(chain)
        } else {
            err(format!("Bug: tried to use as a chain but was {}", self))
        }
    }
}

#[derive(PartialEq, Debug, Clone, Eq, Hash)]
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

#[derive(Debug, Clone, Eq, Hash)]
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
        if type_name.as_ref() == BuiltinType::Array.name()
            || type_name.as_ref() == BuiltinType::List.name()
        {
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
    pub fn single_element(&self) -> Result<TypedIdentifier, AnyError> {
        if let Type::Nested { children, .. } = self {
            if children.len() == 1 {
                if let Some(elem) = children.last() {
                    return Ok(elem.clone());
                }
            }
        }
        err(format!(
            "Bug: expected type to be a nested type with 1 subtype: {:?}",
            self
        ))
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
    pub operations: Operations,
}

impl Chain {
    pub fn new(operations: Operations) -> Self {
        Self { operations }
    }
    pub fn new_initial(initial: ExpressionSpan, mut operations: Operations) -> Self {
        let mut all_ops = Operations::new();
        let sem_type = initial.sem_type().clone();
        all_ops.push(Operation::single(
            OperatorSpan::new(
                Operator::Ignore,
                Span {
                    start: initial.span.start,
                    end: initial.span.start,
                },
            ),
            initial,
            sem_type,
        ));
        all_ops.append(&mut operations);
        Self {
            operations: all_ops,
        }
    }
    pub fn new_opt_initial(initial: Option<Box<ExpressionSpan>>, operations: Operations) -> Self {
        if let Some(initial) = initial {
            Self::new_initial(*initial, operations)
        } else {
            Self { operations }
        }
    }
    pub fn empty() -> Self {
        Self {
            operations: Vec::new(),
        }
    }
    pub fn content_span(&self) -> Span {
        if let Some(first) = self.operations.first() {
            if let Some(last) = self.operations.last() {
                first.content_span().merge(&last.content_span())
            } else {
                first.content_span()
            }
        } else {
            NO_SPAN
        }
    }
}

#[derive(PartialEq, Clone)]
pub struct Operation {
    pub operator: OperatorSpan,
    pub operands: Expressions,

    /// semantic type of the resulting expression after applying this operation
    pub sem_type: Type,
    // pub span: Span, // TODO
}
impl Operation {
    pub fn single_no_sem_type(operator: OperatorSpan, operand: ExpressionSpan) -> Operation {
        Self::several_no_sem_type(operator, vec![operand])
    }
    pub fn several_no_sem_type(operator: OperatorSpan, operands: Expressions) -> Operation {
        Operation {
            operator,
            operands,
            sem_type: builtin_types::UNKNOWN,
        }
    }
    pub fn single(operator: OperatorSpan, operand: ExpressionSpan, sem_type: Type) -> Operation {
        Self::several(operator, vec![operand], sem_type)
    }
    pub fn several(operator: OperatorSpan, operands: Expressions, sem_type: Type) -> Operation {
        Operation {
            operator,
            operands,
            sem_type,
        }
    }
    pub fn content_span(&self) -> Span {
        if let Some(last) = self.operands.last() {
            self.operator.span.merge(&last.span)
        } else {
            self.operator.span
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Function {
    pub parameters: TypedIdentifiers,
    pub returned: TypedIdentifier,
    pub body: Chain,
}
impl Function {
    pub fn new(parameters: TypedIdentifiers, returned: TypedIdentifier, body: Chain) -> Self {
        Self {
            parameters,
            returned,
            body,
        }
    }
    pub fn any_return(parameters: TypedIdentifiers, body: Chain) -> Self {
        Self {
            parameters,
            returned: TypedIdentifier::nameless_any(),
            body,
        }
    }
}
#[derive(PartialEq, Debug, Clone, Eq, Hash)]
pub struct TypedIdentifier {
    pub name: String,
    pub type_: Type,
}

pub type TypedIdentifiers = Vec<TypedIdentifier>;

impl TypedIdentifier {
    pub fn new(name: String, type_: Type) -> Self {
        Self { name, type_: type_ }
    }
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
    pub fn nameless_any() -> Self {
        Self::nameless(builtin_types::ANY)
    }
    pub fn nameless(type_: Type) -> Self {
        Self {
            name: "".to_string(),
            type_: type_,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Loop {
    pub body: Chain,
}
#[derive(PartialEq, Debug, Clone)]
pub struct Browse {
    pub iteration_elem: TypedIdentifier,
    pub body: Chain,
}
#[derive(PartialEq, Debug, Clone)]
pub struct BrowseOr {
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
pub struct Filter {
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
