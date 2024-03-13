use crate::common::{err, err_span, AnyError};
use crate::frontend::expression::{
    Expression, ExpressionSpan, Type, TypedIdentifier, TypedIdentifiers,
};
use crate::frontend::sources::location::{SourceCode, Span};
use crate::frontend::sources::Sources;
use crate::middleend::intrinsics::is_builtin_type;
use std::collections::HashMap;

pub trait TypeView {
    fn get_type(&self, name: &str, span: Span) -> Result<&Type, AnyError>;
    fn get_source(&self, identifier_name: &str) -> Option<&SourceCode>;
}

pub struct Expand<'a> {
    type_view: &'a dyn TypeView,
}
// impl<'a> Expand<'a> {
impl Expand<'_> {
    pub fn expand(type_: &Type, type_view: &dyn TypeView, span: Span) -> Result<Type, AnyError> {
        let e = Expand { type_view };
        e.expand_recursive(type_, span)
    }
    fn expand_recursive(&self, type_: &Type, span: Span) -> Result<Type, AnyError> {
        match type_ {
            Type::Simple { type_name } => {
                if is_builtin_type(type_name.name()).is_some() {
                    Ok(Type::Simple {
                        type_name: type_name.clone(),
                    })
                } else {
                    self.get_type(type_name.name(), span).cloned()
                }
            }
            Type::Nested {
                type_name,
                children,
            } => {
                let expanded = self.expand_children(children, span)?;
                Ok(Type::Nested {
                    type_name: type_name.clone(),
                    children: expanded,
                })
            }
            Type::Function {
                parameters,
                returned,
            } => {
                let expanded = self.expand_children(parameters, span)?;
                Ok(Type::Function {
                    parameters: expanded,
                    returned: Box::new(self.expand_typed_identifier(returned, span)?),
                })
            }
        }
    }
    fn get_type(&self, name: &str, span: Span) -> Result<&Type, AnyError> {
        self.type_view.get_type(name, span)
    }
    fn expand_children(
        &self,
        children: &TypedIdentifiers,
        span: Span,
    ) -> Result<TypedIdentifiers, AnyError> {
        let mut expanded = Vec::new();
        for child in children {
            expanded.push(self.expand_typed_identifier(child, span)?);
        }
        Ok(expanded)
    }
    fn expand_typed_identifier(
        &self,
        type_: &TypedIdentifier,
        span: Span,
    ) -> Result<TypedIdentifier, AnyError> {
        Ok(TypedIdentifier {
            name: type_.name.clone(),
            type_: self.expand_recursive(&type_.type_, span)?,
        })
    }
}
