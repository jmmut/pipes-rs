use crate::frontend::expression::{
    BrowseOr, Chain, Composed, Expression, ExpressionSpan, Function, Loop, Map, Operation, Type,
    TypeName, TypedIdentifier, TypedIdentifiers,
};
use crate::frontend::token::Keyword;
use crate::middleend::intrinsics::builtin_types;
use std::fmt::{Display, Formatter};

impl Display for ExpressionSpan {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.syntactic_type)
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Nothing => write!(f, "{{}}"),
            Expression::Value(value) => write!(f, "{}", value),
            Expression::Identifier(name) => write!(f, "{}", name),
            Expression::Type(type_) => write!(f, "{}", type_),
            Expression::Chain(chain) => write!(f, "{}", chain),
            Expression::StaticList { elements } => {
                write!(f, "[")?;
                for elem in elements {
                    write!(f, " {}", elem)?;
                }
                write!(f, " ]")
            }
            Expression::Function(Function {
                parameters,
                returned,
                body,
            }) => {
                let returneds = vec![returned.clone()];
                let force_param_parens = returned.type_ != builtin_types::NOTHING;
                write!(
                    f,
                    "{}{}{} {}",
                    Keyword::Function.name(),
                    typed_identifiers_to_str(&parameters, force_param_parens),
                    typed_identifiers_to_str(&returneds, false),
                    body,
                )
            }
            Expression::Composed(Composed::Loop(Loop { body })) => {
                write!(f, "{} {}", Keyword::Loop.name(), body)
            }
            Expression::Composed(Composed::Map(Map {
                iteration_elem,
                body,
            })) => {
                write!(
                    f,
                    "{}({}) {{{}}}",
                    Keyword::Map.name(),
                    iteration_elem,
                    body,
                )
            }
            Expression::Composed(Composed::BrowseOr(BrowseOr {
                iteration_elem,
                body,
                otherwise,
            })) => {
                write!(
                    f,
                    "{}({}) {{{}}} {{{}}}",
                    Keyword::BrowseOr.name(),
                    iteration_elem,
                    body,
                    otherwise
                )
            }
            _ => unimplemented!("missing diplay for {:?}", self),
        }
    }
}

impl Display for Chain {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(initial) = &self.initial {
            write!(f, "{}", initial)?;
        }
        for operation in &self.operations {
            write!(f, " {}", operation)?;
        }
        Ok(())
    }
}

impl Display for Operation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.operator.operator)?;
        let mut first = true;
        for operand in &self.operands {
            if !first {
                write!(f, " ")?;
            }
            first = false;
            write!(f, "{}", operand)?;
        }
        Ok(())
    }
}

impl Display for TypeName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            TypeName::Builtin(name) => name,
            TypeName::UserDefined(name) => name.as_str(),
        };
        write!(f, "{}", name)
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Simple { type_name } => {
                write!(f, "{}", type_name)
            }
            Type::Nested {
                type_name,
                children,
            } => {
                write!(
                    f,
                    "{}{}",
                    type_name,
                    typed_identifiers_to_str(&children, false)
                )
            }
            Type::Function {
                parameters,
                returned,
            } => {
                let returneds = vec![*returned.clone()];
                let force_param_parens = returned.type_ != builtin_types::NOTHING;
                write!(
                    f,
                    "{}{}{}",
                    Keyword::Function.name(),
                    typed_identifiers_to_str(&parameters, force_param_parens),
                    typed_identifiers_to_str(&returneds, false)
                )
            }
        }
    }
}

pub fn typed_identifiers_to_str(children: &TypedIdentifiers, force_parenthesis: bool) -> String {
    if children.len() == 0 && !force_parenthesis {
        "".to_string()
    } else if children.len() == 1 && children[0].type_ == builtin_types::NOTHING {
        if force_parenthesis {
            "()".to_string()
        } else {
            "".to_string()
        }
    } else {
        let mut accum = "(".to_string();
        for child in children {
            accum += &child.to_string();
            accum += "  ";
        }
        if !children.is_empty() {
            accum.pop();
            accum.pop();
        }
        accum += ")";
        accum
    }
}

impl Display for TypedIdentifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.name.is_empty() {
            write!(f, ":{}", self.type_)
        } else {
            write!(f, "{} :{}", self.name, self.type_)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::common::unwrap_display;
    use crate::frontend::{lex_and_parse, parse_type};

    #[test]
    fn test_display_simple_type() {
        assert_type_displayed("simple_name");
    }
    #[test]
    fn test_display_nested_type() {
        assert_type_displayed("array(:i64)");
    }
    #[test]
    fn test_display_function_type() {
        assert_type_displayed("function(param_name :param_type)(return_name :return_type)");
        assert_eq!(parse_type("function").to_string(), "function()(:any)");
        assert_type_displayed("function()(:i64)");
    }

    fn assert_type_displayed(code: &str) {
        let type_ = parse_type(code);
        let displayed = type_.to_string();
        assert_eq!(displayed, code);
    }

    #[test]
    fn test_display_complex_expression() {
        let code = "4 |print_char +5";
        let parsed = unwrap_display(lex_and_parse(code));
        let displayed = format!("{}", parsed.main());
        assert_eq!(code, displayed);
    }
}
