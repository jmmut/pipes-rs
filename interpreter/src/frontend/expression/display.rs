use std::fmt::{Debug, Display, Formatter};

use crate::frontend::expression::{
    Branch, Browse, BrowseOr, Cast, Chain, Composed, Comptime, Expression, ExpressionSpan, Filter,
    Function, Inspect, Loop, Map, Operation, Replace, Something, Times, TimesOr, Type, TypeName,
    TypedIdentifier, TypedIdentifiers,
};
use crate::frontend::sources::token::{Keyword, OperatorSpan};
use crate::middleend::intrinsics::builtin_types;

impl Display for ExpressionSpan {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.syntactic_type)
    }
}
impl Debug for ExpressionSpan {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ExpressionSpan")
            .field("syntactic_type", &self.syntactic_type)
            .field("semantic_type", &self.semantic_type.to_string())
            .finish()
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Nothing => Ok(()), // we don't need to write!(f, "{{}}") because we will always use an empty chain to store nothing, I think
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
                is_macro,
            }) => {
                let returneds = vec![returned.clone()];
                let force_param_parens = returned.type_ != builtin_types::NOTHING;
                write!(
                    f,
                    "{}{}{} {}",
                    if *is_macro {
                        Keyword::Macro.name()
                    } else {
                        Keyword::Function.name()
                    },
                    typed_identifiers_to_str(&parameters, force_param_parens),
                    typed_identifiers_to_str(&returneds, false),
                    body,
                )
            }
            Expression::Composed(composed) => write!(f, "{}", composed),
            Expression::TypedIdentifiers(tis) => {
                write!(f, "{}", typed_identifiers_to_str(tis, true))
            }
            Expression::Abstract(abstract_) => write!(f, "{}", abstract_),
        }
    }
}

impl Display for Composed {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name = self.name();
        match self {
            Composed::Loop(Loop { body }) => {
                write!(f, "{} {}", Keyword::Loop.name(), body)
            }
            #[rustfmt::skip]
            Composed::Map(Map { iteration_elem, body})
            | Composed::Replace(Replace { iteration_elem, body})
            | Composed::Filter(Filter { iteration_elem, body})
            | Composed::Browse(Browse { iteration_elem, body })
            | Composed::Times(Times {iteration_elem, body})
            | Composed::Inspect(Inspect { elem: iteration_elem, body })
            => {
                write_types_chain(f, name, iteration_elem, body)
            },
            #[rustfmt::skip]
            Composed::BrowseOr(BrowseOr { iteration_elem, body, otherwise, })
            | Composed::TimesOr(TimesOr { iteration_elem, body, otherwise, })
            | Composed::Something(Something { elem: iteration_elem, something: body, nothing: otherwise })=> {
                write_types_chain_chain(f, name, iteration_elem, body, otherwise)
            },
            Composed::Branch(Branch { yes, no }) => {
                write!(f, "{} {} {}", Keyword::Branch.name(), yes, no)
            }
            Composed::Cast(Cast { target_type }) => {
                write!(f, "{}({})", Keyword::Cast.name(), target_type)
            }
            Composed::Comptime(Comptime { body }) => {
                write!(f, "{} {}", Keyword::Comptime.name(), body)
            }
        }
    }
}

fn write_types_chain_chain(
    f: &mut Formatter,
    composed_name: &str,
    iteration_elem: &TypedIdentifier,
    body: &Chain,
    otherwise: &Chain,
) -> std::fmt::Result {
    write!(
        f,
        "{}({}) {{{}}} {{{}}}",
        composed_name, iteration_elem, body, otherwise
    )
}
fn write_types_chain(
    f: &mut Formatter,
    composed_name: &str,
    iteration_elem: &TypedIdentifier,
    body: &Chain,
) -> std::fmt::Result {
    write!(f, "{}({}) {{{}}}", composed_name, iteration_elem, body)
}

impl Display for Chain {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        let mut first = true;
        for operation in &self.operations {
            if first {
                first = false
            } else {
                write!(f, " ")?;
            }
            write!(f, "{}", operation)?;
        }
        write!(f, "}}")?;
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

impl Debug for Operation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Operation")
            .field("operator", &self.operator)
            .field("operands", &self.operands)
            .field("semantic_type", &self.sem_type.to_string())
            .finish()
    }
}

impl Debug for OperatorSpan {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.operator)
        // f.debug_struct("OperatorSpan").field("operator", &self.operator).finish()
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

pub fn indent(s: &str) -> String {
    let mut deep = 0;
    let indent_size = 4;
    let mut buffer = String::new();
    for b in s.chars() {
        if b == '}' {
            deep -= 1;
            buffer.push('\n');
            buffer += &" ".repeat(indent_size * deep);
        }
        buffer.push(b);
        if b == '{' {
            deep += 1;
            buffer.push('\n');
            buffer += &" ".repeat(indent_size * deep);
        } else if b == '\n' {
            buffer += &" ".repeat(indent_size * deep);
        }
    }
    buffer
}

#[cfg(test)]
mod tests {
    use crate::common::unwrap_display;
    use crate::frontend::expression::display::indent;
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
        assert_eq!(displayed, format!("{{;{}}}", code));
    }
    #[test]
    fn test_display_several_operands() {
        let code = "4 |print_char 5 6";
        let parsed = unwrap_display(lex_and_parse(code));
        let displayed = format!("{}", parsed.main());
        assert_eq!(displayed, format!("{{;{}}}", code));
    }
    #[test]
    fn test_indent() {
        let initial = "function {4 |branch {a} {b}}";
        let indented = indent(initial);
        assert_eq!(
            indented,
            r#"function {
    4 |branch {
        a
    } {
        b
    }
}"#
        )
    }
}
