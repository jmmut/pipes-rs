use crate::frontend::expression::{Type, TypedIdentifier, TypedIdentifiers};
use crate::middleend::intrinsics::{builtin_types, BuiltinType};
use crate::middleend::typing::cast::cast;

/// Compare types semantically and merge them.
/// In summary, the Any type can be casted implicitly to any type, and this function
/// can be used to infer types. For example, the types `function(:i64) (:any)` and
/// `function(:any) (:i64)` are compatible, and this function would return `function(:i64) (:i64)`.
pub fn unify(first: &Type, second: &Type) -> Option<Type> {
    let first_name = first.name();
    let second_name = second.name();
    if first_name == BuiltinType::Any.name() {
        return Some(second.clone());
    } else if second_name == BuiltinType::Any.name() {
        return Some(first.clone());
    } else if let Some(unified) = try_or(first, second) {
        return Some(unified);
    } else if let Some(unified) = try_list(first, second) {
        return Some(unified);
    } else if first_name != second_name {
        return None;
    }

    match (first, second) {
        (Type::Simple { .. }, Type::Simple { .. }) => Some(second.clone()),
        #[rustfmt::skip]
        (Type::Nested { children: children_1, .. }, Type::Nested { children: children_2, .. }) => {
            unify_nested(second_name, children_1, children_2)
        },
        #[rustfmt::skip]
        (
            Type::Function { parameters: param_1, returned: returned_1},
            Type::Function { parameters: param_2, returned: returned_2},
        ) => {
            let parameters = unify_typed_identifiers(param_1, param_2)?;
            let returned = Box::new(unify_typed_identifier(returned_1, returned_2)?);
            Some(Type::Function { parameters, returned})
        }
        (_, _) => None,
    }
}
fn try_or(first: &Type, second: &Type) -> Option<Type> {
    let (or_children, other) = if first.name() == BuiltinType::Or.name() {
        if let Type::Nested { children, .. } = first {
            (children, second)
        } else {
            panic!("Found a :or without inner types. Not sure if this should be allowed");
        }
    } else if second.name() == BuiltinType::Or.name() {
        if let Type::Nested { children, .. } = second {
            (children, first)
        } else {
            panic!("Found a :or without inner types. Not sure if this should be allowed");
        }
    } else {
        return None;
    };
    for child in or_children {
        if let Some(unified) = unify(&child.type_, other) {
            return Some(unified);
        }
    }
    None
}

pub fn try_list(first: &Type, second: &Type) -> Option<Type> {
    if first.name() == BuiltinType::List.name() {
        if second.name() == BuiltinType::Array.name() {
            unify_single_element(first, second, BuiltinType::Array.name())
        } else {
            None // forbid implicit conversion from list to tuple as we don't know the length
        }
    } else if second.name() == BuiltinType::List.name() {
        if first.name() == BuiltinType::Array.name() {
            unify_single_element(first, second, BuiltinType::List.name())
        } else if first.name() == BuiltinType::Tuple.name() {
            if let Type::Nested { children, .. } = first {
                let list_type = second.single_element().unwrap();
                if let Some(unified_subtype) = all_unify_to(children, list_type.type_) {
                    Some(Type::from(BuiltinType::List.name(), vec![unified_subtype]))
                } else {
                    None
                }
            } else {
                panic!("Bug: a tuple should have children")
            }
        } else {
            None
        }
    } else {
        None
    }
}

fn unify_single_element(first: &Type, second: &Type, parent_name: &str) -> Option<Type> {
    let unified_subtype = unify_typed_identifier(
        &first.single_element().unwrap(),
        &second.single_element().unwrap(),
    )?;
    Some(Type::from(parent_name, vec![unified_subtype]))
}

fn unify_nested(
    second_name: &str,
    children_1: &TypedIdentifiers,
    children_2: &TypedIdentifiers,
) -> Option<Type> {
    if let Some(children) = unify_typed_identifiers(children_1, children_2) {
        Some(Type::from(second_name, children))
    } else {
        None
    }
}

pub fn unify_typed_identifier(
    first: &TypedIdentifier,
    second: &TypedIdentifier,
) -> Option<TypedIdentifier> {
    if let Some(type_) = unify(&first.type_, &second.type_) {
        let name = unify_name(&first.name, &second.name);
        Some(TypedIdentifier { name, type_ })
    } else {
        None
    }
}

fn unify_name(first: &String, second: &String) -> String {
    if second.is_empty() {
        first.clone()
    } else {
        second.clone()
    }
}

pub fn unify_typed_identifiers(
    firsts: &TypedIdentifiers,
    seconds: &TypedIdentifiers,
) -> Option<TypedIdentifiers> {
    if firsts.len() == seconds.len() {
        let mut unified_types = Vec::new();
        for i in 0..firsts.len() {
            if let Some(unified) = unify_typed_identifier(&firsts[i], &seconds[i]) {
                unified_types.push(unified);
            } else {
                return None;
            }
        }
        Some(unified_types)
    } else {
        None
    }
}

pub fn all_same_type<T: PartialEq>(types: &Vec<T>) -> bool {
    if types.len() == 0 {
        true
    } else if types.len() == 1 {
        true
    } else {
        for i in 1..types.len() {
            if types[i - 1] != types[i] {
                return false;
            }
        }
        true
    }
}

pub fn all_unify_to(types: &TypedIdentifiers, mut accumulated: Type) -> Option<TypedIdentifier> {
    for type_ in types {
        accumulated = unify(&type_.type_, &accumulated)?;
    }
    Some(TypedIdentifier::nameless(accumulated))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::unwrap_display;
    use crate::frontend::expression::{Type, TypedIdentifier};
    use crate::frontend::parse_type;
    use crate::middleend::typing::builtin_types;

    #[test]
    fn test_basic_unifiable() {
        let unified = unify(&Type::nothing(), &Type::nothing());
        assert_eq!(unified, Some(Type::nothing()));

        let unified = unify(&Type::simple("i64".to_string()), &builtin_types::I64);
        assert_eq!(unified, Some(builtin_types::I64));

        let unified = unify(
            &Type::simple("custom".to_string()),
            &Type::simple("custom".to_string()),
        );
        assert_eq!(unified, Some(Type::simple("custom".to_string())));
    }

    #[test]
    fn test_basic_different() {
        let unified = unify(&Type::simple("i64"), &Type::simple("float"));
        assert_eq!(unified, None);
    }

    #[test]
    fn test_nested_single() {
        let second = Type::children(
            "mystruct",
            vec![TypedIdentifier {
                name: "field".to_string(),
                type_: builtin_types::I64,
            }],
        );

        let unified = unify(
            &Type::children(
                "mystruct",
                vec![TypedIdentifier {
                    name: "field".to_string(),
                    type_: builtin_types::I64,
                }],
            ),
            &second,
        );
        assert_eq!(unified, Some(second));
    }

    #[test]
    fn test_nested_several() {
        let second = Type::children(
            "mystruct",
            vec![
                TypedIdentifier {
                    name: "field".to_string(),
                    type_: builtin_types::I64,
                },
                TypedIdentifier {
                    name: "field_2".to_string(),
                    type_: builtin_types::I64,
                },
            ],
        );
        let unified = unify(
            &Type::children(
                "mystruct",
                vec![
                    TypedIdentifier {
                        name: "field".to_string(),
                        type_: builtin_types::I64,
                    },
                    TypedIdentifier {
                        name: "field_2".to_string(),
                        type_: builtin_types::I64,
                    },
                ],
            ),
            &second,
        );
        assert_eq!(unified, Some(second));
    }

    #[test]
    fn test_array_to_tuple() {
        let child = TypedIdentifier::nameless(builtin_types::I64);
        let first = Type::from("array", vec![child.clone()]);
        let second = Type::from("tuple", vec![child.clone(), child.clone()]);
        let unified = unify(&first, &second);
        assert_eq!(unified, None);
    }

    #[test]
    fn test_tuple_to_array() {
        let child = TypedIdentifier::nameless(builtin_types::I64);
        let first = Type::from("tuple", vec![child.clone(), child.clone()]);
        let second = Type::from("array", vec![child.clone()]);
        let unified = unify(&first, &second);
        assert_eq!(unified, None);
    }

    #[test]
    fn test_empty_tuple() {
        let child = TypedIdentifier::nameless(builtin_types::I64);
        let first = Type::from("tuple", vec![]);
        let parsed_empty_tuple = parse_type("tuple");
        assert_eq!(first, parsed_empty_tuple);
    }
    fn assert_unifies(first: &str, second: &str) {
        let second_type = parse_type(second);
        let unified = unify(&parse_type(first), &second_type);
        assert_eq!(
            unified.map(|u| u.to_string()),
            Some(second_type.to_string())
        )
    }
    fn assert_no_unify(first: &str, second: &str) {
        let unified = unify(&parse_type(first), &parse_type(second));
        assert_eq!(unified, None)
    }
    fn assert_unifies_to(first: &str, second: &str, expected: &str) {
        let unified = unify(&parse_type(first), &parse_type(second));
        let expected_type = parse_type(expected);
        assert_eq!(
            unified.map(|u| u.to_string()),
            Some(expected_type.to_string())
        )
    }
    #[test]
    fn test_list() {
        assert_unifies("list(:i64)", "array(:i64)");
        assert_unifies("list(:any)", "array(:i64)");
        assert_unifies("list", "array");
        assert_unifies("array(:i64)", "list(:i64)");
        assert_unifies_to("array(:i64)", "list(:any)", "list(:i64)");
        assert_unifies_to("array(:i64)", "list", "list(:i64)");

        assert_no_unify("list(:i64)", "tuple(:i64)");
        assert_no_unify("list(:any)", "tuple(:i64)");
        assert_no_unify("list", "tuple");
        assert_unifies("tuple(:i64)", "list(:i64)");
        assert_unifies("tuple(:i64 :i64)", "list(:i64)");
        assert_unifies_to("tuple(:i64 :i64)", "list(:any)", "list(:i64)");
        assert_unifies_to("tuple(:i64 :i64)", "list", "list(:i64)");

        assert_no_unify("tuple(:i64 :function)", "list");
    }
    #[test]
    fn test_nested_list() {
        assert_unifies("tuple(:tuple(:i64) :array(:i64))", "list(:list(:i64))");
    }

    #[test]
    fn test_complementary_types() {
        let second = Type::children(
            "mystruct",
            vec![
                TypedIdentifier::nameless(builtin_types::ANY),
                TypedIdentifier::nameless(builtin_types::I64),
            ],
        );
        let unified = unify(
            &Type::children(
                "mystruct",
                vec![
                    TypedIdentifier::nameless(builtin_types::I64),
                    TypedIdentifier::nameless(builtin_types::ANY),
                ],
            ),
            &second,
        );
        assert_eq!(
            unified,
            Some(Type::children(
                "mystruct",
                vec![
                    TypedIdentifier::nameless(builtin_types::I64),
                    TypedIdentifier::nameless(builtin_types::I64),
                ],
            ))
        );
    }

    #[test]
    fn test_or() {
        let i64 = parse_type("i64");
        let unified = unify(&i64, &parse_type("or(:i64 :nothing)"));
        assert_eq!(unified, Some(i64));
    }
}
