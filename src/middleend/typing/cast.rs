use crate::frontend::expression::{Type, TypedIdentifier, TypedIdentifiers};
use crate::middleend::intrinsics::{builtin_types, BuiltinType};
use crate::middleend::typing::unify::all_same_type;

/// Add to `second` whatever we can from `first`, and return a copy of it.
///
/// The fact that second takes preference is important because 2 types might be different but still
/// compatible, like `:tuple(:i64 :64)` and `:array(:i64)`.
pub fn cast(first: &Type, second: &Type) -> Option<Type> {
    let first_name = first.name();
    let second_name = second.name();
    if first_name == BuiltinType::Any.name() {
        return Some(second.clone());
    } else if second_name == BuiltinType::Any.name() {
        return Some(first.clone());
    }
    match (first, second) {
        (Type::Simple { .. }, Type::Simple { .. }) => {
            clone_if_equal(second, first_name, second_name)
        }
        #[rustfmt::skip]
        (Type::Nested { children: children_1, .. }, Type::Nested { children: children_2, .. }) => {
            cast_nested(first_name, second_name, children_1, children_2)
        },
        (Type::Function { .. }, Type::Function { .. }) => {
            //TODO
            None
        }
        (_, _) => None,
    }
}

fn clone_if_equal<T: PartialEq>(to_clone: &Type, first: T, second: T) -> Option<Type> {
    if first == second {
        Some(to_clone.clone())
    } else {
        None
    }
}

fn cast_nested(
    first_name: &str,
    second_name: &str,
    children_1: &TypedIdentifiers,
    children_2: &TypedIdentifiers,
) -> Option<Type> {
    if first_name == second_name {
        if let Some(children) = cast_list(children_1, children_2) {
            Some(Type::from(second_name, children))
        } else {
            None
        }
    } else {
        if first_name == BuiltinType::Array.name() && second_name == BuiltinType::Tuple.name() {
            None // forbid casting array to tuple because the lengths might not match
        } else if first_name == BuiltinType::Tuple.name()
            && second_name == BuiltinType::Array.name()
        {
            if all_same_type(children_1) {
                let any = TypedIdentifier::nameless(builtin_types::ANY);
                let tuple_type = children_1.last().unwrap_or(&any);
                let array_type = children_2.last().unwrap();
                if let Some(unified_inner) = cast_typed_identifier(tuple_type, array_type) {
                    Some(Type::from(second_name, vec![unified_inner]))
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        }
    }
}

fn cast_typed_identifier(
    first: &TypedIdentifier,
    second: &TypedIdentifier,
) -> Option<TypedIdentifier> {
    if let Some(type_) = cast(&first.type_, &second.type_) {
        let name = cast_name(&first.name, &second.name);
        Some(TypedIdentifier { name, type_ })
    } else {
        None
    }
}

fn cast_name(first: &String, second: &String) -> String {
    if second.is_empty() {
        first.clone()
    } else {
        second.clone()
    }
}

fn cast_list(firsts: &TypedIdentifiers, seconds: &TypedIdentifiers) -> Option<TypedIdentifiers> {
    if firsts.len() == seconds.len() {
        let mut unified_types = Vec::new();
        for i in 0..firsts.len() {
            if let Some(unified) = cast_typed_identifier(&firsts[i], &seconds[i]) {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::expression::{Type, TypedIdentifier};
    use crate::middleend::typing::builtin_types;

    #[test]
    fn test_basic_unifiable() {
        let unified = cast(&Type::nothing(), &Type::nothing());
        assert_eq!(unified, Some(Type::nothing()));

        let unified = cast(&Type::simple("i64".to_string()), &builtin_types::I64);
        assert_eq!(unified, Some(builtin_types::I64));

        let unified = cast(
            &Type::simple("custom".to_string()),
            &Type::simple("custom".to_string()),
        );
        assert_eq!(unified, Some(Type::simple("custom".to_string())));
    }

    #[test]
    fn test_basic_different() {
        let unified = cast(&Type::simple("i64"), &Type::simple("float"));
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

        let unified = cast(
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
        let unified = cast(
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
        let unified = cast(&first, &second);
        assert_eq!(unified, None);
    }

    #[test]
    fn test_tuple_to_array() {
        let child = TypedIdentifier::nameless(builtin_types::I64);
        let first = Type::from("tuple", vec![child.clone(), child.clone()]);
        let second = Type::from("array", vec![child.clone()]);
        let unified = cast(&first, &second);
        assert_eq!(unified, Some(second));
    }
}
