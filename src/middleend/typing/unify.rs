use crate::frontend::expression::{Type, TypedIdentifier, TypedIdentifiers};
use crate::middleend::intrinsics::{builtin_types, BuiltinType};

/// Add to `second` whatever we can from `first`, and return a copy of it.
///
/// The fact that second takes preference is important because 2 types might be different but still
/// compatible, like `:array(:i64)` and `:tuple(:i64 :64)`.
pub fn unify(first: &Type, second: &Type) -> Option<Type> {
    let first_name = first.name();
    let second_name = second.name();
    if first_name == BuiltinType::Any.name() {
        return Some(second.clone());
    } else if second_name == BuiltinType::Any.name() {
        return Some(first.clone());
    }
    match (first, second) {
        (Type::Simple { .. }, Type::Simple { .. }) => {
            if first_name == second_name {
                Some(second.clone())
            } else {
                None
            }
        }
        #[rustfmt::skip]
        (Type::Nested { children: children_1, .. }, Type::Nested { children: children_2, .. }) => {
            if first_name == second_name {
                if let Some(children) = unify_list(children_1, children_2) {
                    Some(Type::from(second_name, children))
                } else {
                    None
                }
            } else {
                None
            }
        },
        (Type::Function { .. }, Type::Function { .. }) => {
            //TODO
            None
        }
        (_, _) => None,
    }
    //     {
    //     (Type::Unknown, Type::Unknown) => Some(Type::Unknown),
    //     (Type::Unknown, _) => Some(second.clone()),
    //     (_, Type::Unknown) => Some(first.clone()),
    //     (Type::Simple { .. }, Type::Simple { .. }) => {
    //         clone_if_equal(second, first.name(), second.name())
    //     }
    //     (Type::Builtin { .. }, Type::Builtin { .. }) => {
    //         clone_if_equal(second, first.name(), second.name())
    //     }
    //     (Type::Builtin { .. }, Type::Simple { .. })
    //     | (Type::Simple { .. }, Type::Builtin { .. }) => None,
    //     (Type::BuiltinSingle { .. }, _)
    //     | (Type::BuiltinSeveral { .. }, _)
    //     | (Type::NestedSingle { .. }, _)
    //     | (Type::NestedSeveral { .. }, _) => nested_unify(first, second),
    //     _ => {
    //         unreachable!(
    //             "Type check bug: missing combination in unify: {:?} and {:?}",
    //             first, second
    //         )
    //     }
    // }
}

fn clone_if_equal<T: PartialEq>(to_clone: &Type, first: T, second: T) -> Option<Type> {
    if first == second {
        Some(to_clone.clone())
    } else {
        None
    }
}

fn unify_typed_identifier(
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

fn unify_list(firsts: &TypedIdentifiers, seconds: &TypedIdentifiers) -> Option<TypedIdentifiers> {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::expression::{Type, TypedIdentifier};
    use crate::middleend::intrinsics::BuiltinType;
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
        assert_eq!(unified, Some(second));
    }
}
