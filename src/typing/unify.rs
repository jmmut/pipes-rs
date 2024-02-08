use crate::frontend::expression::{Type, TypedIdentifier, TypedIdentifiers};
use crate::typing::type_names;

/// Add to `second` whatever we can from `first`, and return a copy of it.
///
/// The fact that second takes preference is important because 2 types might be different but still
/// compatible, like `:array(:i64)` and `:tuple(:i64 :64)`.
pub fn unify(first: &Type, second: &Type) -> Option<Type> {
    match (first, second) {
        (Type::Unknown, Type::Unknown) => Some(Type::Unknown),
        (Type::Unknown, _) => Some(second.clone()),
        (_, Type::Unknown) => Some(first.clone()),
        (Type::Simple { .. }, Type::Simple { .. }) => {
            clone_if_equal(second, first.name(), second.name())
        }
        (Type::Builtin { .. }, Type::Builtin { .. }) => {
            clone_if_equal(second, first.name(), second.name())
        }
        (Type::Builtin { .. }, Type::Simple { .. })
        | (Type::Simple { .. }, Type::Builtin { .. }) => None,
        (Type::BuiltinSingle { .. }, _)
        | (Type::BuiltinSeveral { .. }, _)
        | (Type::NestedSingle { .. }, _)
        | (Type::NestedSeveral { .. }, _) => nested_unify(first, second),
        _ => {
            unreachable!(
                "Type check bug: missing combination in unify: {:?} and {:?}",
                first, second
            )
        }
    }
}

fn clone_if_equal<T: PartialEq>(to_clone: &Type, first: T, second: T) -> Option<Type> {
    if first == second {
        Some(to_clone.clone())
    } else {
        None
    }
}

fn nested_unify(first: &Type, second: &Type) -> Option<Type> {
    match (first, second) {
        #[rustfmt::skip]
        (
            Type::BuiltinSingle { child: child_1, .. },
            Type::BuiltinSingle { child: child_2, .. },
        ) => {
            if first.name() == second.name() {
                if let Some(unified) = unify_typed_identifier(&child_1, &child_2) {
                    return Some(Type::BuiltinSingle { type_name: second.static_name(), child: Box::new(unified)})
                }
            }
            None
        }
        #[rustfmt::skip]
        (
            Type::NestedSingle { child: child_1, .. },
            Type::NestedSingle { child: child_2, .. }
        ) => {
            if first.name() == second.name() {
                if let Some(unified) = unify_typed_identifier(&child_1, &child_2) {
                    return Some(Type::NestedSingle {
                        type_name: second.name().to_string(),
                        child: Box::new(unified),
                    });
                }
            }
            None
        }
        #[rustfmt::skip]
        (
            Type::BuiltinSeveral { children: children_1, .. },
            Type::BuiltinSeveral { children: children_2, .. },
        ) => {
            if first.name() == second.name() {
                if let Some(children) = unify_list(children_1, children_2) {
                    return Some(Type::BuiltinSeveral {type_name: second.static_name(), children})
                }
            }
            None
        }
        #[rustfmt::skip]
        (
            Type::NestedSeveral { children: children_1, .. },
            Type::NestedSeveral { children: children_2, .. },
        ) => {
            if first.name() == second.name() {
                if let Some(children) = unify_list(children_1, children_2) {
                    return Some(Type::NestedSeveral {
                        type_name: second.name().to_string(),
                        children,
                    });
                }
            }
            None
        }
        (Type::BuiltinSeveral {children, ..  }, Type::BuiltinSingle { child, ..}) => {
            if first.name() == type_names::TUPLE && children.len() > 0 && second.name() == type_names::ARRAY {
                let types = children.iter().map(|t| &t.type_).collect::<Vec<_>>();
                if all_same_type(&types) {
                    if let Some(unified) = unify_typed_identifier(&children[0], &*child) {
                        return Some(Type::BuiltinSingle {type_name: type_names::ARRAY, child: Box::new(unified)})
                    }
                }
            }
            None
        }
        _ => {
            None
        }
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

fn all_same_type(types: &Vec<&Type>) -> bool {
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
    use crate::typing::{builtin_types, type_names};

    #[test]
    fn test_basic_unifiable() {
        let unified = unify(&Type::Unknown, &Type::Unknown);
        assert_eq!(unified, Some(Type::Unknown));

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
        let unified = unify(
            &Type::Simple {
                type_name: "i64".to_string(),
            },
            &Type::Builtin {
                type_name: type_names::I64,
            },
        );
        assert_eq!(unified, None);
    }

    #[test]
    fn test_nested_single() {
        let second = Type::NestedSingle {
            type_name: "mystruct".to_string(),
            child: Box::new(TypedIdentifier {
                name: "field".to_string(),
                type_: builtin_types::I64,
            }),
        };
        let unified = unify(
            &Type::NestedSingle {
                type_name: "mystruct".to_string(),
                child: Box::new(TypedIdentifier {
                    name: "field".to_string(),
                    type_: builtin_types::I64,
                }),
            },
            &second,
        );
        assert_eq!(unified, Some(second));
        let second = Type::BuiltinSingle {
            type_name: type_names::STRUCT,
            child: Box::new(TypedIdentifier {
                name: "field".to_string(),
                type_: builtin_types::I64,
            }),
        };
        let unified = unify(
            &Type::BuiltinSingle {
                type_name: type_names::STRUCT,
                child: Box::new(TypedIdentifier {
                    name: "field".to_string(),
                    type_: builtin_types::I64,
                }),
            },
            &second,
        );
        assert_eq!(unified, Some(second));
    }

    #[test]
    fn test_nested_several() {
        let second = Type::NestedSeveral {
            type_name: "mystruct".to_string(),
            children: vec![
                TypedIdentifier {
                    name: "field".to_string(),
                    type_: builtin_types::I64,
                },
                TypedIdentifier {
                    name: "field_2".to_string(),
                    type_: builtin_types::I64,
                },
            ],
        };
        let unified = unify(
            &Type::NestedSeveral {
                type_name: "mystruct".to_string(),
                children: vec![
                    TypedIdentifier {
                        name: "field".to_string(),
                        type_: builtin_types::I64,
                    },
                    TypedIdentifier {
                        name: "field_2".to_string(),
                        type_: builtin_types::I64,
                    },
                ],
            },
            &second,
        );
        assert_eq!(unified, Some(second));
        let second = Type::BuiltinSeveral {
            type_name: type_names::STRUCT,
            children: vec![
                TypedIdentifier {
                    name: "field".to_string(),
                    type_: builtin_types::I64,
                },
                TypedIdentifier {
                    name: "field_2".to_string(),
                    type_: builtin_types::I64,
                },
            ],
        };
        let unified = unify(
            &Type::BuiltinSeveral {
                type_name: type_names::STRUCT,
                children: vec![
                    TypedIdentifier {
                        name: "field".to_string(),
                        type_: builtin_types::I64,
                    },
                    TypedIdentifier {
                        name: "field_2".to_string(),
                        type_: builtin_types::I64,
                    },
                ],
            },
            &second,
        );
        assert_eq!(unified, Some(second));
    }

    #[test]
    fn test_array_to_tuple() {
        let child = TypedIdentifier::nameless(builtin_types::I64);
        let first = Type::builtin("array", vec![child.clone()]);
        let second = Type::builtin("tuple", vec![child.clone(), child.clone()]);
        let unified = unify(&first, &second);
        assert_eq!(unified,None);
    }

    #[test]
    fn test_tuple_to_array() {
        let child = TypedIdentifier::nameless(builtin_types::I64);
        let first = Type::builtin("tuple", vec![child.clone(), child.clone()]);
        let second = Type::builtin("array", vec![child.clone()]);
        let unified = unify(&first, &second);
        assert_eq!(unified, Some(second));
    }
}
