use std::collections::HashSet;

use crate::frontend::expression::{Type, TypedIdentifier, TypedIdentifiers};
use crate::middleend::intrinsics::builtin_types::NOTHING;
use crate::middleend::intrinsics::{builtin_types, BuiltinType};

/// Compare types semantically and merge them.
/// In summary, the `unknown` type can be casted implicitly to all types, and this function
/// can be used to infer types. For example, the types `function(:i64) (:unknown)` and
/// `function(:unknown) (:i64)` are compatible, and this function would return `function(:i64) (:i64)`.
/// Order is significant: this function returns a specialization of "second" where any "first" will work.
/// This is similar to "second" being an interface that "first" implements. In other words,
/// if this function returns some type, you can pass a "first" to a function taking a "second"
/// and it will behave as expected.
/// TODO: this "interface" idea doesn't seem to hold based on tests like :i64 doesn't unify with :or(:i64 :nothing)
/// TODO: consider splitting :any into :any (interface of all types) and :unknown (unifyable with everything)
pub fn unify(first: &Type, second: &Type) -> Option<Type> {
    let first_name = first.name();
    let second_name = second.name();
    if first_name == BuiltinType::Unknown.name() {
        return Some(second.clone());
    } else if second_name == BuiltinType::Unknown.name() {
        return Some(first.clone());
    } else if first_name == BuiltinType::Any.name() || second_name == BuiltinType::Any.name() {
        return Some(builtin_types::ANY);
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
            Type::Function { parameters: param_1, returned: returned_1 },
            Type::Function { parameters: param_2, returned: returned_2 },
        ) => {
            //TODO reverse? if param1 is an interface of param2, then f1 can be used in all places (and more) where f2 is used
            let parameters = unify_typed_identifiers(param_1, param_2)?;

            let returned = Box::new(unify_typed_identifier(returned_1, returned_2)?);
            Some(Type::Function { parameters, returned })
        }
        (_, _) => None,
    }
}

fn try_or(first: &Type, second: &Type) -> Option<Type> {
    let first_is_or = first.name() == BuiltinType::Or.name();
    let second_is_or = second.name() == BuiltinType::Or.name();
    if first_is_or && second_is_or {
        if let (Type::Nested { children: or_1, .. }, Type::Nested { children: or_2, .. }) =
            (first, second)
        {
            let mut sorted_1 = or_1.clone();
            sort_typed_identifiers(&mut sorted_1);
            let mut sorted_2 = or_2.clone();
            sort_typed_identifiers(&mut sorted_2);
            if let Some(unified) = unify_typed_identifiers(&sorted_1, &sorted_2) {
                return Some(Type::from(BuiltinType::Or.name(), unified));
            }
        } else {
            panic!("Found a :or without inner types. Not sure if this should be allowed");
        }
    } else {
        if first_is_or {
            if let Some(unified) = children_unify_to(first, second) {
                return unified;
            }
        } else if second_is_or {
            if let Some(unified) = children_unify_to(second, first) {
                return unified;
            }
        }
    }
    None
}

fn children_unify_to(nested: &Type, simple: &Type) -> Option<Option<Type>> {
    if let Type::Nested { children, .. } = nested {
        if let Some(unified) = all_unify_to(&children, simple.clone()) {
            return Some(Some(unified.type_));
        }
    } else {
        panic!("Found a :or without inner types. Not sure if this should be allowed");
    }
    None
}

fn sort_type(type_: &mut Type) {
    match type_ {
        Type::Simple { .. } => {}
        Type::Nested { children, .. } => {
            sort_typed_identifiers(children);
        }
        Type::Function { .. } => {}
    }
}

fn sort_typed_identifiers(typed_identifiers: &mut TypedIdentifiers) {
    typed_identifiers.sort_by(|t_1, t_2| t_1.type_.name().cmp(&t_2.type_.name()));
    for child in typed_identifiers {
        sort_type(&mut child.type_);
    }
}

pub fn join_or(first: &Type, second: &Type) -> Type {
    let mut set = HashSet::new();
    insert_simple_or_children_of_or(first, &mut set);
    insert_simple_or_children_of_or(second, &mut set);

    let mut sorted = set.into_iter().collect();
    sort_typed_identifiers(&mut sorted);
    Type::from(BuiltinType::Or.name(), sorted)
}

fn insert_simple_or_children_of_or(type_: &Type, set: &mut HashSet<TypedIdentifier>) {
    if type_.name() == BuiltinType::Or.name() {
        if let Type::Nested { children, .. } = type_ {
            for child in children {
                set.insert(child.clone());
            }
        } else {
            panic!("Found a :or without inner types. Not sure if this should be allowed");
        }
    } else {
        set.insert(TypedIdentifier::nameless(type_.clone()));
    }
}

pub fn is_something_or_nothing(type_: &Type) -> Option<TypedIdentifier> {
    if let Type::Nested {
        type_name,
        children,
    } = type_
    {
        if type_name.name() == BuiltinType::Or.name() && children.len() == 2 {
            if children[0].type_ == NOTHING && children[1].type_ != NOTHING {
                Some(children[1].clone())
            } else if children[0].type_ != NOTHING && children[1].type_ == NOTHING {
                Some(children[0].clone())
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
    use crate::frontend::expression::{Type, TypedIdentifier};
    use crate::frontend::parse_type;
    use crate::middleend::typing::builtin_types;

    use super::*;

    #[test]
    fn test_basic_unifiable() {
        let unified = unify(&Type::nothing(), &Type::nothing());
        assert_eq!(unified, Some(Type::nothing()));

        let unified = unify(&Type::simple("i64"), &builtin_types::I64);
        assert_eq!(unified, Some(builtin_types::I64));

        let unified = unify(&Type::simple("custom"), &Type::simple("custom"));
        assert_eq!(unified, Some(Type::simple("custom")));
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
        assert_unifies("list(:unknown)", "array(:i64)");
        assert_unifies("list", "array");
        assert_unifies("array(:i64)", "list(:i64)");
        assert_unifies_to("array(:i64)", "list(:unknown)", "list(:i64)");
        assert_unifies_to("array(:i64)", "list(:any)", "list(:any)");
        assert_unifies_to("array(:i64)", "list", "list(:i64)");

        assert_no_unify("list(:i64)", "tuple(:i64)");
        assert_no_unify("list(:unknown)", "tuple(:i64)");
        assert_no_unify("list", "tuple");
        assert_unifies("tuple(:i64)", "list(:i64)");
        assert_unifies("tuple(:i64 :i64)", "list(:i64)");
        assert_unifies_to("tuple(:i64 :i64)", "list(:any)", "list(:any)");
        assert_unifies_to("tuple(:i64 :i64)", "list(:unknown)", "list(:i64)");
        assert_unifies_to("tuple(:i64 :i64)", "list", "list(:i64)");

        assert_unifies("tuple(:i64 :function)", "list(:any)");
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
                TypedIdentifier::nameless(builtin_types::UNKNOWN),
                TypedIdentifier::nameless(builtin_types::I64),
            ],
        );
        let unified = unify(
            &Type::children(
                "mystruct",
                vec![
                    TypedIdentifier::nameless(builtin_types::I64),
                    TypedIdentifier::nameless(builtin_types::UNKNOWN),
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
        let i64_or_nothing = "or(:i64 :nothing)";
        assert_no_unify("i64", i64_or_nothing);
        assert_unifies_to(i64_or_nothing, i64_or_nothing, i64_or_nothing);
        assert_unifies_to(i64_or_nothing, "or(:nothing :i64)", i64_or_nothing);
        assert_unifies_to("or(:nothing :i64)", i64_or_nothing, i64_or_nothing);
        assert_unifies_to(i64_or_nothing, "or(:nothing :unknown)", i64_or_nothing);
    }
    #[test]
    fn test_simplify_or() {
        let tuple_or_array = "or(:tuple(:i64) :array(:any))";
        assert_unifies_to(tuple_or_array, "list", "list(:any)");
        let tuple_or_array = "or(:tuple(:i64) :array(:unknown))";
        assert_unifies_to(tuple_or_array, "list", "list(:i64)");
    }

    #[test]
    fn test_sort() {
        let mut t = parse_type("or(:i64 :nothing)");
        sort_type(&mut t);
        assert_eq!(t.to_string(), "or(:i64  :nothing)");

        let mut t = parse_type("or(:nothing :i64)");
        sort_type(&mut t);
        assert_eq!(t.to_string(), "or(:i64  :nothing)");
    }

    #[test]
    fn test_join_or() {
        let joined = join_or(&parse_type("i64"), &parse_type("nothing"));
        assert_eq!(joined.to_string(), "or(:i64  :nothing)");
        let joined_nested = join_or(&joined, &parse_type("nothing"));
        assert_eq!(joined_nested.to_string(), joined.to_string());
        let joined_nested = join_or(&joined, &parse_type("i64"));
        assert_eq!(joined_nested.to_string(), joined.to_string());

        let other_joined = join_or(&parse_type("function"), &parse_type("nothing"));
        let joined_nested = join_or(&joined, &other_joined);
        assert_eq!(
            joined_nested.to_string(),
            "or(:function()(:unknown)  :i64  :nothing)"
        );
    }

    #[test]
    fn test_unify_complex_or() {
        let concrete = "or(:nothing  :tuple(:i64  :i64))";
        assert_unifies_to(concrete, "or(:unknown  :nothing)", concrete);
    }
}
