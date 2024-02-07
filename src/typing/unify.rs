use crate::frontend::expression::Type;

pub fn unify(first: &Type, second: &Type) -> Option<Type> {
    match (first, second) {
        (Type::Unknown, Type::Unknown) => Some(Type::Unknown),
        (Type::Simple { type_name: name_1 }, Type::Simple { type_name: name_2 }) => {
            clone_if_equal(second, name_1, name_2)
        }
        (Type::Builtin { type_name: name_1 }, Type::Builtin { type_name: name_2 }) => {
            clone_if_equal(second, name_1, name_2)
        }
        _ => None,
    }
}

fn clone_if_equal<T: PartialEq>(to_clone: &Type, first: T, second: T) -> Option<Type> {
    if first == second {
        Some(to_clone.clone())
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::expression::Type;
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
}
