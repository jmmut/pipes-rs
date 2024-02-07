use crate::frontend::expression::Type;

fn unify(first: &Type, second: &Type) -> Option<Type> {
    match (first, second) {
        (Type::Unknown, Type::Unknown) => Some(Type::Unknown),
        (
            Type::Simple {
                type_name: first_name,
            },
            Type::Simple {
                type_name: second_name,
            },
        ) => {
            if first_name == second_name {
                Some(second.clone())
            } else {
                None
            }
        }
        (
            Type::Builtin {
                type_name: first_name,
            },
            Type::Builtin {
                type_name: second_name,
            },
        ) => {
            if first_name == second_name {
                Some(second.clone())
            } else {
                None
            }
        }
        _ => None,
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
