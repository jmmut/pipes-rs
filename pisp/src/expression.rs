use crate::backend::Environment;
use pipes_rs::common::AnyError;
use std::fmt::{Debug, Display, Formatter};

pub type ResExpr = Result<Expression, AnyError>;
pub type Operation = fn(&mut Environment, &[Expression]) -> ResExpr;

#[derive(Eq, PartialEq, Clone)]
pub enum Expression {
    List(Vec<Expression>),
    Atom(Atom),
}

#[derive(Eq, Clone)]
pub enum Atom {
    Number(i64),
    Symbol(String),
    NativeOperation(Operation),
    NonEvaluatingOperation(Operation),
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::List(elements) => {
                if elements.len() == 0 {
                    write!(f, "()")
                } else {
                    write!(f, "(")?;
                    let mut iter = elements.iter();
                    write!(f, "{}", iter.next().unwrap())?;
                    for elem in iter {
                        write!(f, " {}", elem)?;
                    }
                    write!(f, ")")
                }
            }
            Expression::Atom(atom) => {
                write!(f, "{}", atom)
            }
        }
    }
}
impl Display for Atom {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::Number(n) => {
                write!(f, "{}", n)
            }
            Atom::Symbol(s) => {
                write!(f, "{}", s)
            }
            Atom::NativeOperation(_op) => {
                write!(f, "<native-function>")
            }
            Atom::NonEvaluatingOperation(_op) => {
                write!(f, "<native-non-evaluating-function>")
            }
        }
    }
}
impl Debug for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
impl Debug for Atom {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
impl PartialEq for Atom {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Atom::Number(n), Atom::Number(n2)) => n == n2,
            (Atom::Symbol(s), Atom::Symbol(s2)) => s == s2,
            _ => false,
        }
    }
}
