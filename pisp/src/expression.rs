use crate::backend::Environment;
use pipes_rs::common::AnyError;
use std::fmt::{Debug, Display, Formatter};

pub type ResExpr = Result<Expression, AnyError>;
pub type Operation = fn(&mut Environment, &[Expression]) -> ResExpr;

pub const TRUE_STR: &str = "true";
pub const FALSE_STR: &str = "false";

#[derive(Eq, Clone)]
pub enum Expression {
    Nothing,
    Bool(bool),
    Number(i64),
    Symbol(String),
    NativeOperation(Operation),
    NonEvaluatingOperation(Operation),
    List(Vec<Expression>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use Expression::*;
        match self {
            Nothing => {
                write!(f, "none")
            }
            Bool(b) => {
                write!(f, "{}", if *b { "true" } else { "false" })
            }
            Number(n) => {
                write!(f, "{}", n)
            }
            Symbol(s) => {
                write!(f, "{}", s)
            }
            NativeOperation(_op) => {
                write!(f, "<native-function>")
            }
            NonEvaluatingOperation(_op) => {
                write!(f, "<native-non-evaluating-function>")
            }
            List(elements) => {
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
        }
    }
}
impl Debug for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
impl PartialEq for Expression {
    fn eq(&self, other: &Self) -> bool {
        use Expression::*;
        match (self, other) {
            (Nothing, Nothing) => true,
            (Bool(n), Bool(n2)) => n == n2,
            (Number(n), Number(n2)) => n == n2,
            (Symbol(s), Symbol(s2)) => s == s2,
            _ => false,
        }
    }
}
