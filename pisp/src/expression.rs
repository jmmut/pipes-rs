use std::fmt::{Debug, Display, Formatter};

#[derive(Eq, PartialEq, Clone)]
pub enum Expression {
    List(Vec<Expression>),
    Atom(Atom),
}

#[derive(Eq, PartialEq, Clone)]
pub enum Atom {
    Number(i64),
    Symbol(String),
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
