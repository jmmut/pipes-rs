use crate::backend::Environment;
use pipes_rs::common::{err, AnyError};
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter, Write};

pub type ResExpr = Result<Expression, AnyError>;
pub type Operation = fn(&mut Environment, &[Expression]) -> ResExpr;
pub type Expressions = Vec<Expression>;

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
    List(Expressions),
    Function(Function),
}
impl Expression {
    pub fn as_symbol(&self) -> Result<String, AnyError> {
        if let Expression::Symbol(name) = self {
            Ok(name.clone())
        } else {
            err(format!("expected a symbol but got {}", self))
        }
    }
    pub fn as_exprs(&self) -> Result<Expressions, AnyError> {
        if let Expression::List(exprs) = self {
            Ok(exprs.clone())
        } else {
            err(format!("expected a list of expressions but got {}", self))
        }
    }
}
#[derive(Eq, Clone)]
pub struct Function {
    pub parameters: Box<Expression>,
    pub body: Box<Expression>,
    pub closure: HashMap<String, Expression>,
}
impl Function {
    pub fn params(&self) -> Result<Expressions, AnyError> {
        self.parameters.as_exprs()
    }
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
            List(elements) => fmt_expressions(elements, f),
            Function(func) => {
                write!(f, "(fn {} {})", func.parameters, func.body)
            }
        }
    }
}

fn fmt_expressions(exprs: &[Expression], f: &mut impl Write) -> std::fmt::Result {
    if exprs.len() == 0 {
        write!(f, "()")
    } else {
        write!(f, "(")?;
        let mut iter = exprs.iter();
        write!(f, "{}", iter.next().unwrap())?;
        for elem in iter {
            write!(f, " {}", elem)?;
        }
        write!(f, ")")
    }
}
pub fn exprs_to_string(exprs: &[Expression]) -> String {
    let mut buf = String::new();
    fmt_expressions(exprs, &mut buf).unwrap();
    buf
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
            (List(l), List(l2)) => l == l2,
            _ => false,
        }
    }
}

impl PartialEq for Function {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}
