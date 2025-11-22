use crate::expression::{Atom, Expression};
use pipes_rs::common::{err, AnyError};
use pipes_rs::frontend::sources::token::Operator;
use std::collections::HashMap;

type ResExpr = Result<Expression, AnyError>;
type Operation = fn(&[Expression]) -> ResExpr;

pub fn eval(expression: &Expression) -> ResExpr {
    let mut env = Environment::new();
    env.eval(expression)
}
struct Environment {
    native_symbols: HashMap<String, Operation>,
}

impl Environment {
    pub fn new() -> Self {
        let mut native_symbols = HashMap::new();
        native_symbols.insert(Operator::Add.to_string(), add as Operation);
        native_symbols.insert(Operator::Substract.to_string(), substract as Operation);
        native_symbols.insert(Operator::Multiply.to_string(), multiply as Operation);
        native_symbols.insert(Operator::Divide.to_string(), divide as Operation);
        // native_symbols.insert("*".to_string(), multiply as Operation);  // unsupported by the lexer
        // native_symbols.insert("/".to_string(), divide as Operation);
        Self { native_symbols }
    }

    pub fn eval(&mut self, expression: &Expression) -> ResExpr {
        let mut expr = expression.clone();
        while let Expression::List(list) = expr {
            expr = self.apply_list(&list)?;
        }
        Ok(expr)
    }

    fn apply_list(&mut self, list: &Vec<Expression>) -> ResExpr {
        self.apply(&list[0], &list[1..])
    }

    fn apply(&mut self, function: &Expression, arguments: &[Expression]) -> ResExpr {
        let function = self.eval(function)?;
        if let Expression::Atom(Atom::Symbol(symbol)) = function {
            if let Some(operation) = self.native_symbols.get(&symbol).cloned() {
                let mut evaluated_args = Vec::new();
                for arg in arguments {
                    evaluated_args.push(self.eval(arg)?);
                }
                operation(&evaluated_args)
            } else {
                err(format!("undefined operation: {}", symbol))
            }
        } else {
            err(format!("unsupported operation: {}", function))
        }
    }
}
fn add(arguments: &[Expression]) -> ResExpr {
    if arguments.len() == 0 {
        err("Addition needs 1 or more arguments".to_string())
    } else {
        let mut accum = 0;
        for arg in arguments {
            if let Expression::Atom(Atom::Number(n)) = arg {
                accum += n;
            } else {
                return err(format!("Can not add, not a number: {}", arg));
            }
        }
        Ok(Expression::Atom(Atom::Number(accum)))
    }
}
fn substract(arguments: &[Expression]) -> ResExpr {
    if arguments.len() == 0 {
        err("Substraction needs 1 or more arguments".to_string())
    } else if arguments.len() == 1 {
        if let Expression::Atom(Atom::Number(n)) = arguments[0] {
            Ok(Expression::Atom(Atom::Number(-n)))
        } else {
            return err(format!("Can not negate, not a number: {}", arguments[0]));
        }
    } else {
        let mut accum = 0;
        let mut first = true;
        for arg in arguments {
            if let Expression::Atom(Atom::Number(n)) = arg {
                if first {
                    first = false;
                    accum += n;
                } else {
                    accum -= n;
                }
            } else {
                return err(format!("Can not substract, not a number: {}", arg));
            }
        }
        Ok(Expression::Atom(Atom::Number(accum)))
    }
}
fn multiply(arguments: &[Expression]) -> ResExpr {
    if arguments.len() == 0 {
        err("Multiplication needs 1 or more arguments".to_string())
    } else {
        let mut accum = 1;
        for arg in arguments {
            if let Expression::Atom(Atom::Number(n)) = arg {
                accum *= n;
            } else {
                return err(format!("Can not multiply, not a number: {}", arg));
            }
        }
        Ok(Expression::Atom(Atom::Number(accum)))
    }
}
fn divide(arguments: &[Expression]) -> ResExpr {
    let mut dividend = 0;
    let mut divisor = 0;
    let mut first = true;
    for arg in arguments {
        if let Expression::Atom(Atom::Number(n)) = arg {
            if first {
                first = false;
                dividend += n;
            } else {
                divisor += n;
            }
        } else {
            return err(format!("Can not divide, not a number: {}", arg));
        }
    }
    if divisor == 0 {
        err(format!(
            "Can not divide by 0: {}",
            Expression::List(arguments.to_vec())
        ))
    } else {
        Ok(Expression::Atom(Atom::Number(dividend / divisor)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::frontend;
    use pipes_rs::common::unwrap_display;

    fn interpret(code: &str) -> Expression {
        let expression = unwrap_display(frontend(code));
        let result = unwrap_display(eval(&expression));
        result
    }
    fn assert_incorrect(code: &str) {
        // frontend(code)
        //     .map(|expr| eval(&expr))
        //     .expect_err("should fail");
        frontend(code)
            .and_then(|expr| eval(&expr))
            .expect_err("should fail, but got");
    }
    #[test]
    fn addition() {
        assert_eq!(interpret("(+ 2 3 4)"), Expression::Atom(Atom::Number(9)));
    }
    #[test]
    fn addition_no_args() {
        assert_incorrect("(+)")
    }
    #[test]
    fn substraction() {
        assert_eq!(interpret("(- 6 3 4)"), Expression::Atom(Atom::Number(-1)));
    }
    #[test]
    fn substraction_no_args() {
        assert_incorrect("(-)")
    }
    #[test]
    fn substraction_1_arg() {
        assert_eq!(interpret("(- 1)"), Expression::Atom(Atom::Number(-1)));
    }
    #[test]
    fn multiplication() {
        assert_eq!(
            interpret("(|* 2 3 2 3)"),
            Expression::Atom(Atom::Number(36))
        );
    }
    #[test]
    fn multiplication_no_args() {
        assert_incorrect("(|*)")
    }
    #[test]
    fn division() {
        assert_eq!(interpret("(|/ 9 4)"), Expression::Atom(Atom::Number(2)));
    }
    #[test]
    fn division_no_args() {
        assert_incorrect("(|/)")
    }
    #[test]
    fn division_no_divisor() {
        assert_incorrect("(|/ 5)")
    }

    #[test]
    fn nested_eval() {
        assert_eq!(
            interpret("(+ 1 (+ 2 3) 4)"),
            Expression::Atom(Atom::Number(10))
        );
    }
}
