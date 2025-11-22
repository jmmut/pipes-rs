use crate::expression::{exprs_to_string, Expression, Function, Operation, ResExpr};
use pipes_rs::common::{err, AnyError};
use pipes_rs::frontend::sources::token::Operator;
use std::collections::HashMap;

const DEF: &str = "def";
const SCOPE: &str = "scope";
const IF: &str = "if";
const FN: &str = "fn";

pub fn eval(expression: &Expression) -> ResExpr {
    let mut env = Environment::new();
    env.eval(expression)
}
pub struct Environment {
    scopes: Vec<HashMap<String, Expression>>,
}

impl Environment {
    pub fn new() -> Self {
        let mut env = HashMap::new();
        let nat = |op: Operation| Expression::NativeOperation(op);
        let non_eval = |op: Operation| Expression::NonEvaluatingOperation(op);
        env.insert(Operator::Add.to_string(), nat(add as Operation));
        env.insert(Operator::Substract.to_string(), nat(substract as Operation));
        env.insert(Operator::Multiply.to_string(), nat(multiply as Operation));
        env.insert(Operator::Divide.to_string(), nat(divide as Operation));
        // native_operations.insert("*".to_string(), multiply as Operation);  // unsupported by the lexer
        // native_operations.insert("/".to_string(), divide as Operation);
        env.insert(DEF.to_string(), non_eval(apply_def as Operation));
        env.insert(SCOPE.to_string(), non_eval(apply_scope as Operation));
        env.insert(IF.to_string(), non_eval(apply_if as Operation));
        env.insert(FN.to_string(), non_eval(apply_function as Operation));
        let scopes = vec![env];
        Self { scopes }
    }

    pub fn eval(&mut self, expression: &Expression) -> ResExpr {
        let mut expr = expression.clone();
        while let Expression::List(list) = expr {
            expr = self.apply_list(&list)?;
        }
        if let Expression::Symbol(name) = expr {
            if let Some(value) = self.get(&name) {
                Ok(value)
            } else {
                err(format!("undefined symbol: {}", name))
            }
        } else {
            Ok(expr)
        }
    }

    fn apply_list(&mut self, list: &Vec<Expression>) -> ResExpr {
        self.apply(&list[0], &list[1..])
    }

    fn apply(&mut self, function: &Expression, arguments: &[Expression]) -> ResExpr {
        let function = self.eval(function)?;
        // if let Expression::Symbol(symbol)) = function {
        //     if symbol == DEF {
        //         apply_def(self, arguments)
        //     } else {
        //         err(format!("undefined operation: {}", symbol))
        //     }
        // } else
        if let Expression::NativeOperation(native) = function {
            self.apply_native(native, arguments)
        } else if let Expression::NonEvaluatingOperation(native) = function {
            self.apply_non_evaluating(native, arguments)
        } else {
            err(format!("unsupported operation: {}", function))
        }
    }

    fn apply_native(&mut self, native: Operation, arguments: &[Expression]) -> ResExpr {
        let evaluated_args = self.eval_several(arguments)?;
        native(self, &evaluated_args)
    }

    fn eval_several(&mut self, arguments: &[Expression]) -> Result<Vec<Expression>, AnyError> {
        let mut evaluated_args = Vec::new();
        for arg in arguments {
            evaluated_args.push(self.eval(arg)?);
        }
        Ok(evaluated_args)
    }

    fn apply_non_evaluating(&mut self, native: Operation, arguments: &[Expression]) -> ResExpr {
        native(self, &arguments)
    }
    fn get(&self, symbol: &str) -> Option<Expression> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(symbol) {
                return Some(symbol.clone());
            }
        }
        None
    }
    fn set(&mut self, symbol: String, expression: Expression) -> Expression {
        self.scopes
            .last_mut()
            .unwrap()
            .insert(symbol, expression.clone());
        expression
    }
    fn new_env(&mut self) {
        self.scopes.push(HashMap::new());
    }
    fn drop_env(&mut self) {
        self.scopes.pop();
    }
}

fn apply_def(env: &mut Environment, arguments: &[Expression]) -> ResExpr {
    if let [Expression::Symbol(name), value] = arguments {
        Ok(env.set(name.clone(), value.clone()))
    } else {
        err(format!(
            "{} requires 2 arguments, the variable name and value. Got {}",
            DEF,
            exprs_to_string(arguments)
        ))
    }
}

fn apply_scope(env: &mut Environment, arguments: &[Expression]) -> ResExpr {
    env.new_env();
    let evaluated = env.eval_several(arguments);
    env.drop_env();
    Ok(evaluated?.into_iter().last().unwrap_or(Expression::Nothing))
}
fn apply_if(env: &mut Environment, arguments: &[Expression]) -> ResExpr {
    if arguments.len() != 2 && arguments.len() != 3 {
        err(format!(
            "{} needs a condition and 1 or 2 expressions, got {}: {}",
            IF,
            arguments.len(),
            exprs_to_string(arguments),
        ))
    } else if let Expression::Bool(cond) = arguments[0] {
        if cond {
            env.eval(&arguments[1])
        } else if arguments.len() == 3 {
            env.eval(&arguments[2])
        } else {
            Ok(Expression::Nothing)
        }
    } else {
        err(format!(
            "the first argument to 'if' must be bool, got {}",
            arguments[0]
        ))
    }
}
fn apply_function(env: &mut Environment, arguments: &[Expression]) -> ResExpr {
    if arguments.len() != 2 {
        err(format!(
            "{} needs a parameter list and a body expression, got {}",
            FN,
            exprs_to_string(arguments)
        ))
    } else {
        let parameters = Box::new(arguments[0].clone());
        let body = Box::new(arguments[1].clone());
        Ok(Expression::Function(Function { parameters, body }))
    }
}

fn add(_env: &mut Environment, arguments: &[Expression]) -> ResExpr {
    if arguments.len() == 0 {
        err("Addition needs 1 or more arguments".to_string())
    } else {
        let mut accum = 0;
        for arg in arguments {
            if let Expression::Number(n) = arg {
                accum += n;
            } else {
                return err(format!("Can not add, not a number: {}", arg));
            }
        }
        Ok(Expression::Number(accum))
    }
}
fn substract(_env: &mut Environment, arguments: &[Expression]) -> ResExpr {
    if arguments.len() == 0 {
        err("Substraction needs 1 or more arguments".to_string())
    } else if arguments.len() == 1 {
        if let Expression::Number(n) = arguments[0] {
            Ok(Expression::Number(-n))
        } else {
            return err(format!("Can not negate, not a number: {}", arguments[0]));
        }
    } else {
        let mut accum = 0;
        let mut first = true;
        for arg in arguments {
            if let Expression::Number(n) = arg {
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
        Ok(Expression::Number(accum))
    }
}
fn multiply(_env: &mut Environment, arguments: &[Expression]) -> ResExpr {
    if arguments.len() == 0 {
        err("Multiplication needs 1 or more arguments".to_string())
    } else {
        let mut accum = 1;
        for arg in arguments {
            if let Expression::Number(n) = arg {
                accum *= n;
            } else {
                return err(format!("Can not multiply, not a number: {}", arg));
            }
        }
        Ok(Expression::Number(accum))
    }
}
fn divide(_env: &mut Environment, arguments: &[Expression]) -> ResExpr {
    let mut dividend = 0;
    let mut divisor = 0;
    let mut first = true;
    for arg in arguments {
        if let Expression::Number(n) = arg {
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
        Ok(Expression::Number(dividend / divisor))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::frontend;
    use crate::frontend::tests::n;
    use pipes_rs::common::unwrap_display;

    fn interpret(code: &str) -> Expression {
        let expression = unwrap_display(frontend(code));
        let result = unwrap_display(eval(&expression));
        result
    }
    fn assert_incorrect(code: &str) {
        frontend(code)
            .and_then(|expr| eval(&expr))
            .expect_err("should fail, but got");
    }
    #[test]
    fn addition() {
        assert_eq!(interpret("(+ 2 3 4)"), n(9));
    }
    #[test]
    fn addition_no_args() {
        assert_incorrect("(+)")
    }
    #[test]
    fn substraction() {
        assert_eq!(interpret("(- 6 3 4)"), n(-1));
    }
    #[test]
    fn substraction_no_args() {
        assert_incorrect("(-)")
    }
    #[test]
    fn substraction_1_arg() {
        assert_eq!(interpret("(- 1)"), n(-1));
    }
    #[test]
    fn multiplication() {
        assert_eq!(interpret("(|* 2 3 2 3)"), n(36));
    }
    #[test]
    fn multiplication_no_args() {
        assert_incorrect("(|*)")
    }
    #[test]
    fn division() {
        assert_eq!(interpret("(|/ 9 4)"), n(2));
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
        assert_eq!(interpret("(+ 1 (+ 2 3) 4)"), n(10));
    }

    #[test]
    fn define() {
        assert_eq!(interpret("(def a 3)"), n(3))
    }
    #[test]
    fn scope() {
        assert_eq!(interpret("(scope (def a 3) a)"), n(3));
        assert_eq!(interpret("(scope (def a 3) (scope (def a 4) a))"), n(4));
        assert_eq!(interpret("(scope (def a 3) (scope (def a 4) a) a)"), n(3));
    }
    #[test]
    fn test_if() {
        assert_eq!(interpret("(if true 3 a)"), n(3));
        assert_incorrect("(if false 3 a)");
    }
    #[test]
    fn test_function() {
        let func = "(fn (a) (+ a 1))";
        assert_eq!(interpret(func).to_string(), func);
    }
    #[test]
    fn test_function_call() {
        assert_eq!(interpret("((fn (a) (+ a 1)) 4)"), n(5));
    }
}
