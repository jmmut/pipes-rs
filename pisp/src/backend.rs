use crate::expression::{exprs_to_string, Expression, Function, Operation, ResExpr};
use pipes_rs::common::{err, AnyError};
use pipes_rs::frontend::sources::token::{Operator, EQUALS_ALT};
use std::collections::{BTreeSet, HashMap};
use std::iter::zip;

const ENV: &str = "env";
const DEF: &str = "def";
const SCOPE: &str = "scope";
const IF: &str = "if";
const FN: &str = "fn";
const EVAL: &str = "eval";
const LIST: &str = "list";

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
        env.insert(EQUALS_ALT.to_string(), nat(equals as Operation));
        env.insert(ENV.to_string(), nat(return_env as Operation));
        env.insert(EVAL.to_string(), nat(apply_eval as Operation));
        env.insert(LIST.to_string(), nat(apply_list as Operation));
        // native_operations.insert("*".to_string(), multiply as Operation);  // unsupported by the lexer
        // native_operations.insert("/".to_string(), divide as Operation);

        env.insert(DEF.to_string(), non_eval(apply_def as Operation));
        env.insert(SCOPE.to_string(), non_eval(apply_scope as Operation));
        env.insert(IF.to_string(), non_eval(apply_if as Operation));
        env.insert(FN.to_string(), non_eval(apply_new_fn as Operation));
        let scopes = vec![env];
        Self { scopes }
    }

    pub fn eval(&mut self, expression: &Expression) -> ResExpr {
        let mut expr = expression.clone();
        if let Expression::List(list) = expr {
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
        if let Expression::NativeOperation(native) = function {
            self.apply_native(native, arguments)
        } else if let Expression::NonEvaluatingOperation(native) = function {
            self.apply_non_evaluating(native, arguments)
        } else if let Expression::Function(function) = function {
            apply_user_func(self, function, arguments)
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
    fn new_env_from_closure(&mut self, closure: HashMap<String, Expression>) {
        self.scopes.push(closure);
    }
    fn drop_env(&mut self) {
        self.scopes.pop();
    }
}

fn apply_def(env: &mut Environment, arguments: &[Expression]) -> ResExpr {
    if let [Expression::Symbol(name), value] = arguments {
        // env.set(name.clone(), value.clone());
        // let evaluated = env.eval(value)?;
        // env.set(name.clone(), evaluated); // reserve name for recursive definitions to find themselves
        let evaluated = env.eval(value)?;
        Ok(env.set(name.clone(), evaluated))
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
    } else {
        let evaluated = env.eval(&arguments[0])?;
        if let Expression::Bool(cond) = evaluated {
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
}
fn apply_new_fn(env: &mut Environment, arguments: &[Expression]) -> ResExpr {
    if arguments.len() != 2 {
        err(format!(
            "{} needs a parameter list and a body expression, got {}",
            FN,
            exprs_to_string(arguments)
        ))
    } else {
        let parameters = Box::new(arguments[0].clone());
        let body = Box::new(arguments[1].clone());
        let closure = HashMap::new();
        let mut closure_2 = HashMap::new();
        let mut ignorable = HashMap::new();
        let mut function = Function {
            parameters,
            body,
            closure,
        };
        copy_free_vars_func(&function, &env, &mut closure_2, &mut ignorable)?;
        std::mem::swap(&mut function.closure, &mut closure_2);
        let expr = Expression::Function(function);
        Ok(expr)
    }
}
fn apply_user_func(env: &mut Environment, function: Function, arguments: &[Expression]) -> ResExpr {
    if let Expression::List(params) = &*function.parameters {
        if params.len() != arguments.len() {
            err(format!("to call a function, the number of arguments and parameter must be the same:\nparameters: {}\narguments:  {}", 
                       exprs_to_string(&params), exprs_to_string(arguments)))
        } else {
            env.new_env_from_closure(function.closure.clone());
            for (param, arg) in zip(params, arguments) {
                if let Expression::Symbol(name) = param {
                    let evaluated_arg = env.eval(arg)?;
                    env.set(name.clone(), evaluated_arg);
                } else {
                    return err(format!(
                        "to call a function all parameters must be symbols, got: {}",
                        param
                    ));
                }
            }
            let result = env.eval(&*function.body);
            env.drop_env();
            result
        }
    } else {
        err(format!(
            "to call a function the parameters must be a list, got: {}",
            function.parameters
        ))
    }
}

fn copy_free_vars(
    expr: &Expression,
    env: &Environment,
    closure: &mut HashMap<String, Expression>,
    ignorable: &mut HashMap<String, i32>,
) -> Result<(), AnyError> {
    match expr {
        Expression::Nothing => {}
        Expression::Bool(_) => {}
        Expression::Number(_) => {}
        Expression::Symbol(name) => copy_if_missing(name, env, closure, ignorable)?,
        Expression::NativeOperation(_) => {}
        Expression::NonEvaluatingOperation(_) => {}
        Expression::List(elems) => {
            let mut to_remove = Vec::new();
            if let Some(Expression::Symbol(first)) = elems.first() {
                if first == DEF {
                    let name = elems[1].as_symbol()?;
                    *ignorable.entry(name.clone()).or_insert(0) += 1;
                    to_remove.push(name);
                } else if first == FN {
                    for param in elems[1].as_exprs()? {
                        let name = param.as_symbol()?;
                        *ignorable.entry(name.clone()).or_insert(0) += 1;
                        to_remove.push(name);
                    }
                }
            }
            for elem in elems {
                copy_free_vars(elem, env, closure, ignorable)?
            }
            for removable in to_remove {
                remove_from_ignorable(&removable, ignorable);
            }
        }
        Expression::Function(func) => {
            copy_free_vars_func(&func, env, closure, ignorable)?;
        }
    }
    Ok(())
}

fn copy_free_vars_func(
    func: &Function,
    env: &Environment,
    closure: &mut HashMap<String, Expression>,
    ignorable: &mut HashMap<String, i32>,
) -> Result<(), AnyError> {
    for param in func.params()? {
        *ignorable.entry(param.as_symbol()?).or_insert(0) += 1;
    }
    copy_free_vars(&func.body, env, closure, ignorable)?;
    for param in func.params()? {
        let name = param.as_symbol()?;
        remove_from_ignorable(&name, ignorable);
    }
    Ok(())
}

fn remove_from_ignorable(name: &String, ignorable: &mut HashMap<String, i32>) {
    let definition_count = ignorable.get_mut(name).unwrap();
    if *definition_count == 1 {
        ignorable.remove(name);
    } else {
        *definition_count -= 1;
    }
}

fn copy_if_missing(
    name: &String,
    env: &Environment,
    closure: &mut HashMap<String, Expression>,
    ignorable: &mut HashMap<String, i32>,
) -> Result<(), AnyError> {
    if !closure.contains_key(name) && !ignorable.contains_key(name) {
        if let Some(value) = env.get(name) {
            closure.insert(name.clone(), value);
            Ok(())
        } else {
            err(format!(
                "can not capture symbol '{}' because it is undefined",
                name
            ))
        }
    } else {
        Ok(())
    }
}

pub fn return_env(env: &mut Environment, arguments: &[Expression]) -> ResExpr {
    if arguments.len() != 0 {
        return err(format!("{} does not take any arguments", ENV));
    }
    let mut union = BTreeSet::new();
    for scope in &env.scopes {
        for key in scope.keys() {
            union.insert(key);
        }
    }

    let mut list = Vec::new();
    for key in union {
        let pair = vec![Expression::Symbol(key.clone()), env.get(key).unwrap()];
        list.push(Expression::List(pair));
    }
    Ok(Expression::List(list))
}

pub fn apply_eval(env: &mut Environment, arguments: &[Expression]) -> ResExpr {
    let mut result = Expression::Nothing;
    for arg in arguments {
        result = env.eval(arg)?;
    }
    Ok(result)
}

pub fn apply_list(_env: &mut Environment, arguments: &[Expression]) -> ResExpr {
    Ok(Expression::List(arguments.to_vec()))
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

fn equals(_env: &mut Environment, arguments: &[Expression]) -> ResExpr {
    if arguments.len() < 2 {
        err("Equality needs 2 or more arguments".to_string())
    } else {
        let mut test = Expression::Nothing;
        let mut first = true;
        for arg in arguments {
            if first {
                first = false;
                test = arg.clone();
            } else {
                if *arg != test {
                    return Ok(Expression::Bool(false));
                }
            }
        }
        Ok(Expression::Bool(true))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::frontend;
    use crate::frontend::tests::n;
    use pipes_rs::common::unwrap_display;

    fn eval(expression: &Expression) -> ResExpr {
        let mut env = Environment::new();
        env.eval(expression)
    }
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
        assert_eq!(interpret("(def a 3)"), n(3));
        assert_eq!(interpret("(def a (+ 3 4))"), n(7));
    }
    #[test]
    fn define_evaluates_value() {
        let expression = unwrap_display(frontend("(def a (+ 3 4))"));
        let mut env = Environment::new();
        let _result = unwrap_display(env.eval(&expression));
        let expression = unwrap_display(frontend("a"));
        let result = unwrap_display(env.eval(&expression));
        assert_eq!(result, n(7))
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
        assert_eq!(interpret("(if true 3)"), n(3));
        assert_eq!(interpret("(if false 3)"), Expression::Nothing);
        assert_eq!(interpret("(if (== 1 0) 3 4)"), n(4));
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
    #[test]
    fn test_function_closure() {
        assert_eq!(interpret("( ( (fn (a) (fn (b) (+ a b))) 5) 7)"), n(12));
    }
    #[test]
    fn print_env() {
        interpret("(env)");
    }
    #[test]
    #[ignore]
    fn recursive() {
        let fib =
            "(def fib (fn (N) (if (== N 0) 1 (if (== N 1) 1 (+ (fib (- N 1)) (fib (- N 2)))))))";
        let code = format!("(scope {} (fib 4))", fib);
        assert_eq!(interpret(&code), n(5));
    }
    #[test]
    fn almost_recursive() {
        let fib =
            "(def fib (fn (self N) (if (== N 0) 1 (if (== N 1) 1 (+ (self self (- N 1)) (self self (- N 2)))))))";
        let code = format!("(scope {} (fib fib 4))", fib);
        assert_eq!(interpret(&code), n(5));
    }
    #[test]
    fn test_eval() {
        let code = "(scope (def mal_prog (list + 1 2)) (eval mal_prog))";
        assert_eq!(interpret(&code), n(3));
    }
}
