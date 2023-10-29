use crate::common::context;
use crate::frontend::expression::{Expression, Expressions, StaticList, Transformation};
use crate::frontend::lexer::{Operator, Token, Tokens};
use crate::AnyError;

pub fn parse(tokens: Tokens) -> Result<Expression, AnyError> {
    let mut parser = Parser::new(tokens.into_iter());
    context("Parser", parser.parse_chain())
}
struct Parser<I: Iterator<Item = Token>> {
    iter: I,
    brace_nesting: i64,
}

impl<I: Iterator<Item = Token>> Parser<I> {
    pub fn new(iter: I) -> Self {
        Self {
            iter,
            brace_nesting: 0,
        }
    }
    pub fn parse_chain(&mut self) -> Result<Expression, AnyError> {
        let nesting = self.brace_nesting;
        let initial = self.parse_expression()?.unwrap_or(Expression::Nothing);
        if self.brace_nesting == nesting - 1 {
            Ok(initial)
        } else {
            self.complete_chain(initial)
        }
    }

    fn complete_chain(&mut self, initial: Expression) -> Result<Expression, AnyError> {
        let mut transformations = Vec::new();
        while let Some(transformation) = self.parse_transformation()? {
            transformations.push(transformation);
        }
        if transformations.is_empty() {
            Ok(initial)
        } else {
            let top_level = Expression::Chain {
                initial: Box::new(initial),
                transformations,
            };
            Ok(top_level)
        }
    }

    fn parse_transformation(&mut self) -> Result<Option<Transformation>, AnyError> {
        if let Some(token) = self.iter.next() {
            match token {
                Token::Operator(operator) => self.complete_transformation(operator),
                Token::CloseBrace => self.balance_chain_braces(&token),
                _ => Err(format!("unexpected token {:?}, expected operator", token))?,
            }
        } else {
            Ok(None)
        }
    }

    fn complete_transformation(
        &mut self,
        operator: Operator,
    ) -> Result<Option<Transformation>, AnyError> {
        if let Some(operand) = self.parse_expression()? {
            let transformation = Transformation { operator, operand };
            Ok(Some(transformation))
        } else {
            Err(format!(
                "unfinished operation after operator {:?}",
                operator
            ))?
        }
    }

    fn balance_chain_braces<T>(&mut self, token: &Token) -> Result<Option<T>, AnyError> {
        if self.brace_nesting > 0 {
            self.brace_nesting -= 1;
            Ok(None)
        } else {
            Err(format!("unmatched {:?}", token).into())
        }
    }

    fn parse_expression(&mut self) -> Result<Option<Expression>, AnyError> {
        if let Some(token) = self.iter.next() {
            Ok(Some(self.parse_expression_from_token(token)?))
        } else {
            Ok(None)
        }
    }

    fn parse_expression_from_token(&mut self, token: Token) -> Result<Expression, AnyError> {
        let expression = match token.clone() {
            Token::Number(n) => Expression::Value(n),
            Token::Identifier(name) => Expression::Identifier(name),
            Token::OpenBracket => self.parse_list()?,
            Token::OpenBrace => self.parse_braced_chain()?,
            Token::CloseBrace => {
                self.balance_chain_braces::<()>(&token)?;
                Expression::Nothing
            }
            _ => return Err(format!("unexpected token {:?}, expected expression", token))?,
        };
        Ok(expression)
    }

    fn parse_list(&mut self) -> Result<Expression, AnyError> {
        let mut elements = Expressions::new();
        while let Some(token) = self.iter.next() {
            match token {
                Token::CloseBracket => return Ok(Expression::StaticList(StaticList { elements })),
                _ => elements.push(self.parse_expression_from_token(token)?),
            }
        }
        Err("Unclosed square bracket")?
    }

    fn parse_braced_chain(&mut self) -> Result<Expression, AnyError> {
        let initial_nesting = self.brace_nesting;
        self.brace_nesting += 1;
        let chain = self.parse_chain()?;
        if self.brace_nesting != initial_nesting {
            Err("unmatched braces")?
        } else {
            Ok(chain)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::ast::ast_deserialize;
    use crate::frontend::expression::Expression::{Chain, Value};
    use crate::frontend::iterative_parser;
    use crate::frontend::lexer::lex;
    use std::time::Instant;

    #[test]
    fn add_numbers() {
        let expression = parse(vec![
            Token::Number(5),
            Token::Operator(Operator::Add),
            Token::Number(7),
        ])
        .unwrap();
        assert_eq!(
            expression,
            Chain {
                initial: Box::new(Value(5)),
                transformations: vec![Transformation {
                    operator: Operator::Add,
                    operand: Value(7)
                }],
            }
        )
    }

    #[test]
    fn test_value_in_chain() {
        let tokens = lex("{5}").unwrap();
        let expression = parse(tokens).unwrap();
        assert_eq!(expression, ast_deserialize("5 Chain").unwrap())
    }

    #[test]
    fn test_add_several_numbers() {
        let tokens = lex("5+7+12+34").unwrap();
        let expression = parse(tokens).unwrap();
        assert_eq!(
            expression,
            ast_deserialize("5 +7 Op +12 Op +34 Op Chain").unwrap()
        )
    }

    #[test]
    fn test_call() {
        let tokens = lex("5|print_char").unwrap();
        let expression = parse(tokens).unwrap();
        assert_eq!(
            expression,
            ast_deserialize("5 |print_char Op Chain").unwrap()
        );
    }

    #[test]
    fn test_list() {
        let tokens = lex("[5 6 7]").unwrap();
        let parsed = parse(tokens);
        assert_eq!(parsed.unwrap(), ast_deserialize("[ 5 6 7 ]").unwrap());
    }

    #[test]
    fn test_precedence() {
        let tokens = lex(" 5 - 3 - 1").unwrap();
        let parsed = parse(tokens);
        assert_eq!(
            parsed.unwrap(),
            ast_deserialize("5 -3 Op -1 Op Chain").unwrap()
        );
        let tokens = lex(" 5 - {3 - 1}").unwrap();
        let parsed = parse(tokens);
        assert_eq!(
            parsed.unwrap(),
            ast_deserialize("5 - 3 -1 Op Chain Op Chain").unwrap()
        );
    }

    #[test]
    fn benchmark() {
        let mut code = "1".to_string();
        for x in 0..100 {
            code += "+ {[2 {3";
        }
        for x in 0..100 {
            code += "}]#1}";
        }
        let tokens = lex(code).unwrap();
        let tokens2 = tokens.clone();

        let start = Instant::now();
        let parsed_rec = parse(tokens);
        let duration_recursive = Instant::now().duration_since(start);

        let start = Instant::now();
        let parsed_iter = iterative_parser::Parser::parse_tokens(tokens2);
        let duration_iterative = Instant::now().duration_since(start);

        println!(
            "recursive: {}, iterative: {}",
            duration_recursive.as_micros(),
            duration_iterative.as_micros()
        );
        assert_eq!(parsed_rec.unwrap(), parsed_iter.unwrap());
    }
}
