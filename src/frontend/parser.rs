use crate::common::context;
use crate::frontend::lexer::{Operator, Token, Tokens};
use crate::AnyError;

#[derive(PartialEq, Debug)]
pub enum Expression {
    Nothing,
    Value(i64),
    Identifier(String),
    // Operator {
    //     operator: Operator,
    // },
    // Operation {
    //     operator: Operator,
    //     operands: Expressions,
    // },
    // Transformations {
    //     transformations: Transformations,
    // },
    Chain {
        initial: Box<Expression>,
        transformations: Transformations,
    },
    StaticList(StaticList),
}

// pub type Identifier = String;

#[derive(PartialEq, Debug)]
pub struct Transformation {
    pub operator: Operator,
    pub operand: Expression,
}

#[derive(PartialEq, Debug)]
pub struct StaticList {
    pub elements: Expressions,
}

pub type Expressions = Vec<Expression>;
pub type Transformations = Vec<Transformation>;

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
        let initial = self.parse_expression()?.unwrap_or(Expression::Nothing);
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
                Token::CloseBrace => self.finish_chain(&token),
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

    fn finish_chain(&mut self, token: &Token) -> Result<Option<Transformation>, AnyError> {
        if self.brace_nesting > 0 {
            self.brace_nesting -= 1;
            Ok(None)
        } else {
            Err(format!("unmatched {:?}, expected operator", token))?
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
        let expression = match token {
            Token::Number(n) => Expression::Value(n),
            Token::Identifier(name) => Expression::Identifier(name),
            Token::OpenBracket => self.parse_list()?,
            Token::OpenBrace => self.parse_braced_chain()?,
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
    use crate::frontend::lexer::lex;
    use crate::frontend::parser::Expression::Identifier;
    use Expression::{Chain, Value};

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
    fn test_add_several_numbers() {
        let tokens = lex("5+7+12+34").unwrap();
        let expression = parse(tokens).unwrap();
        assert_eq!(
            expression,
            Chain {
                initial: Box::new(Value(5)),
                transformations: vec![
                    Transformation {
                        operator: Operator::Add,
                        operand: Value(7)
                    },
                    Transformation {
                        operator: Operator::Add,
                        operand: Value(12)
                    },
                    Transformation {
                        operator: Operator::Add,
                        operand: Value(34)
                    },
                ],
            }
        );
    }

    #[test]
    fn test_call() {
        let tokens = lex("5|print_char").unwrap();
        let expression = parse(tokens).unwrap();
        assert_eq!(
            expression,
            Chain {
                initial: Box::new(Value(5)),
                transformations: vec![Transformation {
                    operator: Operator::Call,
                    operand: Identifier("print_char".to_string()),
                }],
            }
        );
    }

    #[test]
    fn test_list() {
        let tokens = lex("[5 6 7]").unwrap();
        let parsed = parse(tokens);
        assert_eq!(
            parsed.unwrap(),
            Expression::StaticList(StaticList {
                elements: vec![Value(5), Value(6), Value(7),]
            })
        );
    }
}
