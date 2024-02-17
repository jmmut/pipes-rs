use crate::frontend::location::{Span, NO_SPAN};
use strum_macros::EnumIter;

#[derive(Clone, PartialEq, Debug)]
pub struct LocatedToken {
    pub token: Token,
    pub span: Span,
}
pub type LocatedTokens = Vec<LocatedToken>;

impl LocatedToken {
    pub fn spanless(token: Token) -> LocatedToken {
        LocatedToken {
            token,
            span: NO_SPAN,
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Token {
    Number(i64),
    Operator(Operator),
    Identifier(String),
    String(Vec<u8>),
    Keyword(Keyword),
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,
    OpenParenthesis,
    CloseParenthesis,
    // Comma,
}
pub type Tokens = Vec<Token>;

#[derive(Copy, Clone, Debug)]
pub struct OperatorSpan {
    pub operator: Operator,
    pub span: Span,
}

impl OperatorSpan {
    pub fn spanless(operator: Operator) -> Self {
        Self {
            operator,
            span: NO_SPAN,
        }
    }
}
impl PartialEq for OperatorSpan {
    fn eq(&self, other: &Self) -> bool {
        self.operator == other.operator
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Operator {
    Add,
    Substract,
    Multiply,
    Divide,
    Modulo,
    Ignore,
    Call,
    Get,
    Type,
    Assignment,
    Overwrite,
    Concatenate,
    Comparison(Comparison),
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Comparison {
    Equals,
    LessThan,
    GreaterThan,
    LessThanEquals,
    GreaterThanEquals,
}

#[derive(Copy, Clone, PartialEq, Debug, EnumIter)]
pub enum Keyword {
    Function,
    Loop,
    LoopOr,
    Times,
    TimesOr,
    Replace,
    Map,
    Branch,
    Something,
    Inspect,
    Public,
    Cast,
}

impl Keyword {
    pub fn name(&self) -> &'static str {
        match self {
            Keyword::Function => "function",
            Keyword::Loop => "loop",
            Keyword::LoopOr => "loop_or",
            Keyword::Times => "times",
            Keyword::TimesOr => "times_or",
            Keyword::Replace => "replace",
            Keyword::Map => "map",
            Keyword::Branch => "branch",
            Keyword::Something => "something",
            Keyword::Inspect => "inspect",
            Keyword::Public => "public",
            Keyword::Cast => "cast",
        }
    }
}
