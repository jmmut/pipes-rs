use std::fmt::{Display, Formatter};

use strum_macros::EnumIter;

use crate::frontend::sources::location::{Span, NO_SPAN};

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

#[derive(Copy, Clone)]
pub struct OperatorSpan {
    pub operator: Operator,
    pub span: Span,
}

impl OperatorSpan {
    pub fn new(operator: Operator, span: Span) -> Self {
        Self { operator, span }
    }
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

pub const ADD: u8 = b'+';
pub const SUBSTRACT: u8 = b'-';
pub const MULTIPLY: &str = "|*";
pub const DIVIDE: &str = "|/";
pub const MODULO: u8 = b'%';
pub const IGNORE: u8 = b';';
pub const CALL: u8 = b'|';
// pub const CALL_REVERSE: &str = "<|";
pub const GET: u8 = b'#';
pub const TYPE: u8 = b':';
pub const ASSIGNMENT: u8 = b'=';
pub const OVERWRITE: &str = "=>";
pub const CONCATENATE: &str = "++";
pub const FIELD: u8 = b'.';

pub const EQUALS: &str = "=?";
pub const EQUALS_ALT: &str = "==";
pub const DIFFERENT: &str = "!=";
pub const LESS_THAN: u8 = b'<';
pub const GREATER_THAN: u8 = b'>';
pub const LESS_THAN_EQUALS: &str = "<=";
pub const GREATER_THAN_EQUALS: &str = ">=";

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
    Field,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Comparison {
    Equals,
    Different,
    LessThan,
    GreaterThan,
    LessThanEquals,
    GreaterThanEquals,
}

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Add => write!(f, "{}", ADD as char),
            Operator::Substract => write!(f, "{}", SUBSTRACT as char),
            Operator::Multiply => write!(f, "{}", MULTIPLY),
            Operator::Divide => write!(f, "{}", DIVIDE),
            Operator::Modulo => write!(f, "{}", MODULO as char),
            Operator::Ignore => write!(f, "{}", IGNORE as char),
            Operator::Call => write!(f, "{}", CALL as char),
            Operator::Get => write!(f, "{}", GET as char),
            Operator::Type => write!(f, "{}", TYPE as char),
            Operator::Assignment => write!(f, "{}", ASSIGNMENT as char),
            Operator::Overwrite => write!(f, "{}", OVERWRITE),
            Operator::Concatenate => write!(f, "{}", CONCATENATE),
            Operator::Comparison(Comparison::Equals) => write!(f, "{}", EQUALS_ALT),
            Operator::Comparison(Comparison::Different) => write!(f, "{}", DIFFERENT),
            Operator::Comparison(Comparison::LessThan) => write!(f, "{}", LESS_THAN as char),
            Operator::Comparison(Comparison::GreaterThan) => write!(f, "{}", GREATER_THAN as char),
            Operator::Comparison(Comparison::LessThanEquals) => write!(f, "{}", LESS_THAN_EQUALS),
            Operator::Comparison(Comparison::GreaterThanEquals) => {
                write!(f, "{}", GREATER_THAN_EQUALS)
            }
            Operator::Field => write!(f, "{}", FIELD as char),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Debug, EnumIter)]
pub enum Keyword {
    Function,
    Loop,
    Browse,
    BrowseOr,
    Times,
    TimesOr,
    Replace,
    Map,
    Filter,
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
            Keyword::Browse => "browse",
            Keyword::BrowseOr => "browse_or",
            Keyword::Times => "times",
            Keyword::TimesOr => "times_or",
            Keyword::Replace => "replace",
            Keyword::Map => "map",
            Keyword::Filter => "filter",
            Keyword::Branch => "branch",
            Keyword::Something => "something",
            Keyword::Inspect => "inspect",
            Keyword::Public => "public",
            Keyword::Cast => "cast",
        }
    }
}
