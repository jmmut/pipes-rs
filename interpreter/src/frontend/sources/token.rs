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
    EndOfFile,
    // Comma,
}

pub type Tokens = Vec<Token>;

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Number(n) => write!(f, "Number '{}'", n),
            Token::Operator(o) => write!(f, "Operator '{}'", o),
            Token::Identifier(name) => write!(f, "Identifier '{}'", name),
            Token::String(s) => write!(f, "String \"{}\"", String::from_utf8_lossy(s)),
            Token::Keyword(s) => write!(f, "Keyword \"{}\"", s.name()),
            Token::OpenBracket => write!(f, "OpenBracket '['"),
            Token::CloseBracket => write!(f, "CloseBracket ']'"),
            Token::OpenBrace => write!(f, "OpenBrace '{{'"),
            Token::CloseBrace => write!(f, "CloseBrace '}}'"),
            Token::OpenParenthesis => write!(f, "OpenParenthesis '('"),
            Token::CloseParenthesis => write!(f, "CloseParenthesis ')'"),
            Token::EndOfFile => write!(f, "EndOfFile"),
        }
    }
}
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
pub const MACRO_CALL: &str = "|`";
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
    MacroCall,
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
            Operator::MacroCall => write!(f, "{}", MACRO_CALL),
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
    Nothing,
    Function,
    Macro,
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
    Comptime,
}

impl Keyword {
    pub fn name(&self) -> &'static str {
        match self {
            // not sure about this, but it seems easier to parse (for humans and computers) if the
            // value nothing (none) and the type nothing (nothing) are called different
            Keyword::Nothing => "none",
            Keyword::Function => "function",
            Keyword::Macro => "macro",
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
            Keyword::Inspect => "inspect_intrinsic",
            Keyword::Public => "public",
            Keyword::Cast => "cast",
            Keyword::Comptime => "comptime",
        }
    }
}
