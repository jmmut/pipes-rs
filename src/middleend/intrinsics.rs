use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::frontend::expression::Type;
use crate::frontend::parse_type;

#[derive(Copy, Clone, EnumIter)]
pub enum Intrinsic {
    PrintChar,
    ReadChar,
    Print,
    ReadLines,
    ToStr,
    NewArray,
    Size,
    Breakpoint,
}

impl Intrinsic {
    pub fn name(&self) -> &'static str {
        match self {
            Intrinsic::PrintChar => "print_char",
            Intrinsic::ReadChar => "read_char",
            Intrinsic::Print => "print",
            Intrinsic::ReadLines => "read_lines",
            Intrinsic::ToStr => "to_str",
            Intrinsic::NewArray => "new_array",
            Intrinsic::Size => "size",
            Intrinsic::Breakpoint => "breakpoint",
        }
    }

    pub fn type_(&self) -> Type {
        match self {
            Intrinsic::PrintChar => parse_type("function(char_to_print :i64) (same_char :i64)"),
            Intrinsic::ReadChar => parse_type("function(reserved :i64) (char :i64)"),
            Intrinsic::Print => {
                parse_type("function(str :list(letter :i64)) (same_input :array(letter :i64))")
            }
            Intrinsic::ReadLines => {
                parse_type("function(reserved :i64) (lines :array(line :array(letter :i64)))")
            }
            Intrinsic::ToStr => {
                parse_type("function(number :i64) (number_str :array(digit_char :i64))")
            }
            Intrinsic::NewArray => parse_type("function(size :i64) (:array(:any))"),
            Intrinsic::Size => parse_type("function(:list(:any)) (:i64)"),
            Intrinsic::Breakpoint => parse_type("function(t) (t)"),
        }
    }
}

#[derive(EnumIter)]
pub enum BuiltinType {
    Unknown,
    Any,
    Or,
    Nothing,
    I64,
    Tuple, // maybe not needed, and any user defined type with children behaves the same
    Array,
    List,
    Struct,
    Function,
    Type,
    Composed,
}

impl BuiltinType {
    pub const fn name(&self) -> &'static str {
        match self {
            BuiltinType::Unknown => "unknown",
            BuiltinType::Any => "any",
            BuiltinType::Or => "or",
            BuiltinType::Nothing => "nothing",
            BuiltinType::I64 => "i64",
            BuiltinType::Tuple => "tuple",
            BuiltinType::Array => "array",
            BuiltinType::List => "list",
            BuiltinType::Struct => "struct",
            BuiltinType::Function => "function",
            BuiltinType::Type => "type",
            BuiltinType::Composed => "composed",
        }
    }
}

pub mod builtin_types {
    use crate::frontend::expression::{Type, TypeName};
    use crate::middleend::intrinsics::BuiltinType;

    pub const UNKNOWN: Type = Type::Simple {
        type_name: TypeName::Builtin(BuiltinType::Unknown.name()),
    };
    pub const ANY: Type = Type::Simple {
        type_name: TypeName::Builtin(BuiltinType::Any.name()),
    };
    pub const NOTHING: Type = Type::Simple {
        type_name: TypeName::Builtin(BuiltinType::Nothing.name()),
    };
    pub const I64: Type = Type::Simple {
        type_name: TypeName::Builtin(BuiltinType::I64.name()),
    };
    pub const TYPE: Type = Type::Simple {
        type_name: TypeName::Builtin(BuiltinType::Type.name()),
    };
    pub const COMPOSED: Type = Type::Simple {
        type_name: TypeName::Builtin(BuiltinType::Composed.name()),
    };
}

pub fn is_builtin_type(name: &str) -> Option<&'static str> {
    for builtin in BuiltinType::iter() {
        let builtin_name = builtin.name();
        if builtin_name == name {
            return Some(builtin_name);
        }
    }
    None
}
