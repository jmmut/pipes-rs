use crate::backend::evaluate::{BindingsStack, Closure, GenericValue, ListPointer};
use crate::frontend::expression::{ExpressionSpan, Function, Type};
use crate::frontend::sources::Sources;
use crate::middleend::intrinsics::Intrinsic;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufReader, Read, Write};
use std::os::raw;
use std::rc::Rc;

mod debugger;
pub mod evaluate;

pub struct Runtime<R: Read, W: Write> {
    /// using a map<index,list> instead of a vec<list> to be able to deallocate individual lists
    lists: HashMap<ListPointer, Vec<GenericValue>>,
    functions: Vec<Rc<FunctionOrIntrinsic>>,
    identifiers: HashMap<String, BindingsStack>,
    static_identifiers: HashMap<String, BindingsStack>,
    identifier_expressions: HashMap<String, ExpressionSpan>,
    types: HashMap<String, Vec<Type>>,
    read_input: Option<R>, // these will always be Some(), but stdin/stdout are not Default, and option is. needed for eval
    print_output: Option<W>,
    _sources: Sources, // TODO: figure out how to show sources. We might be executing some function
    //   pointer, but we don't track where a function ptr was generated
    /// file descriptor to reader. see [Runtime::debugger_prompt]
    extra_inputs: HashMap<raw::c_int, BufReader<File>>,
}

enum FunctionOrIntrinsic {
    Function(Function, Closure),
    Intrinsic(Intrinsic),
}
