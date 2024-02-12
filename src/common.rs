use crate::frontend::location::{Location, SourceCode, Span};

pub type AnyError = Box<dyn std::error::Error>;

pub fn context<T, S: AsRef<str>>(module: S, result: Result<T, AnyError>) -> Result<T, AnyError> {
    result.map_err(|error| {
        // place your breakpoints here
        format!("{}: {error}", module.as_ref()).into()
    })
}

#[track_caller]
pub fn err<T, S: AsRef<str>>(error_message: S) -> Result<T, AnyError> {
    // place your breakpoints here
    let caller_location = std::panic::Location::caller();
    Err(format!("(from {})\n{}", caller_location, error_message.as_ref()).into())
}

#[track_caller]
pub fn err_loc<T, S: AsRef<str>>(error_message: S, code: &SourceCode) -> Result<T, AnyError> {
    // place your breakpoints here
    let caller_location = std::panic::Location::caller();
    Err(format!(
        "(from {})\n{}{}",
        caller_location,
        error_message.as_ref(),
        code.format_current_location()
    )
    .into())
}

#[track_caller]
pub fn err_span<T, S: AsRef<str>>(
    error_message: S,
    code: &SourceCode,
    span: Span,
) -> Result<T, AnyError> {
    // place your breakpoints here
    let caller_location = std::panic::Location::caller();
    Err(format!(
        "(from {})\n{}{}",
        caller_location,
        error_message.as_ref(),
        code.format_span(span)
    )
    .into())
}
#[track_caller]
pub fn err_since<T, S: AsRef<str>>(
    error_message: S,
    code: &SourceCode,
    start: Location,
) -> Result<T, AnyError> {
    // place your breakpoints here
    let caller_location = std::panic::Location::caller();
    Err(format!(
        "(from {})\n{}{}",
        caller_location,
        error_message.as_ref(),
        code.format_span(code.span_since(start))
    )
    .into())
}

#[cfg(test)]
pub fn assert_mentions(err: AnyError, mentions: &[&str]) {
    let err_message = err.to_string().to_ascii_lowercase();
    for mention in mentions {
        assert!(
            err_message.contains(mention),
            "'{}' not mentioned in '{}'",
            mention,
            err
        );
    }
}

pub fn unwrap_display<T>(res: Result<T, AnyError>) -> T {
    match res {
        Ok(t) => t,
        Err(e) => {
            panic!("{}", e)
        }
    }
}
