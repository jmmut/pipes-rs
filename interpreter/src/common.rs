use crate::frontend::sources::location::{Location, SourceCode, Span};

#[cfg(test)]
use std::path::PathBuf;

pub type AnyError = Box<dyn std::error::Error>;

pub fn context<T, S: AsRef<str>, E: Into<AnyError>>(
    module: S,
    result: Result<T, E>,
) -> Result<T, AnyError> {
    result.map_err(|error| {
        // place your breakpoints here
        format!("{}: {}", module.as_ref(), error.into()).into()
    })
}

#[track_caller]
pub fn err<T, S: AsRef<str>>(error_message: S) -> Result<T, AnyError> {
    // place your breakpoints here
    let caller_location = std::panic::Location::caller();
    Err(format!("\n{}:\n{}", caller_location, error_message.as_ref()).into())
}

#[track_caller]
pub fn bug<T, S: AsRef<str>>(error_message: S) -> Result<T, AnyError> {
    // place your breakpoints here
    let caller_location = std::panic::Location::caller();
    Err(format!("\n{}:\nBug: {}", caller_location, error_message.as_ref()).into())
}

#[track_caller]
pub fn err_loc<T, S: AsRef<str>>(error_message: S, code: &SourceCode) -> Result<T, AnyError> {
    // place your breakpoints here
    let caller_location = std::panic::Location::caller();
    Err(format!(
        "\n{}:\n{}{}",
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
        "\n{}:\n{}{}",
        caller_location,
        error_message.as_ref(),
        code.format_span(span)
    )
    .into())
}
#[track_caller]
pub fn bug_span<T, S: AsRef<str>>(
    error_message: S,
    code: &SourceCode,
    span: Span,
) -> Result<T, AnyError> {
    // place your breakpoints here
    let caller_location = std::panic::Location::caller();
    Err(format!(
        "\n{}:\nBug: {}{}",
        caller_location,
        error_message.as_ref(),
        code.format_span(span)
    )
    .into())
}
#[track_caller]
pub fn bug_maybe_span<T, S: AsRef<str>>(
    error_message: S,
    code: Option<&SourceCode>,
    span: Span,
) -> Result<T, AnyError> {
    // place your breakpoints here
    let caller_location = std::panic::Location::caller();
    Err(format!(
        "\n{}:\nBug: {}{}",
        caller_location,
        error_message.as_ref(),
        maybe_format_span(code, span)
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
        "\n{}:\n{}{}",
        caller_location,
        error_message.as_ref(),
        code.format_span(code.span_since(start))
    )
    .into())
}

pub fn maybe_format_span(source: Option<&SourceCode>, span: Span) -> String {
    if let Some(source) = source {
        source.format_span(span)
    } else {
        format!(" at unknown file, line {}", span.to_string())
    }
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

/// apparently `cargo test` uses the package as working directory, so any test will be in a direct
/// subfolder of the root.
#[cfg(test)]
pub fn set_current_dir_to_repo_root() -> Result<PathBuf, AnyError> {
    let previous = std::env::current_dir();
    std::env::set_current_dir(PathBuf::from("..")).unwrap();
    // println!("current dir set to {:?}", std::env::current_dir());
    Ok(previous?)
}

#[cfg(test)]
pub fn run_in_repo_root<R>(f: impl Fn() -> R) -> Result<R, AnyError> {
    let previous = set_current_dir_to_repo_root()?;
    let result = f();
    std::env::set_current_dir(previous)?;
    Ok(result)
}

/// This function is equivalent to doing simply .unwrap(), whose message is:
/// > called `Result::unwrap()` on an `Err` value: "Reverse parser: expected operand after operator but was '|to_str'"
///
/// but using this function to do the panic ourselves is clearer (straight to the point) in some cases:
/// > Reverse parser: expected operand after operator but was '|to_str'
pub fn unwrap_display<T>(res: Result<T, AnyError>) -> T {
    res.unwrap_or_else(|e| panic!("{}", e))
}
