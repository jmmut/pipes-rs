pub type AnyError = Box<dyn std::error::Error>;

pub fn context<T, S: AsRef<str>>(module: S, result: Result<T, AnyError>) -> Result<T, AnyError> {
    result.map_err(|error| format!("{}: {error}", module.as_ref()).into())
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
