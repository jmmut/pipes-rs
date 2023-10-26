pub type AnyError = Box<dyn std::error::Error>;

pub fn context<T>(module: &str, result: Result<T, AnyError>) -> Result<T, AnyError> {
    result.map_err(|error| format!("{module}: {error}").into())
}
