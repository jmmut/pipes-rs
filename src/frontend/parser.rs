pub mod reverse_iterative_parser;

#[cfg(test)]
pub mod slow_iterative_parser;

// pub mod fast_iterative_parser;
#[cfg(test)]
pub mod recursive_parser;

pub use reverse_iterative_parser::parse_tokens;
