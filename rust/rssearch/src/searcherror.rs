use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub struct SearchError {
    pub description: String,
}

impl SearchError {
    /// Create a new Error instance
    pub fn new(desc: &str) -> SearchError {
        SearchError {
            description: desc.to_string(),
        }
    }
}

impl fmt::Display for SearchError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", &self.description)
    }
}

impl Error for SearchError {
    fn description(&self) -> &str {
        &self.description
    }
}
