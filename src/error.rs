use std::fmt;
use std::error::Error as StdError;

pubenum Error {
    ParseFloatError(std::num::ParseFloatError, String),
    InvalidExpr(String),
}

impl fmt::Display for Error {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::ParseFloatError(e, expr) => write!(fmt, "Parse error: {} \"{}\"", e, expr),
            Error::InvalidExpr(expr) => {
                write!(fmt, "Invalid expr: {}", expr)
            }
        }
    }
}

impl StdError for Error {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match self {
            Error::ParseFloatError(e, _) => Some(e),
            Error::InvalidExpr(_) => None,
        }
    }
}

