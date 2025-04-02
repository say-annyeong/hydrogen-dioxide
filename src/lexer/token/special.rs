use std::fmt::{Display, Formatter, Result};

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Special {
    Eof,      // End of File/Input
    Illegal,  // Represents an unrecognized character or sequence
}

impl Display for Special {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Special::Eof => write!(f, "EOF"),
            Special::Illegal => write!(f, "Illegal"),
        }
    }
}
