use std::fmt::{Display, Formatter, Result};

pub mod assignment;
pub mod keyword;
pub mod operators;
pub mod punctuation;
pub mod special;

pub use assignment::Assignment;
pub use keyword::{Keyword, lookup_keyword};
pub use operators::Operator;
pub use punctuation::Punctuation;
pub use special::Special;

/// Represents a position in the source code.
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct Position {
    pub start_line: usize,
    pub start_column: usize,
    pub end_line: usize,
    pub end_column: usize,
}

/// Represents a token in the Oxygen language.
#[derive(Debug, PartialEq, Clone)] // Using Clone for easier handling, potentially review if performance critical
pub enum Token {
    // --- Literals ---
    Ident(String),      // Identifier (e.g., variable name, function name)
    Int(String),        // Integer literal (e.g., "10", "0xFF")
    Float(String),      // Float literal (e.g., "3.14")
    String(String),     // String literal (e.g., "hello", 'world', `template`)

    // --- Operators ---
    Operator(Operator),
    Assignment(Assignment),

    // --- Punctuation ---
    Punctuation(Punctuation),

    // --- Keywords ---
    Keyword(Keyword),

    // --- Special Tokens ---
    Special(Special),
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Token::Ident(s) => write!(f, "Ident({})", s),
            Token::Int(s) => write!(f, "Int({})", s),
            Token::Float(s) => write!(f, "Float({})", s),
            Token::String(s) => write!(f, "String({})", s),
            Token::Operator(op) => op.fmt(f),
            Token::Assignment(assign) => assign.fmt(f),
            Token::Punctuation(punc) => punc.fmt(f),
            Token::Keyword(kw) => kw.fmt(f),
            Token::Special(spec) => spec.fmt(f),
        }
    }
}

impl Token {
    /// Convenience constructor for Identifier tokens.
    pub fn ident(s: impl Into<String>) -> Self {
        Token::Ident(s.into())
    }

    /// Convenience constructor for Int tokens.
    pub fn int(s: impl Into<String>) -> Self {
        Token::Int(s.into())
    }
    
    /// Convenience constructor for Float tokens.
    pub fn float(s: impl Into<String>) -> Self {
        Token::Float(s.into())
    }

    /// Convenience constructor for String tokens.
    pub fn string(s: impl Into<String>) -> Self {
        Token::String(s.into())
    }
}
