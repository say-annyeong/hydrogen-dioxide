use std::fmt::{Display, Formatter, Result};

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Punctuation {
    LeftParen,    // (
    RightParen,   // )
    LeftBrace,    // {
    RightBrace,   // }
    LeftBracket,  // [
    RightBracket, // ]
    Comma,        // ,
    Dot,          // .
    Colon,        // :
    Semicolon,    // ;
    Arrow,        // -> 
}

impl Display for Punctuation {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let punc_str = match self {
            Punctuation::LeftParen => "(",
            Punctuation::RightParen => ")",
            Punctuation::LeftBrace => "{",
            Punctuation::RightBrace => "}",
            Punctuation::LeftBracket => "[",
            Punctuation::RightBracket => "]",
            Punctuation::Comma => ",",
            Punctuation::Dot => ".",
            Punctuation::Colon => ":",
            Punctuation::Semicolon => ";",
            Punctuation::Arrow => "->",
        };
        write!(f, "{}", punc_str)
    }
}
