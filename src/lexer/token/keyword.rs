use std::fmt::{Display, Formatter, Result};

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
#[allow(clippy::enum_variant_names)]
pub enum Keyword {
    Let,
    Fn,
    If,
    Elif,
    Else,
    For,
    While,
    Return,
    Import,
    From,
    As,
    Struct,
    True,
    False,
    Null,
    Break,
    Export,
    // Add other keywords if necessary
}

impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let keyword_str = match self {
            Keyword::Let => "let",
            Keyword::Fn => "fn",
            Keyword::If => "if",
            Keyword::Elif => "elif",
            Keyword::Else => "else",
            Keyword::For => "for",
            Keyword::While => "while",
            Keyword::Return => "return",
            Keyword::Import => "import",
            Keyword::From => "from",
            Keyword::As => "as",
            Keyword::Struct => "struct",
            Keyword::True => "true",
            Keyword::False => "false",
            Keyword::Null => "null",
            Keyword::Break => "break",
            Keyword::Export => "export",
        };
        write!(f, "{}", keyword_str)
    }
}

// Helper function to check if a string is a keyword
pub fn lookup_keyword(s: &str) -> Option<Keyword> {
    match s {
        "let" => Some(Keyword::Let),
        "fn" => Some(Keyword::Fn),
        "if" => Some(Keyword::If),
        "elif" => Some(Keyword::Elif),
        "else" => Some(Keyword::Else),
        "for" => Some(Keyword::For),
        "while" => Some(Keyword::While),
        "return" => Some(Keyword::Return),
        "import" => Some(Keyword::Import),
        "from" => Some(Keyword::From),
        "as" => Some(Keyword::As),
        "struct" => Some(Keyword::Struct),
        "true" => Some(Keyword::True),
        "false" => Some(Keyword::False),
        "null" => Some(Keyword::Null),
        "break" => Some(Keyword::Break),
        "export" => Some(Keyword::Export),
        _ => None,
    }
}
