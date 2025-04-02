use std::iter::Peekable;
use std::str::Chars;

use super::token::{lookup_keyword, Assignment, Keyword, Operator, Punctuation, Special, Token};

#[derive(Debug, Clone)]
pub struct Tokenizer<'a> {
    input: Peekable<Chars<'a>>,
    current_pos: usize, // Tracks the byte position in the original input string
    line: usize,
    column: usize,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        Tokenizer {
            input: input.chars().peekable(),
            current_pos: 0,
            line: 1,
            column: 1, // Start column at 1
        }
    }

    // Helper to advance the iterator and update position/line/column
    fn next_char(&mut self) -> Option<char> {
        match self.input.next() {
            Some(ch) => {
                self.current_pos += ch.len_utf8();
                if ch == '\n' {
                    self.line += 1;
                    self.column = 1;
                } else {
                    self.column += 1;
                }
                Some(ch)
            }
            None => None,
        }
    }

    // Helper to peek at the next character without consuming
    fn peek_char(&mut self) -> Option<&char> {
        self.input.peek()
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            match self.peek_char() {
                Some(&ch) if ch.is_whitespace() => {
                    self.next_char(); // Consume whitespace
                }
                Some(&'/') => {
                    // Peek ahead to see if it's a comment without consuming '/' yet
                    let mut ahead = self.input.clone();
                    ahead.next(); // Advance past the first '/' in the cloned iterator
                    match ahead.peek() {
                        Some('/') => { // It's a // comment
                            self.next_char(); // Consume first '/'
                            self.next_char(); // Consume second '/'
                            // Consume rest of the line
                            while let Some(&c) = self.peek_char() {
                                if c == '\n' {
                                    // Don't consume newline, let next loop handle it
                                    break;
                                }
                                self.next_char();
                            }
                            // Continue outer loop to handle potential whitespace/comments after this one
                        }
                        Some('*') => { // It's a /* comment
                            self.next_char(); // Consume first '/'
                            self.next_char(); // Consume '*'
                            let mut nesting = 1;
                            while let Some(c) = self.next_char() {
                                if c == '*' {
                                    if let Some(&'/') = self.peek_char() {
                                        self.next_char(); // Consume '/'
                                        nesting -= 1;
                                        if nesting == 0 {
                                            break; // End of multi-line comment
                                        }
                                    }
                                } else if c == '/' {
                                     if let Some(&'*') = self.peek_char() {
                                         self.next_char(); // Consume '*'
                                         nesting += 1; // Start of nested comment
                                     }
                                }
                                // Handle EOF within comment? Should break loop naturally. Add error?
                            }
                             if nesting > 0 {
                                 // TODO: Record an error for unclosed multi-line comment
                                 eprintln!("Warning: Unclosed multi-line comment");
                             }
                            // Continue outer loop
                        }
                        _ => {
                            // First '/' is not followed by '/' or '*', so it's not a comment.
                            // Break the skipping loop, let '/' be handled by next_token.
                            break;
                        }
                    }
                }
                _ => break, // Not whitespace or comment start '/'
            }
        }
    }

     fn read_identifier(&mut self, first_char: char) -> String {
        let mut ident = String::new();
        ident.push(first_char);
        while let Some(&ch) = self.peek_char() {
            if ch.is_alphanumeric() || ch == '_' {
                ident.push(self.next_char().unwrap()); // Safe unwrap: peeked first
            } else {
                break;
            }
        }
        ident
    }

    // Basic number reader (integer for now)
    // TODO: Add float, hex, binary support
     fn read_number(&mut self, first_char: char) -> String {
        let mut number = String::new();
        number.push(first_char);

        // Consume initial integer part
        while let Some(&ch) = self.peek_char() {
            if ch.is_ascii_digit() {
                number.push(self.next_char().unwrap()); // Safe unwrap: peeked first
            } else {
                break; // Not a digit
            }
        }

        // Check for decimal part
        if let Some(&'.') = self.peek_char() {
            // Peek ahead one more character
            let mut iter_clone = self.input.clone();
            iter_clone.next(); // Advance past the '.' in the clone

            if let Some(&next_ch) = iter_clone.peek() {
                if next_ch.is_ascii_digit() {
                    // It's a valid float decimal part
                    number.push(self.next_char().unwrap()); // Consume the '.'

                    // Consume digits after the decimal point
                    while let Some(&ch) = self.peek_char() {
                        if ch.is_ascii_digit() {
                            number.push(self.next_char().unwrap());
                        } else {
                            break; // Not a digit
                        }
                    }
                } // Else: '.' is not followed by a digit, so treat number as integer and leave '.'
            } // Else: EOF after '.', treat as integer
        } // Else: Not a '.' after integer part

        number
    }

     // Basic string reader
     // TODO: Handle escape sequences
    fn read_string(&mut self, quote_type: char) -> Result<String, String> {
        let mut s = String::new();
        let start_line = self.line;
        let start_col = self.column;

        loop {
            match self.next_char() {
                Some(ch) => {
                    if ch == quote_type {
                        return Ok(s); // End of string
                    } else if ch == '\\' {
                        // Handle escape sequences (basic implementation)
                         match self.next_char() {
                             Some('n') => s.push('\n'),
                             Some('t') => s.push('\t'),
                             Some('r') => s.push('\r'),
                             Some('\\') => s.push('\\'),
                             Some('\'') => s.push('\''),
                             Some('"') => s.push('"'),
                             Some('`') => s.push('`'), // Allow escaping backtick?
                             Some(other) => { // Invalid escape
                                 // Option 1: Treat as literal backslash + char
                                 // s.push('\\');
                                 // s.push(other);
                                // Option 2: Return an error
                                 return Err(format!("Invalid escape sequence: \\{}", other));
                             }
                             None => return Err("Unterminated string literal: EOF after backslash".to_string()),
                         }
                    }
                     else if ch == '\n' && quote_type != '`' {
                        // Newlines are only allowed in backtick strings
                        return Err(format!(
                            "Unterminated string literal: newline encountered (started at {}:{})",
                            start_line, start_col
                        ));
                    }
                     else {
                        s.push(ch);
                    }
                }
                None => {
                    return Err(format!(
                        "Unterminated string literal: EOF encountered (started at {}:{})",
                        start_line, start_col
                    )); // End of file before closing quote
                }
            }
        }
    }


    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace_and_comments();

        let _start_pos = self.current_pos; // Prefix with underscore

        match self.next_char() {
            Some(ch) => {
                match ch {
                    // Punctuation
                    '(' => Token::Punctuation(Punctuation::LeftParen),
                    ')' => Token::Punctuation(Punctuation::RightParen),
                    '{' => Token::Punctuation(Punctuation::LeftBrace),
                    '}' => Token::Punctuation(Punctuation::RightBrace),
                    '[' => Token::Punctuation(Punctuation::LeftBracket),
                    ']' => Token::Punctuation(Punctuation::RightBracket),
                    ',' => Token::Punctuation(Punctuation::Comma),
                    '.' => Token::Punctuation(Punctuation::Dot),
                    ':' => Token::Punctuation(Punctuation::Colon),
                    ';' => Token::Punctuation(Punctuation::Semicolon),

                    // Operators & Assignments (potentially multi-char)
                    '+' => match self.peek_char() {
                        Some(&'=') => { self.next_char(); Token::Assignment(Assignment::PlusAssign) }
                        _ => Token::Operator(Operator::Plus),
                    },
                    '-' => match self.peek_char() {
                        Some(&'=') => { self.next_char(); Token::Assignment(Assignment::MinusAssign) }
                         Some(&'>') => { self.next_char(); Token::Punctuation(Punctuation::Arrow) }
                        _ => Token::Operator(Operator::Minus),
                    },
                    '*' => match self.peek_char() {
                        Some(&'=') => { self.next_char(); Token::Assignment(Assignment::MultiplyAssign) }
                        _ => Token::Operator(Operator::Multiply),
                    },
                     '/' => match self.peek_char() {
                        // Comments are handled in skip_whitespace_and_comments
                        Some(&'=') => { self.next_char(); Token::Assignment(Assignment::DivideAssign) }
                        _ => Token::Operator(Operator::Divide), // Should not be reachable if comments handled first
                     },
                    '%' => match self.peek_char() {
                        Some(&'=') => { self.next_char(); Token::Assignment(Assignment::ModuloAssign) }
                        _ => Token::Operator(Operator::Modulo),
                    },
                    '=' => match self.peek_char() {
                        Some(&'=') => { self.next_char(); Token::Operator(Operator::Equal) }
                        _ => Token::Assignment(Assignment::Assign),
                    },
                    '!' => match self.peek_char() {
                        Some(&'=') => { self.next_char(); Token::Operator(Operator::NotEqual) }
                        _ => Token::Operator(Operator::Not),
                    },
                    '<' => match self.peek_char() {
                        Some(&'=') => { self.next_char(); Token::Operator(Operator::LessEqual) }
                        _ => Token::Operator(Operator::LessThan),
                    },
                    '>' => match self.peek_char() {
                        Some(&'=') => { self.next_char(); Token::Operator(Operator::GreaterEqual) }
                        _ => Token::Operator(Operator::GreaterThan),
                    },
                     '&' => match self.peek_char() {
                         Some(&'&') => { self.next_char(); Token::Operator(Operator::And) }
                         _ => Token::Special(Special::Illegal), // Single '&' is illegal? Or bitwise? Define later.
                     },
                     '|' => match self.peek_char() {
                         Some(&'|') => { self.next_char(); Token::Operator(Operator::Or) }
                         _ => Token::Special(Special::Illegal), // Single '|' is illegal? Or bitwise? Define later.
                     },


                    // String Literals
                    '"' | '\'' | '`' => {
                        match self.read_string(ch) {
                            Ok(s) => Token::String(s),
                            Err(e) => {
                                // Handle error - maybe log it? For now, return Illegal
                                eprintln!("Lexer Error: {}", e); // Log error to stderr
                                Token::Special(Special::Illegal)
                            }
                        }
                    }

                    // Numbers
                    d if d.is_ascii_digit() => {
                        let num_str = self.read_number(d);
                         if num_str.contains('.') {
                             Token::Float(num_str)
                         } else {
                            Token::Int(num_str)
                         }
                        // TODO: Add hex/binary prefix check (0x, 0b) here
                    }

                    // Identifiers or Keywords
                     id_start if id_start.is_alphabetic() || id_start == '_' => {
                         let ident = self.read_identifier(id_start);
                         match lookup_keyword(&ident) {
                             Some(keyword) => Token::Keyword(keyword),
                             None => Token::Ident(ident),
                         }
                     }

                    // Unrecognized character
                    _ => Token::Special(Special::Illegal),
                }
            }
            None => Token::Special(Special::Eof), // End of input
        }
    }
}

// Implement Iterator to make it easy to loop through tokens
impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Token::Special(Special::Eof) => None, // Stop iteration at EOF
            token => Some(token),
        }
    }
}


// --- Basic Tests ---
#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::token::{Assignment, Keyword, Operator, Punctuation, Special, Token}; // Adjust path as needed

    fn lex_all(input: &str) -> Vec<Token> {
        Tokenizer::new(input).collect()
    }

    #[test]
    fn test_simple_assignment() {
        let input = "let x = 5;";
        let expected = vec![
            Token::Keyword(Keyword::Let),
            Token::Ident("x".to_string()),
            Token::Assignment(Assignment::Assign),
            Token::Int("5".to_string()),
            Token::Punctuation(Punctuation::Semicolon),
        ];
        assert_eq!(lex_all(input), expected);
    }

    #[test]
    fn test_operators_and_parens() {
        let input = "(1 + 2) * 3";
         let expected = vec![
            Token::Punctuation(Punctuation::LeftParen),
            Token::Int("1".to_string()),
            Token::Operator(Operator::Plus),
            Token::Int("2".to_string()),
            Token::Punctuation(Punctuation::RightParen),
            Token::Operator(Operator::Multiply),
            Token::Int("3".to_string()),
         ];
        assert_eq!(lex_all(input), expected);
    }

    #[test]
    fn test_keywords_and_ident() {
        let input = "fn main() { return false; }";
        let expected = vec![
            Token::Keyword(Keyword::Fn),
            Token::Ident("main".to_string()),
            Token::Punctuation(Punctuation::LeftParen),
            Token::Punctuation(Punctuation::RightParen),
            Token::Punctuation(Punctuation::LeftBrace),
            Token::Keyword(Keyword::Return),
            Token::Keyword(Keyword::False),
            Token::Punctuation(Punctuation::Semicolon),
            Token::Punctuation(Punctuation::RightBrace),
        ];
        assert_eq!(lex_all(input), expected);
    }

     #[test]
    fn test_string_literal() {
        let input = "let msg = \"Hello, Oxygen!\";";
        let expected = vec![
            Token::Keyword(Keyword::Let),
            Token::Ident("msg".to_string()),
            Token::Assignment(Assignment::Assign),
            Token::String("Hello, Oxygen!".to_string()),
            Token::Punctuation(Punctuation::Semicolon),
        ];
        assert_eq!(lex_all(input), expected);
    }

     #[test]
    fn test_single_quote_string_literal() {
        let input = "let msg = 'World';";
        let expected = vec![
            Token::Keyword(Keyword::Let),
            Token::Ident("msg".to_string()),
            Token::Assignment(Assignment::Assign),
            Token::String("World".to_string()),
            Token::Punctuation(Punctuation::Semicolon),
        ];
        assert_eq!(lex_all(input), expected);
    }

    #[test]
    fn test_backtick_string_literal() {
        let input = "let html = `<html>\n<body></body>\n</html>`;";
        let expected = vec![
            Token::Keyword(Keyword::Let),
            Token::Ident("html".to_string()),
            Token::Assignment(Assignment::Assign),
            Token::String("<html>\n<body></body>\n</html>".to_string()),
            Token::Punctuation(Punctuation::Semicolon),
        ];
        assert_eq!(lex_all(input), expected);
    }


     #[test]
    fn test_string_escape_sequences() {
        let input = r#"let escapes = "line1\nline2\t'quote'\"quote\"\\backslash";"#;
        let expected = vec![
            Token::Keyword(Keyword::Let),
            Token::Ident("escapes".to_string()),
            Token::Assignment(Assignment::Assign),
            Token::String("line1\nline2\t'quote'\"quote\"\\backslash".to_string()),
            Token::Punctuation(Punctuation::Semicolon),
        ];
        assert_eq!(lex_all(input), expected);
    }

    #[test]
    fn test_unterminated_string() {
        let input = "let bad = \"hello";
        // Expecting Illegal token at the end because read_string returns Err
         let tokens = lex_all(input);
         assert_eq!(tokens.len(), 4);
         assert_eq!(tokens[0], Token::Keyword(Keyword::Let));
         assert_eq!(tokens[1], Token::Ident("bad".to_string()));
         assert_eq!(tokens[2], Token::Assignment(Assignment::Assign));
         assert_eq!(tokens[3], Token::Special(Special::Illegal)); // String reading fails
    }


     #[test]
    fn test_float_literal() {
        let input = "let pi = 3.14159;";
        let expected = vec![
            Token::Keyword(Keyword::Let),
            Token::Ident("pi".to_string()),
            Token::Assignment(Assignment::Assign),
            Token::Float("3.14159".to_string()),
            Token::Punctuation(Punctuation::Semicolon),
        ];
        assert_eq!(lex_all(input), expected);
    }

     #[test]
     fn test_dot_operator_vs_float() {
         let input = "obj.method(5.0)";
         let expected = vec![
             Token::Ident("obj".to_string()),
             Token::Punctuation(Punctuation::Dot),
             Token::Ident("method".to_string()),
             Token::Punctuation(Punctuation::LeftParen),
             Token::Float("5.0".to_string()),
             Token::Punctuation(Punctuation::RightParen),
         ];
        assert_eq!(lex_all(input), expected);
     }


    #[test]
    fn test_multi_char_operators() {
        let input = "x += 1; y -= 2; z *= 3; w /= 4; m %= 5; a == b; c != d; e <= f; g >= h; cond && other; next || prev; !flag; fn() -> void";
        let expected = vec![
            Token::Ident("x".to_string()), Token::Assignment(Assignment::PlusAssign), Token::Int("1".to_string()), Token::Punctuation(Punctuation::Semicolon),
            Token::Ident("y".to_string()), Token::Assignment(Assignment::MinusAssign), Token::Int("2".to_string()), Token::Punctuation(Punctuation::Semicolon),
            Token::Ident("z".to_string()), Token::Assignment(Assignment::MultiplyAssign), Token::Int("3".to_string()), Token::Punctuation(Punctuation::Semicolon),
            Token::Ident("w".to_string()), Token::Assignment(Assignment::DivideAssign), Token::Int("4".to_string()), Token::Punctuation(Punctuation::Semicolon),
            Token::Ident("m".to_string()), Token::Assignment(Assignment::ModuloAssign), Token::Int("5".to_string()), Token::Punctuation(Punctuation::Semicolon),
            Token::Ident("a".to_string()), Token::Operator(Operator::Equal), Token::Ident("b".to_string()), Token::Punctuation(Punctuation::Semicolon),
            Token::Ident("c".to_string()), Token::Operator(Operator::NotEqual), Token::Ident("d".to_string()), Token::Punctuation(Punctuation::Semicolon),
            Token::Ident("e".to_string()), Token::Operator(Operator::LessEqual), Token::Ident("f".to_string()), Token::Punctuation(Punctuation::Semicolon),
            Token::Ident("g".to_string()), Token::Operator(Operator::GreaterEqual), Token::Ident("h".to_string()), Token::Punctuation(Punctuation::Semicolon),
            Token::Ident("cond".to_string()), Token::Operator(Operator::And), Token::Ident("other".to_string()), Token::Punctuation(Punctuation::Semicolon),
            Token::Ident("next".to_string()), Token::Operator(Operator::Or), Token::Ident("prev".to_string()), Token::Punctuation(Punctuation::Semicolon),
            Token::Operator(Operator::Not), Token::Ident("flag".to_string()), Token::Punctuation(Punctuation::Semicolon),
             Token::Keyword(Keyword::Fn), Token::Punctuation(Punctuation::LeftParen), Token::Punctuation(Punctuation::RightParen), Token::Punctuation(Punctuation::Arrow), Token::Ident("void".to_string()), // Treat void as ident for now
        ];
        assert_eq!(lex_all(input), expected);
    }

     #[test]
     fn test_comments() {
         let input = r#"
             let x = 1; // This is a comment
             let y = 2; /* This is a
                            multi-line comment */ let z = 3;
             // Another comment
             /* Nested /* comment */ ok */
             let w = 4;
             let v = 5 / 2; // Division, not comment
         "#;
         let expected = vec![
             Token::Keyword(Keyword::Let), Token::Ident("x".to_string()), Token::Assignment(Assignment::Assign), Token::Int("1".to_string()), Token::Punctuation(Punctuation::Semicolon),
             Token::Keyword(Keyword::Let), Token::Ident("y".to_string()), Token::Assignment(Assignment::Assign), Token::Int("2".to_string()), Token::Punctuation(Punctuation::Semicolon),
             Token::Keyword(Keyword::Let), Token::Ident("z".to_string()), Token::Assignment(Assignment::Assign), Token::Int("3".to_string()), Token::Punctuation(Punctuation::Semicolon),
             Token::Keyword(Keyword::Let), Token::Ident("w".to_string()), Token::Assignment(Assignment::Assign), Token::Int("4".to_string()), Token::Punctuation(Punctuation::Semicolon),
             Token::Keyword(Keyword::Let), Token::Ident("v".to_string()), Token::Assignment(Assignment::Assign), Token::Int("5".to_string()), Token::Operator(Operator::Divide), Token::Int("2".to_string()), Token::Punctuation(Punctuation::Semicolon),
         ];
        assert_eq!(lex_all(input), expected);
     }


    #[test]
    fn test_illegal_char() {
        let input = "let a = @;";
        let expected = vec![
            Token::Keyword(Keyword::Let),
            Token::Ident("a".to_string()),
            Token::Assignment(Assignment::Assign),
            Token::Special(Special::Illegal), // '@' is illegal
            Token::Punctuation(Punctuation::Semicolon),
        ];
        assert_eq!(lex_all(input), expected);
    }
}
