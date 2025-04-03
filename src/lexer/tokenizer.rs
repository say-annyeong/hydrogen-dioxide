use std::iter::Peekable;
use std::str::Chars;

use super::token::{lookup_keyword, Assignment, Keyword, Operator, Position, Punctuation, Special, Token};

#[derive(Debug, Clone)]
pub struct Tokenizer<'a> {
    input: Peekable<Chars<'a>>,
    current_pos: usize, // Tracks the byte position in the original input string
    line: usize,
    column: usize,
    // Keep track of the start of the *next* token
    start_line: usize,
    start_column: usize,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        Tokenizer {
            input: input.chars().peekable(),
            current_pos: 0,
            line: 1,
            column: 1, // Start column at 1
            start_line: 1,
            start_column: 1,
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

    // Helper to create a Position struct for the *just consumed* token
    fn current_token_pos(&self, start_line: usize, start_column: usize) -> Position {
        Position {
            start_line,
            start_column,
            end_line: self.line,
            end_column: self.column, // `column` points to the *next* char
        }
    }

    // Call this *before* consuming the first char of the next token
    fn mark_token_start(&mut self) {
        self.start_line = self.line;
        self.start_column = self.column;
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            match self.peek_char() {
                Some(&ch) if ch.is_whitespace() => {
                    self.next_char(); // Consume whitespace
                    self.mark_token_start(); // Reset start after whitespace
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
                            self.mark_token_start(); // Reset start after // comment
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
                            self.mark_token_start(); // Reset start after /* comment
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
        let _start_line = self.line; // Keep original start line for error msg
        let _start_col = self.column; // Keep original start col for error msg

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
                            _start_line, _start_col
                        ));
                    }
                     else {
                        s.push(ch);
                    }
                }
                None => {
                     // Use the captured start line/col for the error message
                    return Err(format!(
                        "Unterminated string literal: EOF encountered (started at {}:{})",
                        _start_line, _start_col
                    )); // End of file before closing quote
                }
            }
        }
    }


    pub fn next_token(&mut self) -> (Token, Position) {
        self.skip_whitespace_and_comments();

        // Mark the beginning of the potential token *before* consuming the first char
        self.mark_token_start();
        let start_line = self.start_line;
        let start_col = self.start_column;

        match self.next_char() {
            Some(ch) => {
                let token = match ch {
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
                };
                let pos = self.current_token_pos(start_line, start_col); // Calculate pos *after* token is formed
                 (token, pos) // Return the tuple
            }
            None => (Token::Special(Special::Eof), self.current_token_pos(start_line, start_col)), // EOF position
        }
    }
}

// Implement Iterator to make it easy to loop through tokens
impl<'a> Iterator for Tokenizer<'a> {
    type Item = (Token, Position); // Yield token AND position

    fn next(&mut self) -> Option<Self::Item> {
         // next_token now directly returns Option<(Token, Position)> effectively
         let (token, position) = self.next_token();

         match token {
             Token::Special(Special::Eof) => None, // Stop iteration at EOF
             _ => Some((token, position)), // Return the token and its position
         }
    }
}

// --- Update Tests --- //

// Helper to compare tokens ignoring position - This is now simpler as Token doesn't have pos
fn tokens_match(a: &Token, b: &Token) -> bool {
    a == b // Use the derived PartialEq directly
}

// Helper to assert token sequence equality ignoring position
fn assert_tokens_equal(actual: Vec<(Token, Position)>, expected: Vec<Token>) { // Actual is Vec<(Token, Position)>, Expected is Vec<Token>
    assert_eq!(actual.len(), expected.len(),
               "Mismatch in token count.\nActual: {:?}\nExpected: {:?}", actual.iter().map(|(t,_)| t).collect::<Vec<_>>(), expected);
    for (i, ((act_token, _act_pos), exp_token)) in actual.iter().zip(expected.iter()).enumerate() {
        assert!(tokens_match(act_token, exp_token),
                "Token mismatch at index {}.\nActual: {:?}\nExpected: {:?}", i, act_token, exp_token);
    }
}


#[cfg(test)]
mod tests {
    use super::*; // Brings in helpers like assert_tokens_equal
    // No need for Position::default() here anymore
    use crate::lexer::token::{Assignment, Keyword, Operator, Position, Punctuation, Special, Token};

    // Convenience functions create regular Tokens
    fn ident(s: &str) -> Token { Token::Ident(s.to_string()) }
    fn int(s: &str) -> Token { Token::Int(s.to_string()) }
    fn float(s: &str) -> Token { Token::Float(s.to_string()) }
    fn string(s: &str) -> Token { Token::String(s.to_string()) }
    fn op(o: Operator) -> Token { Token::Operator(o) }
    fn assign(a: Assignment) -> Token { Token::Assignment(a) }
    fn punc(p: Punctuation) -> Token { Token::Punctuation(p) }
    fn kw(k: Keyword) -> Token { Token::Keyword(k) }
    fn special(s: Special) -> Token { Token::Special(s) }


    fn lex_all(input: &str) -> Vec<(Token, Position)> { // Return type is correct
        Tokenizer::new(input).collect()
    }

    // Tests remain the same structurally, using the helper constructors
    #[test]
    fn test_simple_assignment() {
        let input = "let x = 5;";
        let expected = vec![
            kw(Keyword::Let),
            ident("x"),
            assign(Assignment::Assign),
            int("5"),
            punc(Punctuation::Semicolon),
        ];
        assert_tokens_equal(lex_all(input), expected);
    }

    #[test]
    fn test_operators_and_parens() {
        let input = "(1 + 2) * 3";
         let expected = vec![
            punc(Punctuation::LeftParen),
            int("1"),
            op(Operator::Plus),
            int("2"),
            punc(Punctuation::RightParen),
            op(Operator::Multiply),
            int("3"),
         ];
        assert_tokens_equal(lex_all(input), expected);
    }

    #[test]
    fn test_keywords_and_ident() {
        let input = "fn main() { return false; }";
        let expected = vec![
            kw(Keyword::Fn),
            ident("main"),
            punc(Punctuation::LeftParen),
            punc(Punctuation::RightParen),
            punc(Punctuation::LeftBrace),
            kw(Keyword::Return),
            kw(Keyword::False),
            punc(Punctuation::Semicolon),
            punc(Punctuation::RightBrace),
        ];
        assert_tokens_equal(lex_all(input), expected);
    }

     #[test]
    fn test_string_literal() {
        let input = "let msg = \"Hello, Oxygen!\";";
        let expected = vec![
            kw(Keyword::Let),
            ident("msg"),
            assign(Assignment::Assign),
            string("Hello, Oxygen!"),
            punc(Punctuation::Semicolon),
        ];
        assert_tokens_equal(lex_all(input), expected);
    }

     #[test]
    fn test_single_quote_string_literal() {
        let input = "let msg = 'World';";
        let expected = vec![
            kw(Keyword::Let),
            ident("msg"),
            assign(Assignment::Assign),
            string("World"),
            punc(Punctuation::Semicolon),
        ];
        assert_tokens_equal(lex_all(input), expected);
    }

    #[test]
    fn test_backtick_string_literal() {
        let input = "let html = `<html>\n<body></body>\n</html>`;";
        let expected = vec![
            kw(Keyword::Let),
            ident("html"),
            assign(Assignment::Assign),
            string("<html>\n<body></body>\n</html>"),
            punc(Punctuation::Semicolon),
        ];
        assert_tokens_equal(lex_all(input), expected);
    }


     #[test]
    fn test_string_escape_sequences() {
        let input = r#"let escapes = "line1\nline2\t'quote'\"quote\"\\backslash";"#;
        let expected = vec![
            kw(Keyword::Let),
            ident("escapes"),
            assign(Assignment::Assign),
            string("line1\nline2\t'quote'\"quote\"\\backslash"),
            punc(Punctuation::Semicolon),
        ];
        assert_tokens_equal(lex_all(input), expected);
    }

    #[test]
    fn test_unterminated_string() {
        let input = "let bad = \"hello";
        let tokens = lex_all(input);
         let expected = vec![
             kw(Keyword::Let),
             ident("bad"),
             assign(Assignment::Assign),
             special(Special::Illegal), // String reading fails
         ];
         assert_tokens_equal(tokens, expected);
    }


     #[test]
    fn test_float_literal() {
        let input = "let pi = 3.14159;";
        let expected = vec![
            kw(Keyword::Let),
            ident("pi"),
            assign(Assignment::Assign),
            float("3.14159"),
            punc(Punctuation::Semicolon),
        ];
        assert_tokens_equal(lex_all(input), expected);
    }

     #[test]
     fn test_dot_operator_vs_float() {
         let input = "obj.method(5.0)";
         let expected = vec![
             ident("obj"),
             punc(Punctuation::Dot),
             ident("method"),
             punc(Punctuation::LeftParen),
             float("5.0"),
             punc(Punctuation::RightParen),
         ];
        assert_tokens_equal(lex_all(input), expected);
     }


    #[test]
    fn test_multi_char_operators() {
        let input = "x += 1; y -= 2; z *= 3; w /= 4; m %= 5; a == b; c != d; e <= f; g >= h; cond && other; next || prev; !flag; fn() -> void";
        let expected = vec![
            ident("x"), assign(Assignment::PlusAssign), int("1"), punc(Punctuation::Semicolon),
            ident("y"), assign(Assignment::MinusAssign), int("2"), punc(Punctuation::Semicolon),
            ident("z"), assign(Assignment::MultiplyAssign), int("3"), punc(Punctuation::Semicolon),
            ident("w"), assign(Assignment::DivideAssign), int("4"), punc(Punctuation::Semicolon),
            ident("m"), assign(Assignment::ModuloAssign), int("5"), punc(Punctuation::Semicolon),
            ident("a"), op(Operator::Equal), ident("b"), punc(Punctuation::Semicolon),
            ident("c"), op(Operator::NotEqual), ident("d"), punc(Punctuation::Semicolon),
            ident("e"), op(Operator::LessEqual), ident("f"), punc(Punctuation::Semicolon),
            ident("g"), op(Operator::GreaterEqual), ident("h"), punc(Punctuation::Semicolon),
            ident("cond"), op(Operator::And), ident("other"), punc(Punctuation::Semicolon),
            ident("next"), op(Operator::Or), ident("prev"), punc(Punctuation::Semicolon),
            op(Operator::Not), ident("flag"), punc(Punctuation::Semicolon),
             kw(Keyword::Fn), punc(Punctuation::LeftParen), punc(Punctuation::RightParen), punc(Punctuation::Arrow), ident("void"), // Treat void as ident for now
        ];
        assert_tokens_equal(lex_all(input), expected);
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
             kw(Keyword::Let), ident("x"), assign(Assignment::Assign), int("1"), punc(Punctuation::Semicolon),
             kw(Keyword::Let), ident("y"), assign(Assignment::Assign), int("2"), punc(Punctuation::Semicolon),
             kw(Keyword::Let), ident("z"), assign(Assignment::Assign), int("3"), punc(Punctuation::Semicolon),
             kw(Keyword::Let), ident("w"), assign(Assignment::Assign), int("4"), punc(Punctuation::Semicolon),
             kw(Keyword::Let), ident("v"), assign(Assignment::Assign), int("5"), op(Operator::Divide), int("2"), punc(Punctuation::Semicolon),
         ];
        assert_tokens_equal(lex_all(input), expected);
     }


    #[test]
    fn test_illegal_char() {
        let input = "let a = @;";
        let expected = vec![
            kw(Keyword::Let),
            ident("a"),
            assign(Assignment::Assign),
            special(Special::Illegal), // '@' is illegal
            punc(Punctuation::Semicolon),
        ];
        assert_tokens_equal(lex_all(input), expected);
    }
}
