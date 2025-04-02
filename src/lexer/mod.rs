pub mod astgen;
pub mod token;
pub mod tokenizer;
pub mod parser;

pub use token::Token;
pub use tokenizer::Tokenizer;
pub use parser::{Parser, ParseError};
pub use astgen::Program;

pub fn lexer(code: &str) -> Result<Program, ParseError> {
    let tokenizer = Tokenizer::new(code);
    let mut parser = Parser::new(tokenizer);
    let program = parser.parse_program();
    Ok(program)
}