#![allow(dead_code)] // TODO: Remove this later
#![allow(unused_variables)] // TODO: Remove this later

use super::astgen::{
    BinaryOperator, BlockStatement, Expression, FieldDefinition, Identifier, IfAlternative,
    ImportDeclaration, Literal, MethodDefinition, Program, Statement, StructDefinition,
    TypeAnnotation, UnaryOperator, /* other AST nodes */
};
use super::token::{Assignment, Keyword, Operator, Punctuation, Special, Token, Position};
use super::tokenizer::Tokenizer;
use std::iter::Peekable;

// Basic error type for parsing
#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    // Found Token (Debug string), Expected description, Position
    UnexpectedToken(String, String, Position),
    // Expected description, Position of EOF
    UnexpectedEof(String, Position),
    // General error message, Position where it occurred (best guess)
    Other(String, Position),
}

// Wrapper for the tokenizer iterator to simplify peeking
type TokenStreamItem = (Token, Position);
type TokenStreamIterator<'a> = Peekable<Tokenizer<'a>>;

pub struct Parser<'a> {
    tokenizer: TokenStreamIterator<'a>,
    // Store the position of the *last consumed* token for error reporting
    last_pos: Position,
    errors: Vec<ParseError>,
}

// --- Helper Macros ---
// Consumes the next token if it matches the pattern, otherwise returns Err.
macro_rules! consume_token {
    ($self:ident, $pattern:pat => $expr:expr, $expected_msg:expr) => {
         {
             // Peek first to get position before consuming
             let pos = match $self.peek_token_pos() {
                Some(p) => *p, // Deref the borrowed position
                None => $self.last_pos, // Use last known position if EOF
             };
             match $self.next_token() {
                 Some($pattern) => Ok($expr),
                 Some(other) => Err(ParseError::UnexpectedToken(
                     format!("{:?}", other),
                     $expected_msg.to_string(),
                     pos // Use the position from peek or last_pos
                 )),
                 None => Err(ParseError::UnexpectedEof(
                     $expected_msg.to_string(),
                     pos // Use the position from peek or last_pos
                 )),
             }
         }
    };
     // Simpler version without extracting value
     ($self:ident, $pattern:pat, $expected_msg:expr) => {
        consume_token!($self, $pattern => (), $expected_msg)
    };
}

// Peeks at the next token, if it matches pattern, consumes it and returns true, otherwise false.
macro_rules! consume_optional_token {
     ($self:ident, $pattern:pat) => {
         if let Some($pattern) = $self.peek_token() {
             $self.next_token(); // Consume
             true
         } else {
             false
         }
     };
 }

// --- Precedence Enum (for future Pratt parser) ---
#[derive(PartialEq, PartialOrd, Ord, Eq, Debug, Clone, Copy)]
enum Precedence {
    Lowest,
    Assign,      // =
    LogicalOr,   // ||
    LogicalAnd,  // &&
    Equality,    // == !=
    Comparison,  // < > <= >=
    Term,        // + -
    Factor,      // * / %
    Unary,       // - !
    Call,        // . () []
}

// Helper to get precedence (defined *before* Parser impl)
fn get_token_precedence(op: &Operator) -> Precedence {
    match op {
        Operator::Equal | Operator::NotEqual => Precedence::Equality,
        Operator::LessThan | Operator::GreaterThan | Operator::LessEqual | Operator::GreaterEqual => Precedence::Comparison,
        Operator::Plus | Operator::Minus => Precedence::Term,
        Operator::Multiply | Operator::Divide | Operator::Modulo => Precedence::Factor,
        Operator::And => Precedence::LogicalAnd,
        Operator::Or => Precedence::LogicalOr,
        Operator::Not => Precedence::Unary, // Assign unary precedence
    }
}

// Helper to get token precedence from a borrowed Token
fn get_peeked_token_precedence(token: &Token) -> Precedence {
    match token {
        Token::Operator(op) => get_token_precedence(op),
         Token::Punctuation(Punctuation::LeftParen) => Precedence::Call,
         Token::Punctuation(Punctuation::Dot) => Precedence::Call,
         Token::Punctuation(Punctuation::LeftBracket) => Precedence::Call,
         Token::Assignment(Assignment::Assign) => Precedence::Assign,
        _ => Precedence::Lowest,
    }
}

impl<'a> Parser<'a> {
    pub fn new(tokenizer: Tokenizer<'a>) -> Self {
        Parser {
            tokenizer: tokenizer.peekable(),
            last_pos: Position::default(),
            errors: Vec::new(),
        }
    }

    // --- Token Helpers --- Updated for (Token, Position) ---
    fn peek_token(&mut self) -> Option<&Token> {
         self.tokenizer.peek().map(|(token, _pos)| token)
    }

    fn peek_token_pos(&mut self) -> Option<&Position> {
        self.tokenizer.peek().map(|(_token, pos)| pos)
    }

    fn next_token(&mut self) -> Option<Token> {
         match self.tokenizer.next() {
             Some((token, pos)) => {
                 self.last_pos = pos; // Update last position
                 Some(token)
             }
             None => None,
         }
    }

    // Checks the token *kind*, ignoring position
    fn check_peek(&mut self, expected: &Token) -> bool {
        self.peek_token().map_or(false, |t| t == expected)
    }

    // --- Error Reporting ---
    fn record_error(&mut self, error: ParseError) {
        self.errors.push(error);
    }

     // Consumes the current token if it matches predicate, else records error
     fn expect_and_consume<F>(&mut self, predicate: F, expected_msg: &str) -> Result<(), ParseError>
     where F: FnOnce(&Token) -> bool {
         match self.peek_token() {
             Some(token) if predicate(token) => {
                 self.next_token(); // Consume
                 Ok(())
             }
             Some(other) => Err(ParseError::UnexpectedToken(format!("{:?}", other), expected_msg.to_string(), self.last_pos)),
             None => Err(ParseError::UnexpectedEof(expected_msg.to_string(), self.last_pos)),
         }
     }


    pub fn errors(&self) -> &[ParseError] {
        &self.errors
    }

    // --- Main Parsing Logic ---
    pub fn parse_program(&mut self) -> Program {
        let mut statements = Vec::new();
        while self.peek_token().is_some()
            && !self.check_peek(&Token::Special(Special::Eof))
        {
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(e) => {
                    // Record the first error encountered
                    self.record_error(e);
                    // Stop parsing immediately after the first error
                    break;
                }
            }
        }
        Program { statements }
    }

    // --- Statement Parsers ---
    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.peek_token() {
            Some(Token::Keyword(Keyword::Let)) => self.parse_let_declaration(),
            Some(Token::Keyword(Keyword::Return)) => self.parse_return_statement(),
            Some(Token::Keyword(Keyword::Fn)) => self.parse_fn_declaration(),
            Some(Token::Keyword(Keyword::If)) => self.parse_if_statement(),
            Some(Token::Keyword(Keyword::While)) => self.parse_while_statement(),
            Some(Token::Keyword(Keyword::For)) => self.parse_for_statement(),
            Some(Token::Keyword(Keyword::Struct)) => self.parse_struct_declaration(),
            Some(Token::Keyword(Keyword::Import)) | Some(Token::Keyword(Keyword::From)) => {
                self.parse_import_statement()
            }
            Some(Token::Punctuation(Punctuation::LeftBrace)) => {
                // Allow standalone block? Let's disallow for now.
                Err(ParseError::UnexpectedToken(format!("{:?}", self.peek_token().unwrap()), "Expected statement start keyword or expression".to_string(), self.last_pos))
            }
            // Handle empty statements (lone semicolons)
            Some(Token::Punctuation(Punctuation::Semicolon)) => {
                self.next_token(); // Consume the semicolon
                // Return an empty expression statement
                Ok(Statement::ExpressionStatement(Expression::Literal(Literal::Null)))
            }
            Some(_) => self.parse_expression_statement(),
            None => Err(ParseError::UnexpectedEof("Expected statement".to_string(), self.last_pos)),
        }
    }

    fn parse_let_declaration(&mut self) -> Result<Statement, ParseError> {
        self.next_token(); // Consume 'let'
        let name = self.parse_identifier()?;

        let mut type_annotation = None;
        if consume_optional_token!(self, Token::Punctuation(Punctuation::Colon)) {
            type_annotation = Some(self.parse_type_annotation()?);
        }

        let mut initializer = None;
        if consume_optional_token!(self, Token::Assignment(Assignment::Assign)) {
            initializer = Some(self.parse_expression(Precedence::Lowest)?);
        }

        // Optional semicolon
        consume_optional_token!(self, Token::Punctuation(Punctuation::Semicolon));

        Ok(Statement::LetDeclaration {
            name,
            type_annotation,
            initializer,
        })
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParseError> {
        self.next_token(); // Consume 'return'
        let mut value = None;
        if !self.check_peek(&Token::Punctuation(Punctuation::Semicolon))
            && !self.check_peek(&Token::Punctuation(Punctuation::RightBrace))
        {
            value = Some(self.parse_expression(Precedence::Lowest)?);
        }
        // Optional semicolon
        consume_optional_token!(self, Token::Punctuation(Punctuation::Semicolon));
        Ok(Statement::ReturnStatement { value })
    }

    fn parse_fn_declaration(&mut self) -> Result<Statement, ParseError> {
        self.next_token(); // Consume 'fn'
        let name = self.parse_identifier()?;

        consume_token!(self, Token::Punctuation(Punctuation::LeftParen), "Expected '(' after function name")?;

        let parameters = self.parse_parameter_list()?;

        let mut return_type = None;
        if consume_optional_token!(self, Token::Punctuation(Punctuation::Arrow)) {
            return_type = Some(self.parse_type_annotation()?);
        }

        let body = self.parse_block_statement()?;

        Ok(Statement::FunctionDeclaration {
            name,
            parameters,
            return_type,
            body,
        })
    }

     fn parse_parameter_list(&mut self) -> Result<Vec<(Identifier, Option<TypeAnnotation>)>, ParseError> {
         let mut params = Vec::new();
         if self.check_peek(&Token::Punctuation(Punctuation::RightParen)) {
             self.next_token(); // Consume ')'
             return Ok(params);
         }

         loop {
             let param_name = self.parse_identifier()?;
             let mut param_type = None;
             if consume_optional_token!(self, Token::Punctuation(Punctuation::Colon)) {
                 param_type = Some(self.parse_type_annotation()?);
             }
             params.push((param_name, param_type));

             if !consume_optional_token!(self, Token::Punctuation(Punctuation::Comma)) {
                 break;
             }
         }

         consume_token!(self, Token::Punctuation(Punctuation::RightParen), "Expected ')' or ',' in parameter list")?;
         Ok(params)
     }

    fn parse_if_statement(&mut self) -> Result<Statement, ParseError> {
        self.next_token(); // Consume 'if'
        consume_token!(self, Token::Punctuation(Punctuation::LeftParen), "Expected '(' after 'if'")?;
        let condition = self.parse_expression(Precedence::Lowest)?;
        consume_token!(self, Token::Punctuation(Punctuation::RightParen), "Expected ')' after if condition")?;
        let consequence = self.parse_block_statement()?;
        let alternative = self.parse_if_alternative()?;

        Ok(Statement::IfStatement {
            condition,
            consequence,
            alternative,
        })
    }

     fn parse_if_alternative(&mut self) -> Result<Option<IfAlternative>, ParseError> {
         if consume_optional_token!(self, Token::Keyword(Keyword::Elif)) {
             consume_token!(self, Token::Punctuation(Punctuation::LeftParen), "Expected '(' after 'elif'")?;
             let condition = self.parse_expression(Precedence::Lowest)?;
             consume_token!(self, Token::Punctuation(Punctuation::RightParen), "Expected ')' after elif condition")?;
             let consequence = self.parse_block_statement()?;
             let alternative = self.parse_if_alternative()?; // Recursive call for more elif/else
             Ok(Some(IfAlternative::Elif {
                 condition,
                 consequence,
                 alternative: alternative.map(Box::new),
             }))
         } else if consume_optional_token!(self, Token::Keyword(Keyword::Else)) {
             let consequence = self.parse_block_statement()?;
             Ok(Some(IfAlternative::Else { consequence }))
         } else {
             Ok(None) // No elif or else
         }
     }

     fn parse_while_statement(&mut self) -> Result<Statement, ParseError> {
         self.next_token(); // Consume 'while'
         consume_token!(self, Token::Punctuation(Punctuation::LeftParen), "Expected '(' after 'while'")?;
         let condition = self.parse_expression(Precedence::Lowest)?;
         consume_token!(self, Token::Punctuation(Punctuation::RightParen), "Expected ')' after while condition")?;
         let body = self.parse_block_statement()?;
         Ok(Statement::WhileStatement { condition, body })
     }

     fn parse_for_statement(&mut self) -> Result<Statement, ParseError> {
         self.next_token(); // Consume 'for'
         let variable = self.parse_identifier()?;
         // Expect the identifier "in" - not a keyword in the spec
          let in_token = self.next_token().ok_or_else(|| ParseError::UnexpectedEof("Expected 'in' after variable in for loop".to_string(), self.last_pos))?;
          match in_token {
              Token::Ident(ref s) if s == "in" => { /* continue */ }
              _ => return Err(ParseError::UnexpectedToken(format!("{:?}", in_token), "Expected 'in'".to_string(), self.last_pos)),
          }

         let iterable = self.parse_expression(Precedence::Lowest)?;
         let body = self.parse_block_statement()?;
         Ok(Statement::ForStatement { variable, iterable, body })
     }

      fn parse_struct_declaration(&mut self) -> Result<Statement, ParseError> {
          self.next_token(); // Consume 'struct'
          let name = self.parse_identifier()?;
          consume_token!(self, Token::Punctuation(Punctuation::LeftBrace), "Expected '{' after struct name")?;

          let mut fields = Vec::new();
          let methods = Vec::new(); // Removed mut

          while !self.check_peek(&Token::Punctuation(Punctuation::RightBrace)) && self.peek_token().is_some() {
              // Check if it's a method definition (`fn`) or a field definition
              match self.peek_token() {
                  Some(Token::Keyword(Keyword::Fn)) => {
                      // TODO: Implement Method parsing - requires handling `self`?
                      // For now, consume tokens related to a potential method to avoid infinite loop
                      self.record_error(ParseError::Other("Method parsing within struct not implemented yet.".to_string(), self.last_pos));
                      // Consume 'fn' and potentially identifier + block to recover somewhat
                      self.next_token(); // consume fn
                       if let Some(Token::Ident(_)) = self.peek_token() { self.next_token(); }
                       if let Some(Token::Punctuation(Punctuation::LeftBrace)) = self.peek_token() {
                            match self.parse_block_statement() { // Attempt to parse block to consume it
                                Ok(_) => {},
                                Err(e) => self.record_error(e),
                            }
                       }

                      // methods.push(self.parse_method_definition()?); // Replace above recovery with this
                  }
                  Some(Token::Ident(_)) => {
                      // Assume field definition: name: Type,
                       let field_name = self.parse_identifier()?;
                       consume_token!(self, Token::Punctuation(Punctuation::Colon), "Expected ':' after field name")?;
                       let field_type = self.parse_type_annotation()?;
                        consume_optional_token!(self, Token::Punctuation(Punctuation::Comma)); // Optional comma
                       fields.push(FieldDefinition { name: field_name, type_annotation: field_type });
                  }
                   Some(other) => {
                        let e = ParseError::UnexpectedToken(format!("{:?}", other), "Expected field (identifier) or method ('fn') in struct body".to_string(), self.last_pos);
                        self.record_error(e);
                        self.next_token(); // Consume the unexpected token to attempt recovery
                   }
                  None => return Err(ParseError::UnexpectedEof("Expected field, method, or '}' in struct body".to_string(), self.last_pos)),
              }
          }

          consume_token!(self, Token::Punctuation(Punctuation::RightBrace), "Expected '}' to end struct definition")?;

          Ok(Statement::StructDeclaration(StructDefinition { name, fields, methods }))
      }

      fn parse_import_statement(&mut self) -> Result<Statement, ParseError> {
          if let Some(Token::Keyword(Keyword::Import)) = self.peek_token() {
              self.next_token(); // Consume 'import'
              // Expect path (string literal)
               let path = consume_token!(self, Token::String(s) => s, "Expected string literal for module path after 'import'")?;

              let mut alias = None;
              if consume_optional_token!(self, Token::Keyword(Keyword::As)) {
                  alias = Some(self.parse_identifier()?);
              }
              consume_optional_token!(self, Token::Punctuation(Punctuation::Semicolon));
              Ok(Statement::ImportStatement(ImportDeclaration::ImportModule { path, alias }))

          } else if let Some(Token::Keyword(Keyword::From)) = self.peek_token() {
              self.next_token(); // Consume 'from'
               let path = consume_token!(self, Token::String(s) => s, "Expected string literal for module path after 'from'")?;
              consume_token!(self, Token::Keyword(Keyword::Import), "Expected 'import' after path in 'from' statement")?;

              let mut symbols = Vec::new();
              loop {
                  let name = self.parse_identifier()?;
                  let mut alias = None;
                  if consume_optional_token!(self, Token::Keyword(Keyword::As)) {
                      alias = Some(self.parse_identifier()?);
                  }
                  symbols.push((name, alias));
                  if !consume_optional_token!(self, Token::Punctuation(Punctuation::Comma)) {
                      break;
                  }
              }
               consume_optional_token!(self, Token::Punctuation(Punctuation::Semicolon));
              Ok(Statement::ImportStatement(ImportDeclaration::ImportSymbols { path, symbols }))
          } else {
               // Should not happen if called correctly from parse_statement
               Err(ParseError::Other("Internal error: parse_import_statement called unexpectedly".to_string(), self.last_pos))
          }
      }


    fn parse_expression_statement(&mut self) -> Result<Statement, ParseError> {
        // Check if the next token is a semicolon, which would indicate an empty expression
        if self.check_peek(&Token::Punctuation(Punctuation::Semicolon)) {
            self.next_token(); // Consume the semicolon
            return Ok(Statement::ExpressionStatement(Expression::Literal(Literal::Null)));
        }
        
        let expression = self.parse_expression(Precedence::Lowest)?;
        // Optional semicolon
        consume_optional_token!(self, Token::Punctuation(Punctuation::Semicolon));
        Ok(Statement::ExpressionStatement(expression))
    }

    // --- Block and Helper Parsers ---
    fn parse_block_statement(&mut self) -> Result<BlockStatement, ParseError> {
        consume_token!(self, Token::Punctuation(Punctuation::LeftBrace), "Expected '{' to start block")?;
        let mut statements = Vec::new();
        while !self.check_peek(&Token::Punctuation(Punctuation::RightBrace))
            && self.peek_token().is_some()
        {
            statements.push(self.parse_statement()?);
        }
        consume_token!(self, Token::Punctuation(Punctuation::RightBrace), "Expected '}' to end block")?;
        Ok(BlockStatement { statements })
    }

     fn parse_identifier(&mut self) -> Result<Identifier, ParseError> {
         consume_token!(self, Token::Ident(s) => Identifier { name: s }, "Expected identifier")
     }

    // Basic version, handles simple `Ident` and `Ident<Type>`
    fn parse_type_annotation(&mut self) -> Result<TypeAnnotation, ParseError> {
        let base_ident = self.parse_identifier()?;

        if base_ident.name == "void" {
             return Ok(TypeAnnotation::Void);
        }

        // Check for generic arguments (e.g., List<int>)
        if consume_optional_token!(self, Token::Operator(Operator::LessThan)) {
             let mut arguments = Vec::new();
             if !self.check_peek(&Token::Operator(Operator::GreaterThan)) {
                  loop {
                      arguments.push(self.parse_type_annotation()?); // Recursive call
                      if !consume_optional_token!(self, Token::Punctuation(Punctuation::Comma)) {
                          break;
                      }
                  }
             }
              // Use expect_and_consume for better error on missing '>'
             self.expect_and_consume(|t| *t == Token::Operator(Operator::GreaterThan), "Expected '>' after generic type arguments")?;

             Ok(TypeAnnotation::Generic { base: base_ident, arguments })
        } else {
             Ok(TypeAnnotation::Simple(base_ident))
        }
    }

    // --- Expression Parsing (Rudimentary) ---
    // Needs replacement with Pratt parsing for correctness
    fn parse_expression(&mut self, _precedence: Precedence) -> Result<Expression, ParseError> {
        // Prefix parsing - Match needs to yield Result<Expression, ParseError>
        let mut left_expr = match self.peek_token() {
            Some(Token::Int(_)) => {
                if let Some(Token::Int(s)) = self.next_token() { // Consume
                    Ok(Expression::Literal(Literal::Int(s)))
                } else { unreachable!("Token mismatch after peek") }
            }
            Some(Token::Float(_)) => {
                 if let Some(Token::Float(s)) = self.next_token() { // Consume
                     Ok(Expression::Literal(Literal::Float(s)))
                 } else { unreachable!("Token mismatch after peek") }
            }
            Some(Token::String(_)) => {
                 if let Some(Token::String(s)) = self.next_token() { // Consume
                     Ok(Expression::Literal(Literal::String(s)))
                 } else { unreachable!("Token mismatch after peek") }
            }
            Some(Token::Keyword(Keyword::True)) => { self.next_token(); Ok(Expression::Literal(Literal::Boolean(true))) }
            Some(Token::Keyword(Keyword::False)) => { self.next_token(); Ok(Expression::Literal(Literal::Boolean(false))) }
            Some(Token::Keyword(Keyword::Null)) => { self.next_token(); Ok(Expression::Literal(Literal::Null)) }
            Some(Token::Ident(_)) => {
                 if let Some(Token::Ident(s)) = self.next_token() { // Consume
                    Ok(Expression::Identifier(Identifier { name: s }))
                 } else { unreachable!("Token mismatch after peek") }
            }
            Some(Token::Punctuation(Punctuation::LeftParen)) => {
                self.next_token(); // Consume '('
                let expr = self.parse_expression(Precedence::Lowest)?;
                consume_token!(self, Token::Punctuation(Punctuation::RightParen), "Expected ')' after grouped expression")?;
                Ok(expr)
            }
            Some(Token::Operator(Operator::Minus)) | Some(Token::Operator(Operator::Not)) => {
                 let op_token = self.next_token().unwrap(); // Consume operator
                 let op = if op_token == Token::Operator(Operator::Minus) { UnaryOperator::Negate } else { UnaryOperator::Not };
                 let operand = self.parse_expression(Precedence::Unary)?; // Parse with higher precedence
                 Ok(Expression::UnaryOperation { op, operand: Box::new(operand) })
            }
             // These calls return Result, so just pass them through
             Some(Token::Punctuation(Punctuation::LeftBracket)) => self.parse_list_initializer(),
             Some(Token::Punctuation(Punctuation::LeftBrace)) => self.parse_dict_initializer(),

            Some(other) => {
                 let token_str = format!("{:?}", other.clone());
                 Err(ParseError::UnexpectedToken(
                     token_str,
                     "Expected literal, identifier, '(', '-', '!', '[', or '{'".to_string(),
                     self.last_pos
                 ))
            }
            None => Err(ParseError::UnexpectedEof("Expected expression".to_string(), self.last_pos)),
        }?; // Apply '?' to the Result produced by the match block

        // --- Infix/Postfix parsing loop --- (Revised for correct precedence climbing)
         loop {
             // Peek FIRST to check precedence before deciding to proceed
             let peeked_token = match self.peek_token() {
                 Some(t) => t,
                 None => break, // No more tokens
             };

             let peeked_precedence = get_peeked_token_precedence(peeked_token);

             // The core Pratt check: only continue if the next token binds tighter than the current context
             if _precedence >= peeked_precedence {
                 break;
             }

             // Now consume the token since we know we're handling it
             // Use next_token() directly inside the match for clarity
             match self.next_token().unwrap() { // Safe unwrap: we peeked Some(t) just before
                 Token::Operator(op) => {
                      // We already know op is not Operator::Not from prefix handling
                      let binary_op = match op {
                           Operator::Plus => BinaryOperator::Add,
                           Operator::Minus => BinaryOperator::Subtract,
                           Operator::Multiply => BinaryOperator::Multiply,
                           Operator::Divide => BinaryOperator::Divide,
                           Operator::Modulo => BinaryOperator::Modulo,
                           Operator::Equal => BinaryOperator::Equal,
                           Operator::NotEqual => BinaryOperator::NotEqual,
                           Operator::LessThan => BinaryOperator::LessThan,
                           Operator::GreaterThan => BinaryOperator::GreaterThan,
                           Operator::LessEqual => BinaryOperator::LessEqual,
                           Operator::GreaterEqual => BinaryOperator::GreaterEqual,
                           Operator::And => BinaryOperator::And,
                           Operator::Or => BinaryOperator::Or,
                           Operator::Not => unreachable!("Not operator should be handled by prefix parser"),
                      };
                      // Parse right operand with the *current* operator's precedence as the context
                      let right_expr = self.parse_expression(peeked_precedence)?;
                      left_expr = Expression::BinaryOperation {
                           left: Box::new(left_expr),
                           op: binary_op,
                           right: Box::new(right_expr),
                      };
                 }
                 Token::Assignment(Assignment::Assign) => {
                     // Assignment is right-associative, parse RHS with slightly lower precedence
                     // We subtract 1 from the current precedence level for right-associativity.
                     let current_precedence = Precedence::Assign;
                     // Handle potential underflow if Lowest is 0. Ensure Lowest is truly the lowest.
                     let right_precedence = if current_precedence > Precedence::Lowest {
                         // Find the next lower precedence level (this is slightly hacky without explicit enum values)
                         // A better approach might be to define integer values for precedence levels.
                         // For now, just using Lowest seems the most robust way to handle right-associativity
                         // in the absence of easily decrementable precedence. Let's revert to that.
                         // current_precedence - 1; // This requires defining subtraction or integer values.
                         Precedence::Lowest // Reverting to Lowest - Less correct but avoids complex enum logic for now.
                                         // NOTE: This makes assignment LEFT-associative (a = b = c -> (a = b) = c)
                                         // We need a better precedence system to fix this properly. For example:
                                         // let value = self.parse_expression(Precedence::Assign - 1)?; // If precedence were integers
                     } else {
                         Precedence::Lowest
                     };
                     
                     // Parse the right side of the assignment (the value being assigned)
                     let value = self.parse_expression(Precedence::Lowest)?; // Keep as Lowest for left-associative behavior

                     // Check if left_expr is a valid L-value (Identifier, MemberAccess, etc.)
                     // This check is better done during semantic analysis or interpretation.
                     left_expr = Expression::Assignment {
                         target: Box::new(left_expr),
                         value: Box::new(value),
                     };
                 }
                 Token::Punctuation(Punctuation::LeftParen) => {
                      // Arguments are parsed with lowest precedence
                      let arguments = self.parse_expression_list(Punctuation::RightParen)?;
                       consume_token!(self, Token::Punctuation(Punctuation::RightParen), "Expected ')' after function arguments")?;
                      left_expr = Expression::FunctionCall {
                           callee: Box::new(left_expr),
                           arguments,
                      };
                  }
                 Token::Punctuation(Punctuation::Dot) => {
                      let member = self.parse_identifier()?;
                      left_expr = Expression::MemberAccess {
                           base: Box::new(left_expr),
                           member,
                      };
                  }
                  Token::Punctuation(Punctuation::LeftBracket) => {
                      // Index expression is parsed with lowest precedence
                      let index = self.parse_expression(Precedence::Lowest)?;
                       consume_token!(self, Token::Punctuation(Punctuation::RightBracket), "Expected ']' after index expression")?;
                      left_expr = Expression::IndexAccess {
                           base: Box::new(left_expr),
                           index: Box::new(index),
                      };
                  }
                 // This case should ideally not be reached if peeked_precedence logic is correct
                 other => return Err(ParseError::Other(format!("Unexpected token {:?} in infix/postfix position after precedence check", other), self.last_pos)),
             }
         }

        Ok(left_expr)
    }

     // Helper for parsing comma-separated expressions until a closing delimiter
     fn parse_expression_list(&mut self, end_delimiter: Punctuation) -> Result<Vec<Expression>, ParseError> {
         let mut list = Vec::new();
         let end_token = Token::Punctuation(end_delimiter);

         if self.check_peek(&end_token) {
             // Empty list is handled by the caller consuming the end token
             return Ok(list);
         }

         loop {
             list.push(self.parse_expression(Precedence::Lowest)?);
             if !consume_optional_token!(self, Token::Punctuation(Punctuation::Comma)) {
                 break; // No comma, expect end delimiter next
             }
              if self.check_peek(&end_token) {
                 // Allow trailing comma
                 break;
              }
         }
         // Caller should consume the end_delimiter
         Ok(list)
     }

      fn parse_list_initializer(&mut self) -> Result<Expression, ParseError> {
         consume_token!(self, Token::Punctuation(Punctuation::LeftBracket), "Expected '[' for list initializer")?;
         let items = self.parse_expression_list(Punctuation::RightBracket)?;
         consume_token!(self, Token::Punctuation(Punctuation::RightBracket), "Expected ']' after list items")?;
         Ok(Expression::ListInitializer { items })
     }

      fn parse_dict_initializer(&mut self) -> Result<Expression, ParseError> {
          consume_token!(self, Token::Punctuation(Punctuation::LeftBrace), "Expected '{' for dict initializer")?;
          let mut pairs = Vec::new();
          let end_token = Token::Punctuation(Punctuation::RightBrace);

          if !self.check_peek(&end_token) {
              loop {
                  let key = self.parse_expression(Precedence::Lowest)?; // Allow expressions as keys? Spec shows strings.
                  consume_token!(self, Token::Punctuation(Punctuation::Colon), "Expected ':' after dict key")?;
                  let value = self.parse_expression(Precedence::Lowest)?;
                  pairs.push((key, value));

                  if !consume_optional_token!(self, Token::Punctuation(Punctuation::Comma)) {
                      break;
                  }
                   if self.check_peek(&end_token) {
                       // Allow trailing comma
                       break;
                   }
              }
          }
          consume_token!(self, Token::Punctuation(Punctuation::RightBrace), "Expected '}' after dict items")?;
          Ok(Expression::DictInitializer { pairs })
      }

}


// --- Basic Tests ---
#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::astgen::*; // Import all ast nodes for convenience in tests
    // No need for Assignment import if not used directly in asserts
    // use crate::lexer::token::Assignment;
    use crate::lexer::Tokenizer;

    // Helper to parse code and return program, checking for errors
    fn parse_ok(code: &str) -> Program {
        let tokenizer = Tokenizer::new(code);
        let mut parser = Parser::new(tokenizer);
        let program = parser.parse_program();
        assert!(
            parser.errors().is_empty(),
             // Enhance error message to show the code and error details
             "Parse failed on input:\n```\n{}\n```\nErrors: {:#?}",
             code,
            parser.errors()
        );
        program
    }

    #[test]
    fn test_empty_input() {
        let program = parse_ok("");
        assert!(program.statements.is_empty());
    }

    #[test]
    fn test_let_statement_simple() {
        let program = parse_ok("let x = 5;");
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::LetDeclaration {
                name,
                type_annotation,
                initializer,
            } => {
                assert_eq!(name.name, "x");
                assert!(type_annotation.is_none());
                assert_eq!(
                    initializer.as_ref().unwrap(),
                    &Expression::Literal(Literal::Int("5".to_string()))
                );
            }
            _ => panic!("Expected LetDeclaration"),
        }
    }

    #[test]
    fn test_let_statement_with_type() {
        let program = parse_ok("let greeting: string = \"hello\";");
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::LetDeclaration {
                name,
                type_annotation,
                initializer,
            } => {
                assert_eq!(name.name, "greeting");
                assert_eq!(
                    type_annotation.as_ref().unwrap(),
                    &TypeAnnotation::Simple(Identifier {
                        name: "string".to_string()
                    })
                );
                assert_eq!(
                    initializer.as_ref().unwrap(),
                    &Expression::Literal(Literal::String("hello".to_string()))
                );
            }
            _ => panic!("Expected LetDeclaration"),
        }
    }

     #[test]
     fn test_let_statement_no_semicolon() {
         let program = parse_ok("let x = 10"); // Simple style
         assert_eq!(program.statements.len(), 1);
         match &program.statements[0] {
             Statement::LetDeclaration { name, .. } => assert_eq!(name.name, "x"),
             _ => panic!("Expected LetDeclaration"),
         }
     }

    #[test]
    fn test_return_statement() {
        let program = parse_ok("return 10;");
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::ReturnStatement { value } => {
                assert_eq!(
                    value.as_ref().unwrap(),
                    &Expression::Literal(Literal::Int("10".to_string()))
                );
            }
            _ => panic!("Expected ReturnStatement"),
        }
    }

     #[test]
     fn test_return_no_value() {
         let program = parse_ok("return;");
         assert_eq!(program.statements.len(), 1);
         match &program.statements[0] {
             Statement::ReturnStatement { value } => assert!(value.is_none()),
             _ => panic!("Expected ReturnStatement"),
         }
     }


    #[test]
    fn test_expression_statement_call() {
        let program = parse_ok("print(\"test\");");
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::ExpressionStatement(Expression::FunctionCall { callee, arguments }) => {
                assert_eq!(
                    callee.as_ref(),
                    &Expression::Identifier(Identifier {
                        name: "print".to_string()
                    })
                );
                assert_eq!(arguments.len(), 1);
                assert_eq!(
                    arguments[0],
                    Expression::Literal(Literal::String("test".to_string()))
                );
            }
            _ => panic!("Expected ExpressionStatement(FunctionCall)"),
        }
    }

     #[test]
     fn test_basic_fn_declaration() {
         let program = parse_ok("fn main() { let x = 1; return x; }");
         assert_eq!(program.statements.len(), 1);
         match &program.statements[0] {
             Statement::FunctionDeclaration { name, parameters, return_type, body } => {
                 assert_eq!(name.name, "main");
                 assert!(parameters.is_empty());
                 assert!(return_type.is_none());
                 assert_eq!(body.statements.len(), 2); // Check block content
             }
             _ => panic!("Expected FunctionDeclaration"),
         }
     }

      #[test]
      fn test_fn_declaration_with_params_and_return() {
          let program = parse_ok("fn add(a: int, b: int) -> int { return a + b; }");
          assert_eq!(program.statements.len(), 1);
          match &program.statements[0] {
              Statement::FunctionDeclaration { name, parameters, return_type, body } => {
                  assert_eq!(name.name, "add");
                  assert_eq!(parameters.len(), 2);
                  assert_eq!(parameters[0].0.name, "a");
                  assert!(parameters[0].1.is_some());
                   assert_eq!(parameters[1].0.name, "b");
                  assert!(parameters[1].1.is_some());
                   assert!(return_type.is_some());
                   assert_eq!(return_type.as_ref().unwrap(), &TypeAnnotation::Simple(Identifier{name: "int".to_string()}));
                  assert_eq!(body.statements.len(), 1); // Check block content (return a+b)
              }
              _ => panic!("Expected FunctionDeclaration"),
          }
      }

       #[test]
      fn test_basic_if_statement() {
          let program = parse_ok("if (x > 0) { return true; }");
           assert_eq!(program.statements.len(), 1);
          match &program.statements[0] {
              Statement::IfStatement { condition, consequence, alternative } => {
                   // Cannot easily assert condition structure without better expr parsing
                   assert_eq!(consequence.statements.len(), 1);
                   assert!(alternative.is_none());
              }
              _ => panic!("Expected IfStatement"),
          }
      }

        #[test]
       fn test_if_else_statement() {
           let program = parse_ok("if (x) { x = 1; } else { x = 0; }");
            assert_eq!(program.statements.len(), 1);
           match &program.statements[0] {
               Statement::IfStatement { condition, consequence, alternative } => {
                    assert_eq!(consequence.statements.len(), 1);
                    assert!(alternative.is_some());
                    match alternative.as_ref().unwrap() {
                        IfAlternative::Else { consequence: else_consequence } => {
                             assert_eq!(else_consequence.statements.len(), 1);
                        }
                         _ => panic!("Expected Else alternative"),
                    }
               }
               _ => panic!("Expected IfStatement"),
           }
       }

        #[test]
       fn test_if_elif_else_statement() {
           let program = parse_ok("if (x > 10) { return 1; } elif (x > 5) { return 2; } else { return 3; }");
            assert_eq!(program.statements.len(), 1);
            match &program.statements[0] {
                 Statement::IfStatement { condition, consequence, alternative } => {
                     assert_eq!(consequence.statements.len(), 1);
                     assert!(alternative.is_some());
                     match alternative.as_ref().unwrap() {
                          IfAlternative::Elif { consequence: elif_consequence, alternative: elif_alternative, .. } => {
                              assert_eq!(elif_consequence.statements.len(), 1);
                               assert!(elif_alternative.is_some());
                               match elif_alternative.as_ref().unwrap().as_ref() { // Double deref for Box<Option<Box<..>>> pattern? Check this. Box needed? Let's assume Box was used.
                                    IfAlternative::Else { consequence: else_consequence } => {
                                        assert_eq!(else_consequence.statements.len(), 1);
                                    }
                                     _ => panic!("Expected Else alternative after Elif"),
                               }

                          }
                          _ => panic!("Expected Elif alternative"),
                     }
                 }
                 _ => panic!("Expected IfStatement"),
            }
       }

        #[test]
        fn test_while_statement() {
            let program = parse_ok("while (i < 5) { i = i + 1; }");
             assert_eq!(program.statements.len(), 1);
            match &program.statements[0] {
                Statement::WhileStatement { condition, body } => {
                     assert_eq!(body.statements.len(), 1);
                 }
                 _ => panic!("Expected WhileStatement"),
             }
        }

         #[test]
        fn test_for_statement() {
            let program = parse_ok("for item in items { print(item); }");
             assert_eq!(program.statements.len(), 1);
             match &program.statements[0] {
                 Statement::ForStatement { variable, iterable, body } => {
                      assert_eq!(variable.name, "item");
                      // Cannot easily assert iterable structure without better expr parsing
                       assert_eq!(body.statements.len(), 1);
                  }
                  _ => panic!("Expected ForStatement"),
              }
        }

         #[test]
        fn test_struct_declaration_simple_fields() {
            // Methods are not parsed yet
             let program = parse_ok("struct Point { x: int, y: int }");
             assert_eq!(program.statements.len(), 1);
              match &program.statements[0] {
                  Statement::StructDeclaration(StructDefinition { name, fields, methods }) => {
                       assert_eq!(name.name, "Point");
                       assert_eq!(fields.len(), 2);
                       assert_eq!(fields[0].name.name, "x");
                       assert_eq!(fields[0].type_annotation, TypeAnnotation::Simple(Identifier{name: "int".to_string()}));
                       assert_eq!(fields[1].name.name, "y");
                        assert!(methods.is_empty()); // Methods not implemented
                   }
                   _ => panic!("Expected StructDeclaration"),
               }
        }

         #[test]
        fn test_import_module() {
             let program = parse_ok("import \"./math\";");
             assert_eq!(program.statements.len(), 1);
              match &program.statements[0] {
                 Statement::ImportStatement(ImportDeclaration::ImportModule { path, alias }) => {
                       assert_eq!(path, "./math");
                       assert!(alias.is_none());
                   }
                   _ => panic!("Expected ImportModule"),
               }
        }
        #[test]
        fn test_import_module_with_alias() {
             let program = parse_ok("import \"./long_name\" as short;");
             assert_eq!(program.statements.len(), 1);
              match &program.statements[0] {
                  Statement::ImportStatement(ImportDeclaration::ImportModule { path, alias }) => {
                       assert_eq!(path, "./long_name");
                       assert!(alias.is_some());
                       assert_eq!(alias.as_ref().unwrap().name, "short");
                   }
                   _ => panic!("Expected ImportModule with alias"),
               }
        }

         #[test]
        fn test_import_symbols() {
            let program = parse_ok("from \"math\" import add, multiply;");
             assert_eq!(program.statements.len(), 1);
              match &program.statements[0] {
                  Statement::ImportStatement(ImportDeclaration::ImportSymbols { path, symbols }) => {
                      assert_eq!(path, "math");
                      assert_eq!(symbols.len(), 2);
                       assert_eq!(symbols[0].0.name, "add"); // name
                      assert!(symbols[0].1.is_none()); // alias
                      assert_eq!(symbols[1].0.name, "multiply");
                       assert!(symbols[1].1.is_none());
                  }
                   _ => panic!("Expected ImportSymbols"),
              }
        }

         #[test]
        fn test_import_symbols_with_alias() {
            let program = parse_ok("from \"math\" import add as sum, sub;");
             assert_eq!(program.statements.len(), 1);
             match &program.statements[0] {
                 Statement::ImportStatement(ImportDeclaration::ImportSymbols { path, symbols }) => {
                     assert_eq!(path, "math");
                     assert_eq!(symbols.len(), 2);
                     assert_eq!(symbols[0].0.name, "add");
                     assert!(symbols[0].1.is_some());
                     assert_eq!(symbols[0].1.as_ref().unwrap().name, "sum");
                     assert_eq!(symbols[1].0.name, "sub");
                     assert!(symbols[1].1.is_none());
                 }
                 _ => panic!("Expected ImportSymbols with alias"),
             }
        }

        #[test]
       fn test_type_annotation_generic() {
            let program = parse_ok("let items: List<int>;");
            assert_eq!(program.statements.len(), 1);
             match &program.statements[0] {
                 Statement::LetDeclaration { type_annotation, .. } => {
                     assert!(type_annotation.is_some());
                      match type_annotation.as_ref().unwrap() {
                          TypeAnnotation::Generic { base, arguments } => {
                              assert_eq!(base.name, "List");
                              assert_eq!(arguments.len(), 1);
                              assert_eq!(arguments[0], TypeAnnotation::Simple(Identifier{name: "int".to_string()}));
                          }
                          _ => panic!("Expected Generic TypeAnnotation"),
                      }
                 }
                 _ => panic!("Expected LetDeclaration"),
             }
       }

        #[test]
       fn test_consecutive_let_statements() {
           let program = parse_ok("let a = 1; let b = 2;");
           assert_eq!(program.statements.len(), 2);
           match &program.statements[0] {
               Statement::LetDeclaration { name, initializer, .. } => {
                   assert_eq!(name.name, "a");
                   assert!(initializer.is_some());
                    assert_eq!(initializer.as_ref().unwrap(), &Expression::Literal(Literal::Int("1".to_string())));
               }
               _ => panic!("Expected LetDeclaration for 'a'"),
           }
           match &program.statements[1] {
               Statement::LetDeclaration { name, initializer, .. } => {
                   assert_eq!(name.name, "b");
                   assert!(initializer.is_some());
                    assert_eq!(initializer.as_ref().unwrap(), &Expression::Literal(Literal::Int("2".to_string())));
               }
               _ => panic!("Expected LetDeclaration for 'b'"),
           }
       }

} 