use crate::interpret::value::Value;
use std::fmt;

// --- Runtime Error Enum ---

#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeError {
    UndefinedVariable(String),
    TypeError(String), // E.g., "Cannot add string and integer"
    InvalidOperation(String),
    Return(Value), // Special case for return statements
    Break, // Special case for break statements
}

impl fmt::Display for RuntimeError {
     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
         match self {
             RuntimeError::UndefinedVariable(name) => write!(f, "Undefined variable: {}", name),
             RuntimeError::TypeError(msg) => write!(f, "Type error: {}", msg),
             RuntimeError::InvalidOperation(msg) => write!(f, "Invalid operation: {}", msg),
             RuntimeError::Return(value) => write!(f, "Return value: {}", value), // Should this be displayed?
             RuntimeError::Break => write!(f, "Break statement encountered outside of loop"),
         }
     }
} 