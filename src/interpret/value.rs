use crate::lexer::astgen::{BlockStatement, Identifier, TypeAnnotation};
use crate::interpret::environment::Environment;
use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;

// --- Runtime Value Enum ---

#[derive(Debug, Clone, PartialEq)] // Should maybe not derive PartialEq for floats later
pub enum Value {
    Int(i64),
    Float(f64), // Use f64 for floats
    String(String),
    Boolean(bool),
    Null,
    // Add Function value type
    Function(Box<Function>),
    // Add List/Array value type
    List(Vec<Value>),
    // TODO: Add StructInstance, Dict etc. later
}

// Represents a function (user-defined or built-in)
#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<(Identifier, Option<TypeAnnotation>)>, // Parameter names and optional types
    pub body: Option<BlockStatement>, // None for built-in functions
    pub env: Rc<RefCell<Environment>>, // Capture the defining environment (closure)
    pub is_builtin: bool,
}

// We need a custom PartialEq impl for Function to avoid Environment comparison issues
impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        // Functions are equal if they have the same name and parameter count
        // This is a simplification - in a real language, we'd need more sophisticated equality
        self.name == other.name &&
        self.parameters.len() == other.parameters.len() &&
        self.is_builtin == other.is_builtin
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(i) => write!(f, "{}", i),
            Value::Float(fl) => write!(f, "{}", fl),
            Value::String(s) => write!(f, "{}", s),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Null => write!(f, "null"),
            Value::Function(func) => write!(f, "<function {}>", func.name),
            Value::List(items) => {
                write!(f, "[")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", item)?;
                }
                write!(f, "]")
            },
        }
    }
} 