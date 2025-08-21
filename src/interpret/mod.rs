// Re-export key components for easier access
pub mod error;
pub mod value;
pub mod environment;
pub mod evaluator;
 
pub use error::RuntimeError;
pub use value::{Value, Function};
pub use environment::Environment;
pub use evaluator::Interpreter; 
pub use evaluator::BuiltinsCache;