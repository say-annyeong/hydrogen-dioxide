//! Defines the Abstract Syntax Tree (AST) nodes for the Oxygen language.

pub mod expression;
pub mod import;
pub mod literal;
pub mod statement;
pub mod structdef; // Added structdef module

pub use expression::Expression;
pub use import::{ImportDeclaration, ImportSource};
pub use literal::Literal;
pub use statement::{BlockStatement, IfAlternative, Statement};
pub use structdef::{FieldDefinition, MethodDefinition, StructDefinition}; // Added struct exports

// --- Shared Basic Types ---

/// Represents an identifier (e.g., variable name, function name).
#[derive(Debug, PartialEq, Eq, Clone, Hash)] // Added Eq, Hash for potential use in maps
pub struct Identifier {
    pub name: String,
    // Potentially add span/location info here later
}

/// Represents a type annotation (e.g., `: int`, `: string`, `: List<int>`).
#[derive(Debug, PartialEq, Clone)]
pub enum TypeAnnotation {
    Simple(Identifier), // e.g., `int`, `string`, `MyStruct`
    Generic {
        base: Identifier, // e.g., `List`, `Dict`
        arguments: Vec<TypeAnnotation>, // e.g., `[int]`, `[string, int]`
    },
    // Add `void` type specifically? Or handle as a special Simple identifier?
    Void, // Explicit void type for function returns
}

// --- Operator Enums (used in Expression) ---

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)] // Added Eq, Hash, Copy
pub enum BinaryOperator {
    // Arithmetic
    Add, // +
    Subtract, // -
    Multiply, // *
    Divide, // /
    Modulo, // %

    // Comparison
    Equal, // ==
    NotEqual, // !=
    LessThan, // <
    GreaterThan, // >
    LessEqual, // <=
    GreaterEqual, // >=

    // Logical
    And, // &&
    Or, // ||
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)] // Added Eq, Hash, Copy
pub enum UnaryOperator {
    Not,   // !
    Negate, // -
}

/// Represents the top-level structure of an Oxygen program (a list of statements).
#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}
