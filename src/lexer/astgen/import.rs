use super::Identifier; // Defined in mod.rs

#[derive(Debug, PartialEq, Clone)]
pub enum ImportDeclaration {
    ImportModule {
        path: String, // The module path (e.g., "math", "./utils")
        alias: Option<Identifier>, // Optional alias (e.g., `as lmn`)
    },
    ImportSymbols {
        path: String,
        symbols: Vec<(Identifier, Option<Identifier>)>, // Original name and optional alias
    },
}
