use std::fmt::{Display, Formatter, Result};

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Operator {
    // Arithmetic
    Plus,       // +
    Minus,      // -
    Multiply,   // *
    Divide,     // /
    Modulo,     // %

    // Comparison
    Equal,      // ==
    NotEqual,   // !=
    LessThan,   // <
    GreaterThan,// >
    LessEqual,  // <=
    GreaterEqual,// >=

    // Logical
    And,        // &&
    Or,         // ||
    Not,        // !
}

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let op_str = match self {
            Operator::Plus => "+",
            Operator::Minus => "-",
            Operator::Multiply => "*",
            Operator::Divide => "/",
            Operator::Modulo => "%",
            Operator::Equal => "==",
            Operator::NotEqual => "!=",
            Operator::LessThan => "<",
            Operator::GreaterThan => ">",
            Operator::LessEqual => "<=",
            Operator::GreaterEqual => ">=",
            Operator::And => "&&",
            Operator::Or => "||",
            Operator::Not => "!",
        };
        write!(f, "{}", op_str)
    }
}
