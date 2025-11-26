use std::fmt::{Display, Formatter, Result};

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Assignment {
    Assign,         // =
    //PlusAssign,     // +=
    //MinusAssign,    // -=
    //MultiplyAssign, // *=
    //DivideAssign,   // /=
    //ModuloAssign,   // %=
    // Add other assignment operators if needed (e.g., bitwise?)
}

impl Display for Assignment {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let assign_str = match self {
            Assignment::Assign => "=",
            /*
            Assignment::PlusAssign => "+=",
            Assignment::MinusAssign => "-=",
            Assignment::MultiplyAssign => "*=",
            Assignment::DivideAssign => "/=",
            Assignment::ModuloAssign => "%=",
            */
        };
        write!(f, "{}", assign_str)
    }
}
