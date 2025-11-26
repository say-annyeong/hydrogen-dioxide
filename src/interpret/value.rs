use crate::lexer::astgen::{BlockStatement, Identifier, TypeAnnotation, FieldDefinition, ImplMethodDefinition};
use crate::interpret::environment::Environment;
use std::fmt;
use std::borrow::Cow;
// Builtin function identifiers for fast dispatch
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinId {
    Print,
    Println,
    Len,
    Type,
    ToString,
    /*
    TcpConnect,
    TcpConnectWithTimeout,
    SocketWrite,
    SocketRead,
    HttpGet,
    TcpBind,
    TcpAccept,
    UdpBind,
    UdpSendTo,
    UdpRecvFrom,
    */
    StringTrimEnd,
}

use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::any::Any;

// --- Runtime Value Enum ---

#[derive(Debug, Clone)] // Removed PartialEq - complex for functions/structs
pub enum Value {
    Int(i64),
    Float(f64),
    String(Cow<'static, str>),
    Boolean(bool),
    Null,
    Function(Box<Function>),
    List(Vec<Value>),
    Dict(HashMap<String, Value>),
    StructDefinition(StructDefinitionValue),
    StructInstance(StructInstanceValue),
    BoundMethod(BoundMethodValue),
    NativeResource(Rc<RefCell<dyn Any>>),
}

// Represents a struct definition captured at runtime
#[derive(Debug, Clone)]
pub struct StructDefinitionValue {
    pub name: Identifier,
    pub fields: Vec<FieldDefinition>,
    pub methods: Rc<RefCell<HashMap<String, ImplMethodDefinition>>>,
}

// Represents an instance of a struct
#[derive(Debug, Clone)]
pub struct StructInstanceValue {
    pub type_name: Identifier, // The name of the struct type
    pub fields: Rc<RefCell<HashMap<String, Value>>>, // Field names map to values
}

impl PartialEq for StructInstanceValue {
    fn eq(&self, other: &Self) -> bool {
        self.type_name == other.type_name && *self.fields.borrow() == *other.fields.borrow()
    }
}

// Represents a method bound to a specific instance
#[derive(Debug, Clone)]
pub struct BoundMethodValue {
    pub instance: StructInstanceValue, // Rc? No, clone the instance for now?
    pub method: ImplMethodDefinition,
}

// Custom PartialEq for Value (needed because we removed derive)
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int(l), Value::Int(r)) => l == r,
            (Value::Float(l), Value::Float(r)) => l == r,
            (Value::String(l), Value::String(r)) => l == r,
            (Value::Boolean(l), Value::Boolean(r)) => l == r,
            (Value::Null, Value::Null) => true,
            (Value::List(l), Value::List(r)) => l == r, // Relies on PartialEq for Vec<Value>
            (Value::Dict(l), Value::Dict(r)) => l == r,
            (Value::Function(l), Value::Function(r)) => l == r, // Uses Function's PartialEq
            (Value::StructInstance(l), Value::StructInstance(r)) => {
                 // Instances are equal if they are the same type and fields are equal
                 l.type_name == r.type_name &&
                 *l.fields.borrow() == *r.fields.borrow()
             }
            (Value::BoundMethod(l), Value::BoundMethod(r)) => std::ptr::eq(&l.method, &r.method) && l.instance == r.instance,
            (Value::StructDefinition(_), Value::StructDefinition(_)) => false,
            (Value::NativeResource(l), Value::NativeResource(r)) => Rc::ptr_eq(l, r),
            _ => false, // Different types are not equal
        }
    }
}

// Represents a function (user-defined or built-in)
#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<(Identifier, Option<TypeAnnotation>)>, // Parameter names and optional types
    pub body: Option<BlockStatement>, // None for built-in functions
    pub env: Rc<RefCell<Environment>>, // Capture the defining environment (closure)
    pub is_builtin: bool,
    pub builtin_id: Option<BuiltinId>,
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
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", item)?;
                }
                write!(f, "]")
            },
            Value::Dict(map) => {
                write!(f, "{{")?;
                let mut first = true;
                for (k, v) in map.iter() {
                    if !first { write!(f, ", ")?; }
                    write!(f, "{}: {}", k, v)?;
                    first = false;
                }
                write!(f, "}}")
            }
            Value::StructDefinition(def) => write!(f, "<struct {}>", def.name.name),
            Value::StructInstance(inst) => {
                // Basic display for struct instance
                 write!(f, "{} {{ ", inst.type_name.name)?;
                 let fields = inst.fields.borrow();
                 let mut first = true;
                 for (name, value) in fields.iter() {
                     if !first { write!(f, ", ")?; }
                     write!(f, "{}: {}", name, value)?;
                     first = false;
                 }
                 write!(f, " }}")
            },
            Value::BoundMethod(bm) => write!(f, "<bound method {}.{}>", bm.instance.type_name.name, bm.method.name.name),
            Value::NativeResource(_) => write!(f, "<native resource>"),
        }
    }
} 