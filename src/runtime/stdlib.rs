use crate::interpret::{Value, RuntimeError};
use std::io::{self, Write};

/// Built-in `print` function.
/// Takes any number of arguments, converts them to strings,
/// joins them with spaces, and prints to stdout followed by a newline.
pub fn builtin_print(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let mut output = String::new();
    for (i, arg) in args.iter().enumerate() {
        if i > 0 {
            output.push(' ');
        }
        // Use the Display implementation of Value
        output.push_str(&arg.to_string());
    }
    println!("{}", output);
    // Ensure stdout is flushed, especially important for testing or pipelines
    io::stdout().flush().map_err(|e| RuntimeError::InvalidOperation(format!("Failed to flush stdout: {}", e)))?;
    Ok(Value::Null) // print returns null
}

/// Built-in `len` function to get the length of strings or lists.
pub fn builtin_len(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::TypeError(format!(
            "len() expects 1 argument, got {}",
            args.len()
        )));
    }

    match &args[0] {
        Value::String(s) => Ok(Value::Int(s.chars().count() as i64)),
        Value::List(list) => Ok(Value::Int(list.len() as i64)),
        _ => Err(RuntimeError::TypeError(format!(
            "len() expects a string or list, got {}",
            args[0]
        ))),
    }
}

/// Built-in `type` function to get the type of a value as a string.
pub fn builtin_type(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::TypeError(format!(
            "type() expects 1 argument, got {}",
            args.len()
        )));
    }

    let type_str = match &args[0] {
        Value::Int(_) => "int",
        Value::Float(_) => "float",
        Value::String(_) => "string",
        Value::Boolean(_) => "boolean",
        Value::Null => "null",
        Value::Function(_) => "function",
        Value::List(_) => "list",
        Value::StructDefinition(def) => def.name.name.as_str(),
        Value::StructInstance(inst) => inst.type_name.name.as_str(),
    };

    Ok(Value::String(type_str.to_string()))
}

/// Register all stdlib functions in an Interpreter environment.
pub fn register_stdlib(env: &mut crate::interpret::environment::Environment) {
    use crate::interpret::value::Function;
    use std::rc::Rc;
    use std::cell::RefCell;

    // Register built-in functions
    let functions = [
        ("print", builtin_print as fn(Vec<Value>) -> Result<Value, RuntimeError>),
        ("len", builtin_len as fn(Vec<Value>) -> Result<Value, RuntimeError>),
        ("type", builtin_type as fn(Vec<Value>) -> Result<Value, RuntimeError>),
    ];

    for (name, _) in functions.iter() {
        env.define(
            name.to_string(),
            Value::Function(Box::new(Function {
                name: name.to_string(),
                parameters: vec![], // Parameters are dynamic for built-ins
                body: None,
                env: Rc::new(RefCell::new(crate::interpret::environment::Environment::new())),
                is_builtin: true,
            }))
        );
    }
}

// TODO: Add other built-in functions here (e.g., len, type, input, file I/O, etc.) 