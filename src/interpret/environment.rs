use crate::interpret::value::Value;
use crate::interpret::error::RuntimeError;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

// --- Environment ---

#[derive(Debug, Clone)] // Consider removing outer from Debug if it causes deep prints
pub struct Environment {
    store: HashMap<String, Value>,
    outer: Option<Rc<RefCell<Environment>>>, // For nested scopes
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_with_outer(outer: Rc<RefCell<Environment>>) -> Self {
        Environment {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }

    pub fn new_with_outer_capacity(outer: Rc<RefCell<Environment>>, capacity: usize) -> Self {
        Environment {
            store: HashMap::with_capacity(capacity),
            outer: Some(outer),
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.store.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        match self.store.get(name) {
            Some(value) => Some(value.clone()),
            None => match &self.outer {
                Some(outer) => outer.borrow().get(name),
                None => None,
            },
        }
    }

    pub fn has_local(&self, name: &str) -> bool {
        self.store.contains_key(name)
    }

    pub fn get_local(&self, name: &str) -> Option<Value> {
        self.store.get(name).cloned()
    }

    // Assign to an existing variable, searching outer scopes
    pub fn assign(&mut self, name: &str, value: Value) -> Result<(), RuntimeError> {
        if self.store.contains_key(name) {
            self.store.insert(name.to_string(), value);
            Ok(())
        } else {
            match &self.outer {
                Some(outer) => outer.borrow_mut().assign(name, value),
                None => Err(RuntimeError::UndefinedVariable(name.to_string())),
            }
        }
    }

    // Added for debugging
    pub fn debug_print(&self, indent: &str) {
        println!("{}Environment {{level}}", indent);
        for (name, value) in &self.store {
            println!("{}{} -> {}", indent, name, value); // Use Value's Display
        }
        if let Some(outer_env) = &self.outer {
            println!("{}Outer ->", indent);
            outer_env.borrow().debug_print(&(indent.to_string() + "  "));
        } else {
            println!("{}Outer -> None", indent);
        }
    }

    // --- In-place mutation helpers for performance hot paths ---

    pub fn append_to_list(&mut self, name: &str, element: Value) -> Result<(), RuntimeError> {
        if self.store.contains_key(name) {
            match self.store.get_mut(name) {
                Some(Value::List(list)) => {
                    list.push(element);
                    Ok(())
                }
                Some(other) => Err(RuntimeError::TypeError(format!(
                    "Cannot append to non-list variable '{}', found {}",
                    name, other
                ))),
                None => unreachable!(),
            }
        } else {
            match &mut self.outer {
                Some(outer) => outer.borrow_mut().append_to_list(name, element),
                None => Err(RuntimeError::UndefinedVariable(name.to_string())),
            }
        }
    }

    pub fn add_in_place(&mut self, name: &str, rhs: Value) -> Result<(), RuntimeError> {
        if let Some(val) = self.store.get_mut(name) {
            match val {
                Value::Int(l) => match rhs {
                    Value::Int(r) => { *l += r; Ok(()) }
                    Value::Float(r) => { let new_val = (*l as f64) + r; *val = Value::Float(new_val); Ok(()) }
                    other => Err(RuntimeError::TypeError(format!(
                        "Cannot add {} to int variable '{}'",
                        other.type_name(), name
                    )))
                },
                Value::Float(l) => match rhs {
                    Value::Int(r) => { *l += r as f64; Ok(()) }
                    Value::Float(r) => { *l += r; Ok(()) }
                    other => Err(RuntimeError::TypeError(format!(
                        "Cannot add {} to float variable '{}'",
                        other.type_name(), name
                    )))
                },
                other => Err(RuntimeError::TypeError(format!(
                    "In-place addition only supported for numeric variables, found {} in '{}'",
                    other.type_name(), name
                )))
            }
        } else {
            match &mut self.outer {
                Some(outer) => outer.borrow_mut().add_in_place(name, rhs),
                None => Err(RuntimeError::UndefinedVariable(name.to_string())),
            }
        }
    }
} 