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
} 