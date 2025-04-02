use crate::lexer::astgen::{
    BinaryOperator, BlockStatement, Expression, Identifier, Literal, Program, Statement, UnaryOperator,
    TypeAnnotation, /* other AST nodes */
};
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;
use crate::lexer::token::Operator;

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
    // TODO: Add StructInstance, etc. later
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
        }
    }
}

// --- Runtime Error Enum ---

#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeError {
    UndefinedVariable(String),
    TypeError(String), // E.g., "Cannot add string and integer"
    InvalidOperation(String),
    Return(Value), // Special case for return statements
}

impl fmt::Display for RuntimeError {
     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
         match self {
             RuntimeError::UndefinedVariable(name) => write!(f, "Undefined variable: {}", name),
             RuntimeError::TypeError(msg) => write!(f, "Type error: {}", msg),
             RuntimeError::InvalidOperation(msg) => write!(f, "Invalid operation: {}", msg),
             RuntimeError::Return(value) => write!(f, "Return value: {}", value),
         }
     }
}


// --- Environment ---

#[derive(Debug, Clone)]
struct Environment {
    store: HashMap<String, Value>,
    outer: Option<Rc<RefCell<Environment>>>, // For nested scopes
}

impl Environment {
    fn new() -> Self {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    fn new_with_outer(outer: Rc<RefCell<Environment>>) -> Self {
        Environment {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }

    fn define(&mut self, name: String, value: Value) {
        self.store.insert(name, value);
    }

    fn get(&self, name: &str) -> Option<Value> {
        match self.store.get(name) {
            Some(value) => Some(value.clone()),
            None => match &self.outer {
                Some(outer) => outer.borrow().get(name),
                None => None,
            },
        }
    }
    
    // Assign to an existing variable, searching outer scopes
    fn assign(&mut self, name: &str, value: Value) -> Result<(), RuntimeError> {
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


// --- Interpreter ---

pub struct Interpreter {
    environment: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            environment: Rc::new(RefCell::new(Environment::new())),
        }
    }

    pub fn interpret(&mut self, program: Program) -> Result<(), Vec<RuntimeError>> {
        let mut errors = Vec::new();
        for statement in program.statements {
            match self.eval_statement(&statement) {
                Ok(_) => {} // Continue execution
                Err(RuntimeError::Return(value)) => {
                    // Top-level return is allowed but will end execution
                    println!("Program returned value: {}", value);
                    return Ok(());
                }
                Err(e) => {
                    errors.push(e);
                    // Basic error handling: stop execution on first error
                    return Err(errors);
                }
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

     // Statements typically don't produce a value, but might alter state or cause control flow changes
    fn eval_statement(&mut self, statement: &Statement) -> Result<(), RuntimeError> {
         match statement {
             Statement::ExpressionStatement(expr) => {
                 // Handle assignment expressions specially
                 match expr {
                     // For now, we'll only check for variables being assigned directly
                     Expression::Identifier(ident) => {
                         // Check if the next token is an assignment
                         let name = &ident.name;
                         if let Some(value) = self.environment.borrow().get(name) {
                             // Variable exists, evaluate it and continue
                             return Ok(());
                         } else {
                             // Evaluate the expression but it will likely cause an undefined variable error
                             self.eval_expression(expr)?;
                             return Ok(());
                         }
                     },
                     // Check for function calls, which are evaluated for side effects
                     Expression::FunctionCall { callee, arguments } => {
                         self.eval_expression(expr)?;
                         return Ok(());
                     },
                     _ => {
                         // For all other expressions, evaluate normally
                         self.eval_expression(expr)?;
                         return Ok(());
                     }
                 }
             },
             Statement::LetDeclaration { name, type_annotation: _, initializer } => {
                 let value = match initializer {
                     Some(init_expr) => self.eval_expression(init_expr)?,
                     None => Value::Null, // Default to null if no initializer
                 };
                 // TODO: Check type annotation if present
                  self.environment.borrow_mut().define(name.name.clone(), value);
                 Ok(())
             },
             Statement::ReturnStatement { value } => {
                 // Evaluate the return value and immediately signal return by using the Return error variant
                 match value {
                     Some(expr) => {
                         let return_value = self.eval_expression(expr)?;
                         Err(RuntimeError::Return(return_value))
                     },
                     None => Err(RuntimeError::Return(Value::Null)),
                 }
             },
             Statement::FunctionDeclaration { name, parameters, return_type: _, body } => {
                 // Create a function object and store it in the environment
                 let func = Function {
                     name: name.name.clone(),
                     parameters: parameters.clone(),
                     body: Some(body.clone()),
                     env: Rc::clone(&self.environment), // Capture current environment (closure) by cloning the Rc
                     is_builtin: false,
                 };
                 
                 // Store the function in the current environment
                 self.environment.borrow_mut().define(
                     name.name.clone(), 
                     Value::Function(Box::new(func))
                 );
                 
                 // For the main function, also execute it
                 if name.name == "main" {
                     eprintln!("Executing 'main' function");
                     self.eval_block_statement(body)?;
                 }
                 
                 Ok(())
             },
             Statement::IfStatement { condition, consequence, alternative } => {
                 // Basic non-functional stub - evaluate condition but ignore branches
                 let condition_value = self.eval_expression(condition)?;
                 if self.is_truthy(condition_value) {
                     self.eval_block_statement(consequence)?;
                 } else {
                    // Evaluate the alternative if condition is false
                    if let Some(alt) = alternative {
                        self.eval_if_alternative(alt)?;
                    }
                    // If condition is false and no alternative, do nothing
                 }
                 Ok(())
             },
             Statement::WhileStatement { condition, body } => {
                 eprintln!("Warning: While statement encountered but looping logic not implemented.");
                 self.eval_expression(condition)?; // Evaluate condition once
                // self.eval_block_statement(body)?; // Don't evaluate body
                Ok(())
             },
             Statement::ForStatement { variable, iterable, body } => {
                 eprintln!("Warning: For statement encountered but looping/iteration logic not implemented.");
                 self.eval_expression(iterable)?; // Evaluate iterable
                // self.eval_block_statement(body)?; // Don't evaluate body
                Ok(())
             },
             Statement::StructDeclaration { .. } => {
                 eprintln!("Warning: Struct declaration encountered but not interpreted.");
                Ok(())
             },
             Statement::ImportStatement { .. } => {
                 eprintln!("Warning: Import statement encountered but not interpreted.");
                Ok(())
             },
         }
    }

     fn eval_block_statement(&mut self, block: &BlockStatement) -> Result<(), RuntimeError> {
         for statement in &block.statements {
             match self.eval_statement(statement) {
                 Ok(_) => continue,
                 Err(err @ RuntimeError::Return(_)) => return Err(err),
                 Err(other_err) => return Err(other_err),
             }
         }
         Ok(())
     }


    // Expressions evaluate to a runtime value
    fn eval_expression(&mut self, expression: &Expression) -> Result<Value, RuntimeError> {
        match expression {
            Expression::Literal(literal) => self.eval_literal(literal),
             Expression::Identifier(ident) => self.eval_identifier(ident),

             Expression::BinaryOperation { left, op, right } => {
                 let left_val = self.eval_expression(left)?;
                 let right_val = self.eval_expression(right)?;
                 self.eval_binary_operation(op, left_val, right_val)
             }
              Expression::UnaryOperation { op, operand } => {
                  let operand_val = self.eval_expression(operand)?;
                  self.eval_unary_operation(op, operand_val)
              }

             // --- Expressions not yet implemented ---
              Expression::FunctionCall { callee, arguments } => {
                  // First, check if this is a special case where we're calling a function
                  // that's actually performing assignment (e.g., x = value)
                  if let Expression::Identifier(id) = callee.as_ref() {
                      if id.name == "assign" && arguments.len() == 2 {
                          // This is a special internal function for assignment: assign(target, value)
                          if let Expression::Identifier(target) = &arguments[0] {
                              let value = self.eval_expression(&arguments[1])?;
                              // Try to assign to variable
                              self.environment.borrow_mut().assign(&target.name, value.clone())?;
                              return Ok(value);
                          }
                      }
                  }
                  
                  // Otherwise, handle as a normal function call
                  // Evaluate the callee expression first (might be an identifier or member access)
                  match callee.as_ref() {
                      Expression::Identifier(id) if id.name == "print" => {
                          // Handle built-in print function
                          let mut output = String::new();
                          for (i, arg_expr) in arguments.iter().enumerate() {
                              if i > 0 { output.push_str(" "); }
                              let arg_val = self.eval_expression(arg_expr)?;
                              output.push_str(&arg_val.to_string());
                          }
                          println!("{}", output);
                          Ok(Value::Null)
                      }
                       Expression::MemberAccess { base, member } if member.name == "toString" => {
                           // Handle .toString() method call
                           if !arguments.is_empty() {
                               return Err(RuntimeError::TypeError(".toString() expects no arguments".to_string()));
                           }
                           let base_val = self.eval_expression(base)?;
                           // Perform the conversion based on the type of base_val
                            match base_val {
                                Value::Int(i) => Ok(Value::String(i.to_string())),
                                Value::Float(f) => Ok(Value::String(f.to_string())),
                                Value::Boolean(b) => Ok(Value::String(b.to_string())),
                                Value::String(s) => Ok(Value::String(s)), // toString on string is idempotent
                                Value::Null => Ok(Value::String("null".to_string())),
                                Value::Function(f) => Ok(Value::String(format!("<function {}>", f.name))),
                            }
                       }
                      // Other callees (identifiers, complex expressions)
                      _ => {
                            // Evaluate the callee to get the function
                            let callee_val = self.eval_expression(callee)?;
                            
                            match callee_val {
                                Value::Function(func) => {
                                    // Evaluate all arguments
                                    let mut arg_values = Vec::new();
                                    for arg_expr in arguments {
                                        arg_values.push(self.eval_expression(arg_expr)?);
                                    }
                                    
                                    // Check argument count
                                    if arg_values.len() != func.parameters.len() {
                                        return Err(RuntimeError::TypeError(
                                            format!("Function '{}' expected {} arguments but got {}", 
                                                func.name, func.parameters.len(), arg_values.len())
                                        ));
                                    }
                                    
                                    // Handle built-in functions if needed
                                    if func.is_builtin {
                                        return Err(RuntimeError::InvalidOperation("Built-in functions not implemented yet".to_string()));
                                    }
                                    
                                    // Create a new environment for the function call, using the function's closure env as outer
                                    let func_env = Rc::new(RefCell::new(Environment::new_with_outer(
                                        Rc::clone(&func.env) // Use the cloned Rc from the function's definition
                                    )));
                                    
                                    // Bind parameters to arguments in the new environment
                                    for (i, (param, _)) in func.parameters.iter().enumerate() {
                                        func_env.borrow_mut().define(param.name.clone(), arg_values[i].clone());
                                    }
                                    
                                    // Execute the function with this environment
                                    if let Some(body) = &func.body {
                                        // Save the current environment
                                        let prev_env = Rc::clone(&self.environment);
                                        
                                        // Set the interpreter's environment to the function's environment
                                        self.environment = func_env;
                                        
                                        // Execute the function body
                                        let result = self.execute_function_body(body);
                                        
                                        // Restore the previous environment
                                        self.environment = prev_env;
                                        
                                        return result;
                                    } else {
                                        return Err(RuntimeError::InvalidOperation("Function has no body".to_string()));
                                    }
                                }
                                _ => Err(RuntimeError::TypeError(format!("Cannot call non-function value: {}", callee_val))),
                            }
                      }
                  }
              }
              Expression::ListInitializer { items } => {
                   // Evaluate items but don't create a list value yet
                  for item_expr in items { self.eval_expression(item_expr)?; }
                  eprintln!("Warning: List initializer evaluated but list value not created.");
                  Ok(Value::Null) // Placeholder
                   // Err(RuntimeError::Other("List initializers not implemented".to_string()))
              }
               Expression::DictInitializer { pairs } => {
                   // Evaluate items but don't create a dict value yet
                  for (k, v) in pairs {
                      self.eval_expression(k)?;
                      self.eval_expression(v)?;
                  }
                   eprintln!("Warning: Dict initializer evaluated but dict value not created.");
                   Ok(Value::Null) // Placeholder
                  // Err(RuntimeError::Other("Dict initializers not implemented".to_string()))
              }
               Expression::IndexAccess { base, index } => {
                    self.eval_expression(base)?;
                   self.eval_expression(index)?;
                    Err(RuntimeError::InvalidOperation("Index access not implemented".to_string()))
               }
                Expression::MemberAccess { base, member } => {
                    let base_val = self.eval_expression(base)?;
                     // Rudimentary .toString() for numbers
                     if member.name == "toString" {
                         match base_val {
                             Value::Int(i) => Ok(Value::String(i.to_string())),
                             Value::Float(f) => Ok(Value::String(f.to_string())),
                              // Add others? Boolean? String itself?
                              Value::Boolean(b) => Ok(Value::String(b.to_string())),
                              Value::String(s) => Ok(Value::String(s)), // toString on string is idempotent
                              Value::Null => Ok(Value::String("null".to_string())),
                              Value::Function(f) => Ok(Value::String(format!("<function {}>", f.name))),
                         }
                     } else {
                        Err(RuntimeError::InvalidOperation(format!("Member access for '.{}' not implemented", member.name)))
                     }
                }
        }
    }

    fn eval_literal(&self, literal: &Literal) -> Result<Value, RuntimeError> {
        match literal {
            Literal::Int(s) => s.parse::<i64>()
                .map(Value::Int)
                .map_err(|e| RuntimeError::InvalidOperation(format!("Invalid integer literal '{}': {}", s, e))),
            Literal::Float(s) => s.parse::<f64>()
                .map(Value::Float)
                .map_err(|e| RuntimeError::InvalidOperation(format!("Invalid float literal '{}': {}", s, e))),
            Literal::String(s) => Ok(Value::String(s.clone())),
            Literal::Boolean(b) => Ok(Value::Boolean(*b)),
            Literal::Null => Ok(Value::Null),
        }
    }

     fn eval_identifier(&mut self, identifier: &Identifier) -> Result<Value, RuntimeError> {
         self.environment.borrow().get(&identifier.name)
             .ok_or_else(|| RuntimeError::UndefinedVariable(identifier.name.clone()))
     }

     // Basic binary operations
     fn eval_binary_operation(&self, op: &BinaryOperator, left: Value, right: Value) -> Result<Value, RuntimeError> {
        //  println!("DEBUG: eval_binary_operation: {:?} {:?} {:?}", left, op, right);
          match op {
             // --- Arithmetic ---
             BinaryOperator::Add => match (left, right) {
                 (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l + r)),
                  (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l + r)),
                  (Value::Int(l), Value::Float(r)) => Ok(Value::Float(l as f64 + r)),
                  (Value::Float(l), Value::Int(r)) => Ok(Value::Float(l + r as f64)),
                  (Value::String(l), Value::String(r)) => Ok(Value::String(l + &r)), // String concat
                 (Value::Function(_), _) | (_, Value::Function(_)) => 
                    Err(RuntimeError::TypeError("Cannot add function values".to_string())),
                 (l, r) => Err(RuntimeError::TypeError(format!("Cannot add {} and {}", l, r))),
             },
              BinaryOperator::Subtract => match (left, right) {
                  (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l - r)),
                  (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l - r)),
                  (Value::Int(l), Value::Float(r)) => Ok(Value::Float(l as f64 - r)),
                  (Value::Float(l), Value::Int(r)) => Ok(Value::Float(l - r as f64)),
                  (Value::Function(_), _) | (_, Value::Function(_)) => 
                    Err(RuntimeError::TypeError("Cannot subtract function values".to_string())),
                  (l, r) => Err(RuntimeError::TypeError(format!("Cannot subtract {} from {}", r, l))),
              },
              BinaryOperator::Multiply => match (left, right) {
                  (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l * r)),
                  (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l * r)),
                  (Value::Int(l), Value::Float(r)) => Ok(Value::Float(l as f64 * r)),
                  (Value::Float(l), Value::Int(r)) => Ok(Value::Float(l * r as f64)),
                  (Value::Function(_), _) | (_, Value::Function(_)) => 
                    Err(RuntimeError::TypeError("Cannot multiply function values".to_string())),
                  (l, r) => Err(RuntimeError::TypeError(format!("Cannot multiply {} and {}", l, r))),
              },
              BinaryOperator::Divide => match (left, right) {
                   // Integer division results in float for simplicity here
                  (Value::Int(l), Value::Int(r)) => if r == 0 { Err(RuntimeError::InvalidOperation("Division by zero".to_string())) } else { Ok(Value::Float(l as f64 / r as f64)) },
                  (Value::Float(l), Value::Float(r)) => if r == 0.0 { Err(RuntimeError::InvalidOperation("Division by zero".to_string())) } else { Ok(Value::Float(l / r)) },
                  (Value::Int(l), Value::Float(r)) => if r == 0.0 { Err(RuntimeError::InvalidOperation("Division by zero".to_string())) } else { Ok(Value::Float(l as f64 / r)) },
                  (Value::Float(l), Value::Int(r)) => if r == 0 { Err(RuntimeError::InvalidOperation("Division by zero".to_string())) } else { Ok(Value::Float(l / r as f64)) },
                  (Value::Function(_), _) | (_, Value::Function(_)) => 
                    Err(RuntimeError::TypeError("Cannot divide function values".to_string())),
                  (l, r) => Err(RuntimeError::TypeError(format!("Cannot divide {} by {}", l, r))),
              },
               BinaryOperator::Modulo => match (left, right) {
                  (Value::Int(l), Value::Int(r)) => if r == 0 { Err(RuntimeError::InvalidOperation("Modulo by zero".to_string())) } else { Ok(Value::Int(l % r)) },
                  // Modulo on floats? Maybe disallow.
                   (Value::Function(_), _) | (_, Value::Function(_)) => 
                    Err(RuntimeError::TypeError("Cannot calculate modulo for function values".to_string())),
                   (l, r) => Err(RuntimeError::TypeError(format!("Cannot calculate modulo for {} % {}", l, r))),
              },

             // --- Comparison ---
              // Note: Comparisons between Int/Float promote Int to Float
              BinaryOperator::Equal => Ok(Value::Boolean(self.values_equal(left, right))),
              BinaryOperator::NotEqual => Ok(Value::Boolean(!self.values_equal(left, right))),
              BinaryOperator::LessThan => {
                   let result = self.compare_values(&left, &right)?;
                   Ok(Value::Boolean(result == Some(std::cmp::Ordering::Less)))
              }
               BinaryOperator::GreaterThan => {
                   let result = self.compare_values(&left, &right)?;
                   Ok(Value::Boolean(result == Some(std::cmp::Ordering::Greater)))
               }
               BinaryOperator::LessEqual => {
                   let result = self.compare_values(&left, &right)?;
                    Ok(Value::Boolean(result == Some(std::cmp::Ordering::Less) || result == Some(std::cmp::Ordering::Equal)))
               }
               BinaryOperator::GreaterEqual => {
                   let result = self.compare_values(&left, &right)?;
                    Ok(Value::Boolean(result == Some(std::cmp::Ordering::Greater) || result == Some(std::cmp::Ordering::Equal)))
               }

             // --- Logical ---
              BinaryOperator::And => Ok(Value::Boolean(self.is_truthy(left) && self.is_truthy(right))), // Short-circuiting not implemented here
              BinaryOperator::Or => Ok(Value::Boolean(self.is_truthy(left) || self.is_truthy(right))),   // Short-circuiting not implemented here

         }
     }

      // Basic unary operations
      fn eval_unary_operation(&self, op: &UnaryOperator, operand: Value) -> Result<Value, RuntimeError> {
          match op {
              UnaryOperator::Not => Ok(Value::Boolean(!self.is_truthy(operand))),
              UnaryOperator::Negate => match operand {
                  Value::Int(i) => Ok(Value::Int(-i)),
                  Value::Float(f) => Ok(Value::Float(-f)),
                  Value::Function(_) => Err(RuntimeError::TypeError("Cannot negate a function".to_string())),
                  v => Err(RuntimeError::TypeError(format!("Cannot negate non-numeric value: {}", v))),
              },
          }
      }


      // --- Helper Methods for Evaluation ---

      // Determines truthiness according to typical dynamic language rules
      fn is_truthy(&self, value: Value) -> bool {
          match value {
              Value::Null => false,
              Value::Boolean(b) => b,
              Value::Int(i) => i != 0,
              Value::Float(f) => f != 0.0 && !f.is_nan(), // NaN is falsy
              Value::String(s) => !s.is_empty(),
              Value::Function(_) => true, // Functions are always truthy
              // Other types (functions, objects) are generally truthy
          }
      }

      // Compares values, handling Int/Float promotion. Returns None if types are incomparable.
       fn compare_values(&self, left: &Value, right: &Value) -> Result<Option<std::cmp::Ordering>, RuntimeError> {
            match (left, right) {
                 (Value::Int(l), Value::Int(r)) => Ok(l.partial_cmp(r)),
                 (Value::Float(l), Value::Float(r)) => Ok(l.partial_cmp(r)),
                 (Value::Int(l), Value::Float(r)) => Ok((*l as f64).partial_cmp(r)),
                 (Value::Float(l), Value::Int(r)) => Ok(l.partial_cmp(&(*r as f64))),
                 // Maybe compare strings?
                 (Value::String(l), Value::String(r)) => Ok(l.partial_cmp(r)),
                 // Function comparison not supported
                 (Value::Function(_), _) | (_, Value::Function(_)) => 
                    Err(RuntimeError::TypeError("Cannot compare function values".to_string())),
                 // Other comparisons are type errors
                 (l, r) => Err(RuntimeError::TypeError(format!("Cannot compare {} and {}", l, r))),
            }
       }

       // Checks equality, handling Int/Float promotion. Different types are never equal (except Int/Float).
       fn values_equal(&self, left: Value, right: Value) -> bool {
            match (left, right) {
                 (Value::Null, Value::Null) => true,
                 (Value::Boolean(l), Value::Boolean(r)) => l == r,
                 (Value::Int(l), Value::Int(r)) => l == r,
                 (Value::Float(l), Value::Float(r)) => l == r, // Note potential float precision issues
                  (Value::Int(l), Value::Float(r)) => (l as f64) == r,
                  (Value::Float(l), Value::Int(r)) => l == (r as f64),
                 (Value::String(l), Value::String(r)) => l == r,
                 (Value::Function(l), Value::Function(r)) => l == r, // Use our PartialEq impl
                 // Different types are not equal
                  _ => false,
            }
       }

    // Helper to evaluate the alternative branch (elif/else) of an if statement
    fn eval_if_alternative(&mut self, alternative: &crate::lexer::astgen::IfAlternative) -> Result<(), RuntimeError> {
         match alternative {
             crate::lexer::astgen::IfAlternative::Elif { condition, consequence, alternative: next_alternative } => {
                 let condition_value = self.eval_expression(condition)?;
                 if self.is_truthy(condition_value) {
                     self.eval_block_statement(consequence)?;
                 } else if let Some(next_alt) = next_alternative {
                     // If elif condition is false, evaluate the *next* alternative
                     self.eval_if_alternative(next_alt)?;
                 }
                 // If elif condition is false and no further alternative, do nothing
                 Ok(())
             }
             crate::lexer::astgen::IfAlternative::Else { consequence } => {
                 // If it's an Else block, just evaluate its consequence
                 self.eval_block_statement(consequence)?;
                 Ok(())
             }
         }
    }

    fn get_operator_result(&self, operator: &Operator, value: Value) -> Result<Value, RuntimeError> {
        match operator {
            Operator::Minus => match value {
                Value::Int(i) => Ok(Value::Int(-i)),
                Value::Float(f) => Ok(Value::Float(-f)),
                Value::Function(_) => Err(RuntimeError::TypeError("Cannot negate a function".to_string())),
                v => Err(RuntimeError::TypeError(format!("Cannot negate {}", v))),
            },
            Operator::Not => match value {
                Value::Boolean(b) => Ok(Value::Boolean(!b)),
                Value::Function(_) => Err(RuntimeError::TypeError("Cannot apply logical NOT to a function".to_string())),
                v => Err(RuntimeError::TypeError(format!("Cannot apply logical NOT to {}", v))),
            },
            _ => Err(RuntimeError::TypeError(format!("Unsupported prefix operator: {:?}", operator))),
        }
    }

    fn print_value(&self, value: &Value) {
        match value {
            Value::Null => print!("null"),
            Value::Boolean(b) => print!("{}", b),
            Value::Int(i) => print!("{}", i),
            Value::Float(f) => print!("{}", f),
            Value::String(s) => print!("{}", s),
            Value::Function(f) => print!("<function {}>", f.name),
        }
    }

    // Helper function to execute function body and handle return values
    fn execute_function_body(&mut self, body: &BlockStatement) -> Result<Value, RuntimeError> {
        match self.eval_block_statement(body) {
            Ok(_) => Ok(Value::Null), // No return statement found
            Err(RuntimeError::Return(value)) => Ok(value), // Return value from function
            Err(err) => Err(err),
        }
    }

}
