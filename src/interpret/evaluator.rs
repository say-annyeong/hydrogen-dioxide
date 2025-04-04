use crate::lexer::astgen::{
    BinaryOperator, BlockStatement, Expression, Identifier, Literal, Program, Statement, UnaryOperator,
    IfAlternative, ImportDeclaration, ImportSource, ExportDeclaration // Ensure IfAlternative, ImportDeclaration, and ImportSource are imported if used
};
use crate::runtime; // Import the runtime module (for stdlib)
use crate::interpret::value::{Value, Function};
use crate::interpret::environment::Environment;
use crate::interpret::error::RuntimeError;

use std::rc::Rc;
use std::cell::RefCell;
use std::fs; // Import fs for file reading
use crate::lexer::{Tokenizer, Parser}; // Import Tokenizer and Parser
use std::path::{Path, PathBuf}; // Import Path and PathBuf

// --- Interpreter ---

pub struct Interpreter {
    // The main environment for user code execution
    environment: Rc<RefCell<Environment>>,
    // A separate environment holding standard library definitions
    stdlib_environment: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Self {
        // 1. Create the dedicated stdlib environment
        let stdlib_env = Rc::new(RefCell::new(Environment::new()));

        // 2. Populate stdlib_env with Rust built-ins
        runtime::stdlib::register_stdlib(&mut stdlib_env.borrow_mut());

        // 3. Dynamically load and execute Oxygen stdlib code (.oxy files) into stdlib_env
        let stdlib_root = PathBuf::from("stdlib");
        if let Err(e) = Self::load_oxygen_module(&stdlib_root, Rc::clone(&stdlib_env)) {
             eprintln!("!!! Fatal Error: Failed to load standard library: {} !!!", e);
             // Consider exiting or returning an error from new()
        }

        // 4. Create the main global environment for user code
        let global_env = Rc::new(RefCell::new(Environment::new()));

        // 5. Return the final interpreter instance
        Interpreter {
            environment: global_env,
            stdlib_environment: stdlib_env,
        }
    }

    // --- New Function: Recursively Loads Oxygen Modules ---
    fn load_oxygen_module(dir_path: &Path, target_env: Rc<RefCell<Environment>>) -> Result<(), String> {
        // Determine whether to use lib.oxy or mod.oxy
        let is_stdlib_root = dir_path == Path::new("stdlib");
        let entry_point_path = if is_stdlib_root {
            let lib_path = dir_path.join("lib.oxy");
            if lib_path.exists() { lib_path } else { dir_path.join("mod.oxy") }
        } else {
            dir_path.join("mod.oxy")
        };

        if !entry_point_path.exists() {
            return Ok(()); // Silently skip if entry point doesn't exist
        }

        let entry_code = fs::read_to_string(&entry_point_path)
            .map_err(|e| format!("Failed to read {}: {}", entry_point_path.display(), e))?;

        let tokenizer = Tokenizer::new(&entry_code);
        let mut parser = Parser::new(tokenizer);
        let program = parser.parse_program();

        if !parser.errors().is_empty() {
             let error_details = parser.errors().iter()
                 .map(|e| format!("- {:?}", e))
                 .collect::<Vec<_>>().join("\n");
             return Err(format!("Parse errors in {}:\n{}", entry_point_path.display(), error_details));
        }

        // Evaluate the statements in the entry file (lib.oxy or mod.oxy)
        let mut temp_interpreter = Interpreter {
            environment: Rc::clone(&target_env),
            stdlib_environment: Rc::clone(&target_env),
        };

        for statement in program.statements {
            match statement {
                Statement::ExportStatement(decl) => {
                    match decl {
                        ExportDeclaration::Identifier(ident) => {
                            let name = &ident.name;
                            let potential_dir_path = dir_path.join(name);
                            let mut potential_file_path = dir_path.join(name);
                            potential_file_path.set_extension("oxy");

                            if potential_dir_path.is_dir() {
                                // It's a subdirectory, load its entry point (mod.oxy)
                                Self::load_oxygen_module(&potential_dir_path, Rc::clone(&target_env))?;
                            } else if potential_file_path.exists() {
                                // It's a sibling file, load and execute it.
                                let file_code = fs::read_to_string(&potential_file_path)
                                    .map_err(|e| format!("Failed to read {}: {}", potential_file_path.display(), e))?;

                                let file_tokenizer = Tokenizer::new(&file_code);
                                let mut file_parser = Parser::new(file_tokenizer);
                                let file_program = file_parser.parse_program();

                                if !file_parser.errors().is_empty() {
                                    let error_details = file_parser.errors().iter()
                                        .map(|e| format!("- {:?}", e))
                                        .collect::<Vec<_>>().join("\n");
                                    return Err(format!("Parse errors in {}:\n{}", potential_file_path.display(), error_details));
                                }

                                // Evaluate the file's program using the *same* temporary interpreter
                                match temp_interpreter.interpret_program_in_place(file_program) {
                                    Ok(_) => { /* File loaded successfully */ }
                                    Err(errors) => {
                                        let error_details = errors.iter()
                                            .map(|e| format!("- {}", e))
                                            .collect::<Vec<_>>().join("\n");
                                        return Err(format!("Runtime errors in {}:\n{}", potential_file_path.display(), error_details));
                                    }
                                }
                            } else {
                                return Err(format!(
                                    "Export target '{}' in {} corresponds to neither a directory nor an .oxy file (checked {} and {})",
                                    name,
                                    entry_point_path.display(),
                                    potential_dir_path.display(),
                                    potential_file_path.display()
                                ));
                            }
                        }
                        // Removed Module case
                    }
                }
                // Evaluate non-export statements directly
                _ => {
                     match temp_interpreter.eval_statement(&statement) {
                         Ok(_) => { /* Statement evaluated successfully */ }
                         Err(RuntimeError::Return(_)) => {
                            return Err(format!("Runtime error in {}: Top-level return not allowed.", entry_point_path.display()));
                         }
                         Err(e) => {
                            return Err(format!("Runtime error in {}: {}", entry_point_path.display(), e));
                         }
                    }
                }
            }
        }
        Ok(())
    }

    // Helper function to interpret a Program without creating a new Interpreter instance
    // Modifies the existing environment.
    fn interpret_program_in_place(&mut self, program: Program) -> Result<(), Vec<RuntimeError>> {
        let mut errors = Vec::new();
        for statement in program.statements {
            match self.eval_statement(&statement) {
                Ok(_) => {} // Continue execution
                Err(RuntimeError::Return(_)) => {
                    // Top-level return in stdlib is an error
                     errors.push(RuntimeError::InvalidOperation("Return statement outside function is not allowed in stdlib initialization.".to_string()));
                     break;
                }
                Err(e) => {
                    errors.push(e);
                    // Stop on first error during stdlib initialization?
                    break;
                }
            }
        }
        if errors.is_empty() { Ok(()) } else { Err(errors) }
    }

    pub fn interpret(&mut self, program: Program) -> Result<(), Vec<RuntimeError>> {
        // This function remains the primary entry point for user code
        self.interpret_program_in_place(program)
    }

    // --- Statement Evaluation ---
    fn eval_statement(&mut self, statement: &Statement) -> Result<(), RuntimeError> {
        match statement {
            Statement::ExpressionStatement(expr) => {
                // Evaluate expression statements for their side effects
                self.eval_expression(expr)?;
                Ok(())
            },
            Statement::LetDeclaration { name, type_annotation: _, initializer } => {
                let value = match initializer {
                    Some(init_expr) => self.eval_expression(init_expr)?,
                    None => Value::Null, // Default to null if no initializer
                };
                 self.environment.borrow_mut().define(name.name.clone(), value);
                Ok(())
            },
            Statement::ReturnStatement { value } => {
                match value {
                    Some(expr) => {
                        let return_value = self.eval_expression(expr)?;
                        Err(RuntimeError::Return(return_value))
                    },
                    None => Err(RuntimeError::Return(Value::Null)),
                }
            },
            Statement::BreakStatement => {
                // Break statement just returns a Break error to be caught by the loop
                Err(RuntimeError::Break)
            },
            Statement::FunctionDeclaration { name, parameters, return_type: _, body } => {
                let func = Function {
                    name: name.name.clone(),
                    parameters: parameters.clone(),
                    body: Some(body.clone()),
                    env: Rc::clone(&self.environment), // Capture current environment
                    is_builtin: false,
                };
                self.environment.borrow_mut().define(name.name.clone(), Value::Function(Box::new(func)));
                Ok(())
            },
            Statement::IfStatement { condition, consequence, alternative } => {
                let condition_value = self.eval_expression(condition)?;
                if self.is_truthy(condition_value) {
                    self.eval_block_statement(consequence)?;
                } else if let Some(alt) = alternative {
                    self.eval_if_alternative(alt)?;
                }
                Ok(())
            },
            Statement::WhileStatement { condition, body } => {
                // Replace warning with actual implementation
                loop {
                    // Evaluate the condition
                    let condition_value = self.eval_expression(condition)?;
                    
                    // Check if we should continue looping
                    if !self.is_truthy(condition_value) {
                        break;
                    }
                    
                    // Execute the body of the loop
                    match self.eval_block_statement(body) {
                        Ok(_) => (), // Continue to next iteration
                        Err(RuntimeError::Return(value)) => return Err(RuntimeError::Return(value)), // Propagate return up
                        Err(RuntimeError::Break) => break, // Break out of the loop
                        Err(e) => return Err(e), // Propagate other errors
                    }
                }
                Ok(())
            },
            Statement::ForStatement { variable, iterable, body } => {
                 // TODO: Implement proper iteration
                eprintln!("Warning: For statement encountered but looping/iteration logic not implemented.");
                self.eval_expression(iterable)?;
               Ok(())
            },
            Statement::StructDeclaration { .. } => {
                eprintln!("Warning: Struct declaration encountered but not interpreted.");
               Ok(())
            },
            Statement::ImportStatement(decl) => self.eval_import_statement(decl),
            Statement::ExportStatement(_) => {
                // Exports are only processed during stdlib loading, ignore during normal execution
                Ok(())
            }
        }
   }

    fn eval_block_statement(&mut self, block: &BlockStatement) -> Result<(), RuntimeError> {
        // Create a new environment for the block scope
        let block_env = Rc::new(RefCell::new(Environment::new_with_outer(Rc::clone(&self.environment))));
        let previous_env = std::mem::replace(&mut self.environment, block_env);

        let mut result = Ok(());
        for statement in &block.statements {
            match self.eval_statement(statement) {
                Ok(_) => continue,
                Err(err @ RuntimeError::Return(_)) => {
                    result = Err(err); // Propagate return upwards
                    break;
                 },
                Err(other_err) => {
                    result = Err(other_err); // Propagate other errors
                    break;
                },
            }
        }

        // Restore the previous environment
        self.environment = previous_env;
        result
    }

    fn eval_if_alternative(&mut self, alternative: &IfAlternative) -> Result<(), RuntimeError> {
        match alternative {
            IfAlternative::Elif { condition, consequence, alternative: next_alternative } => {
                let condition_value = self.eval_expression(condition)?;
                if self.is_truthy(condition_value) {
                    self.eval_block_statement(consequence)?;
                } else if let Some(next_alt) = next_alternative {
                    // Need to box the recursive call? Check IfAlternative definition
                    self.eval_if_alternative(next_alt)?; // Assuming Box is handled
                }
                Ok(())
            }
            IfAlternative::Else { consequence } => {
                self.eval_block_statement(consequence)?;
                Ok(())
            }
        }
   }

    // --- New function to handle import statements ---
    fn eval_import_statement(&mut self, decl: &ImportDeclaration) -> Result<(), RuntimeError> {
        match decl {
            ImportDeclaration::ImportSymbols { source, symbols } => {
                match source {
                    ImportSource::Std => {
                        // Import from the dedicated stdlib environment
                        for (name, alias) in symbols {
                             let value = self.stdlib_environment.borrow().get(&name.name)
                                 .ok_or_else(|| RuntimeError::UndefinedVariable(format!("Symbol '{}' not found in standard library.", name.name)))?;
                             let import_name = alias.as_ref().map_or(&name.name, |a| &a.name);
                             self.environment.borrow_mut().define(import_name.clone(), value);
                        }
                        Ok(())
                    }
                    ImportSource::File(path) => {
                        // TODO: Implement file-based module loading
                        // This would involve reading, parsing, and executing the file
                        // in a *new* environment, then selectively importing symbols.
                         eprintln!("Warning: File-based import ('from \"{}\"') not yet implemented.", path);
                         Ok(())
                    }
                }
            }
            ImportDeclaration::ImportModule { path, alias } => {
                 // TODO: Implement whole module import
                 // This would load the module and bind it to the alias (or a default name)
                 eprintln!("Warning: Module import ('import \"{}\"') not yet implemented.", path);
                Ok(())
            }
        }
    }

    // --- Expression Evaluation ---
    fn eval_expression(&mut self, expression: &Expression) -> Result<Value, RuntimeError> {
        match expression {
            Expression::Literal(literal) => self.eval_literal(literal),
            Expression::Identifier(ident) => self.eval_identifier(ident),
            Expression::BinaryOperation { left, op, right } => {
                let left_val = self.eval_expression(left)?;
                let right_val = self.eval_expression(right)?;
                self.eval_binary_operation(op, left_val, right_val)
            },
            Expression::UnaryOperation { op, operand } => {
                let operand_val = self.eval_expression(operand)?;
                self.eval_unary_operation(op, operand_val)
            },
            Expression::Assignment { target, value } => {
                let assigned_value = self.eval_expression(value)?;
                match target.as_ref() {
                    Expression::Identifier(ident) => {
                        self.environment.borrow_mut().assign(&ident.name, assigned_value.clone())?;
                        Ok(assigned_value)
                    }
                     // TODO: Handle MemberAccess and IndexAccess assignments
                    _ => Err(RuntimeError::InvalidOperation(format!("Invalid assignment target: {:?}", target)))
                }
            },
            Expression::FunctionCall { callee, arguments } => self.eval_function_call(callee, arguments),
            Expression::ListInitializer { items } => self.eval_list_initializer(items),
            Expression::DictInitializer { pairs } => self.eval_dict_initializer(pairs),
            Expression::IndexAccess { base, index } => self.eval_index_access(base, index),
            Expression::MemberAccess { base, member } => self.eval_member_access(base, member),
        }
    }

    // --- Specific Expression Evaluators ---

    fn eval_literal(&self, literal: &Literal) -> Result<Value, RuntimeError> {
        match literal {
            Literal::Int(s) => s.parse::<i64>().map(Value::Int).map_err(|e| RuntimeError::InvalidOperation(format!("Invalid integer literal '{}': {}", s, e))),
            Literal::Float(s) => s.parse::<f64>().map(Value::Float).map_err(|e| RuntimeError::InvalidOperation(format!("Invalid float literal '{}': {}", s, e))),
            Literal::String(s) => Ok(Value::String(s.clone())),
            Literal::Boolean(b) => Ok(Value::Boolean(*b)),
            Literal::Null => Ok(Value::Null),
        }
    }

    fn eval_identifier(&mut self, identifier: &Identifier) -> Result<Value, RuntimeError> {
        // First, try to find the identifier in the current user environment
        if let Some(value) = self.environment.borrow().get(&identifier.name) {
            return Ok(value);
        }
        // If not found locally, check the stdlib environment (implicitly imported)
        // This provides access to things like `print` without explicit import for now.
        // TODO: Reconsider this implicit fallback - should `print` require `from std import print`?
        // For now, it allows built-ins defined in Rust (like print) to work without import.
        self.stdlib_environment.borrow().get(&identifier.name)
            .ok_or_else(|| RuntimeError::UndefinedVariable(identifier.name.clone()))
    }

    fn eval_binary_operation(&self, op: &BinaryOperator, left: Value, right: Value) -> Result<Value, RuntimeError> {
        match op {
            // Arithmetic
            BinaryOperator::Add => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l + r)),
                (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l + r)),
                (Value::Int(l), Value::Float(r)) => Ok(Value::Float(l as f64 + r)),
                (Value::Float(l), Value::Int(r)) => Ok(Value::Float(l + r as f64)),
                (Value::String(l), Value::String(r)) => Ok(Value::String(l + &r)),
                (Value::List(l), Value::List(r)) => {
                    let mut result = l.clone();
                    result.extend(r.clone());
                    Ok(Value::List(result))
                },
                (l, r) => Err(RuntimeError::TypeError(format!("Cannot add {} and {}", l, r))),
            },
            BinaryOperator::Subtract => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l - r)),
                (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l - r)),
                (Value::Int(l), Value::Float(r)) => Ok(Value::Float(l as f64 - r)),
                (Value::Float(l), Value::Int(r)) => Ok(Value::Float(l - r as f64)),
                (l, r) => Err(RuntimeError::TypeError(format!("Cannot subtract {} from {}", r, l))),
            },
            BinaryOperator::Multiply => match (left, right) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l * r)),
                (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l * r)),
                (Value::Int(l), Value::Float(r)) => Ok(Value::Float(l as f64 * r)),
                (Value::Float(l), Value::Int(r)) => Ok(Value::Float(l * r as f64)),
                (l, r) => Err(RuntimeError::TypeError(format!("Cannot multiply {} and {}", l, r))),
            },
            BinaryOperator::Divide => match (left, right) {
                (Value::Int(l), Value::Int(r)) => if r == 0 { Err(RuntimeError::InvalidOperation("Division by zero".to_string())) } else { Ok(Value::Float(l as f64 / r as f64)) },
                (Value::Float(l), Value::Float(r)) => if r == 0.0 { Err(RuntimeError::InvalidOperation("Division by zero".to_string())) } else { Ok(Value::Float(l / r)) },
                (Value::Int(l), Value::Float(r)) => if r == 0.0 { Err(RuntimeError::InvalidOperation("Division by zero".to_string())) } else { Ok(Value::Float(l as f64 / r)) },
                (Value::Float(l), Value::Int(r)) => if r == 0 { Err(RuntimeError::InvalidOperation("Division by zero".to_string())) } else { Ok(Value::Float(l / r as f64)) },
                (l, r) => Err(RuntimeError::TypeError(format!("Cannot divide {} by {}", l, r))),
            },
            BinaryOperator::Modulo => match (left, right) {
               (Value::Int(l), Value::Int(r)) => if r == 0 { Err(RuntimeError::InvalidOperation("Modulo by zero".to_string())) } else { Ok(Value::Int(l % r)) },
               (l, r) => Err(RuntimeError::TypeError(format!("Cannot calculate modulo for {} % {}", l, r))),
           },
           // Comparison
           BinaryOperator::Equal => Ok(Value::Boolean(self.values_equal(left, right)?)),
           BinaryOperator::NotEqual => Ok(Value::Boolean(!self.values_equal(left, right)?)),
           BinaryOperator::LessThan => self.compare_values(left, right).map(|ord| Value::Boolean(ord == Some(std::cmp::Ordering::Less))),
           BinaryOperator::GreaterThan => self.compare_values(left, right).map(|ord| Value::Boolean(ord == Some(std::cmp::Ordering::Greater))),
           BinaryOperator::LessEqual => self.compare_values(left, right).map(|ord| Value::Boolean(ord != Some(std::cmp::Ordering::Greater))),
           BinaryOperator::GreaterEqual => self.compare_values(left, right).map(|ord| Value::Boolean(ord != Some(std::cmp::Ordering::Less))),
           // Logical (implement short-circuiting later)
           BinaryOperator::And => Ok(Value::Boolean(self.is_truthy(left) && self.is_truthy(right))),
           BinaryOperator::Or => Ok(Value::Boolean(self.is_truthy(left) || self.is_truthy(right))),
       }
    }

    fn eval_unary_operation(&self, op: &UnaryOperator, operand: Value) -> Result<Value, RuntimeError> {
        match op {
            UnaryOperator::Not => Ok(Value::Boolean(!self.is_truthy(operand))),
            UnaryOperator::Negate => match operand {
                Value::Int(i) => Ok(Value::Int(-i)),
                Value::Float(f) => Ok(Value::Float(-f)),
                v => Err(RuntimeError::TypeError(format!("Cannot negate non-numeric value: {}", v))),
            },
        }
    }

    fn eval_function_call(&mut self, callee: &Expression, arguments: &[Expression]) -> Result<Value, RuntimeError> {
        let callee_val = self.eval_expression(callee)?;
        let mut arg_values = Vec::new();
        for arg_expr in arguments {
            arg_values.push(self.eval_expression(arg_expr)?);
        }

        match callee_val {
            Value::Function(func) => {
                if func.is_builtin {
                    // Check for string methods
                    if func.name == "to_upper" || func.name == "to_lower" {
                        // Retrieve the string value from the environment
                        if let Some(Value::String(s)) = func.env.borrow().get("__self") {
                            return if func.name == "to_upper" {
                                Ok(Value::String(s.to_uppercase()))
                            } else {
                                Ok(Value::String(s.to_lowercase()))
                            };
                        }
                    }
                    
                    // Otherwise, call the standard builtin function
                    self.call_builtin_function(&func.name, arg_values)
                } else {
                    self.call_user_function(*func, arg_values)
                }
            }
            // Special case for .toString() method-like call
            _ if matches!(callee, Expression::MemberAccess { member, .. } if member.name == "toString") => {
                if !arguments.is_empty() {
                    return Err(RuntimeError::TypeError(".toString() expects no arguments".to_string()));
                }
                if let Expression::MemberAccess { base, .. } = callee {
                    let base_val = self.eval_expression(base)?;
                    Ok(Value::String(base_val.to_string())) // Use Display impl
                } else {
                    unreachable!()
                }
            }
            _ => Err(RuntimeError::TypeError(format!("Cannot call non-function value: {}", callee_val))),
        }
    }

    fn call_builtin_function(&mut self, name: &str, args: Vec<Value>) -> Result<Value, RuntimeError> {
         match name {
             "print" => runtime::stdlib::builtin_print(args),
             "len" => runtime::stdlib::builtin_len(args),
             "type" => runtime::stdlib::builtin_type(args),
             _ => Err(RuntimeError::InvalidOperation(format!("Unknown built-in function: {}", name))),
         }
    }

    fn call_user_function(&mut self, func: Function, args: Vec<Value>) -> Result<Value, RuntimeError> {
        if args.len() != func.parameters.len() {
            return Err(RuntimeError::TypeError(format!(
                "Function '{}' expected {} arguments but got {}",
                func.name, func.parameters.len(), args.len()
            )));
        }

        let func_env = Rc::new(RefCell::new(Environment::new_with_outer(Rc::clone(&func.env))));
        for (i, (param, _)) in func.parameters.iter().enumerate() {
            func_env.borrow_mut().define(param.name.clone(), args[i].clone());
        }

        if let Some(body) = func.body {
             // Temporarily switch environment
            let previous_env = std::mem::replace(&mut self.environment, func_env);
            let result = self.execute_function_body(&body);
            self.environment = previous_env; // Restore environment
            result
        } else {
            Err(RuntimeError::InvalidOperation("User-defined function has no body".to_string()))
        }
    }

    // Helper to execute body and capture Return error
    fn execute_function_body(&mut self, body: &BlockStatement) -> Result<Value, RuntimeError> {
        match self.eval_block_statement(body) {
            Ok(_) => Ok(Value::Null), // Implicit return null
            Err(RuntimeError::Return(value)) => Ok(value), // Explicit return
            Err(other_err) => Err(other_err), // Propagate other errors
        }
    }

    fn eval_list_initializer(&mut self, items: &[Expression]) -> Result<Value, RuntimeError> {
        let mut list_items = Vec::with_capacity(items.len());
        for item_expr in items {
            list_items.push(self.eval_expression(item_expr)?);
        }
        Ok(Value::List(list_items))
    }

    fn eval_dict_initializer(&mut self, pairs: &[(Expression, Expression)]) -> Result<Value, RuntimeError> {
        // TODO: Implement actual Dict value type
        for (k, v) in pairs {
           self.eval_expression(k)?;
           self.eval_expression(v)?;
        }
        eprintln!("Warning: Dict initializer evaluated but dict value not created.");
        Ok(Value::Null) // Placeholder
    }

    fn eval_index_access(&mut self, base_expr: &Expression, index_expr: &Expression) -> Result<Value, RuntimeError> {
        let base_val = self.eval_expression(base_expr)?;
        let index_val = self.eval_expression(index_expr)?;

        match (base_val, index_val) {
            (Value::String(s), Value::Int(idx)) => {
                if idx < 0 {
                    return Err(RuntimeError::InvalidOperation(format!("Index out of bounds: index {} is negative", idx)));
                }
                let u_idx = idx as usize;
                match s.chars().nth(u_idx) {
                    Some(ch) => Ok(Value::String(ch.to_string())),
                    None => Err(RuntimeError::InvalidOperation(format!("Index out of bounds: index {} >= string length {}", idx, s.chars().count())))
                }
            }
            (Value::List(list), Value::Int(idx)) => {
                if idx < 0 {
                    return Err(RuntimeError::InvalidOperation(format!("Index out of bounds: index {} is negative", idx)));
                }
                let u_idx = idx as usize;
                if u_idx >= list.len() {
                    return Err(RuntimeError::InvalidOperation(format!("Index out of bounds: index {} >= list length {}", idx, list.len())));
                }
                Ok(list[u_idx].clone())
            }
            (b, i) => Err(RuntimeError::TypeError(format!("Cannot index type {} with type {}", b, i))),
        }
    }

    fn eval_member_access(&mut self, base_expr: &Expression, member: &Identifier) -> Result<Value, RuntimeError> {
        let base_val = self.eval_expression(base_expr)?;
        match (&base_val, member.name.as_str()) {
            (Value::String(s), "length") => {
                Ok(Value::Int(s.chars().count() as i64))
            }
            (Value::List(list), "length") => {
                Ok(Value::Int(list.len() as i64))
            }
            // Add string case conversion methods
            (Value::String(s), "to_upper") => {
                // Return a function that will convert to uppercase when called
                let upper_fn = Function {
                    name: "to_upper".to_string(),
                    parameters: vec![],
                    body: None, // No body for built-in methods
                    env: Rc::new(RefCell::new(Environment::new())),
                    is_builtin: true,
                };
                // Store the string value in the environment for later access
                upper_fn.env.borrow_mut().define("__self".to_string(), Value::String(s.clone()));
                Ok(Value::Function(Box::new(upper_fn)))
            }
            (Value::String(s), "to_lower") => {
                // Return a function that will convert to lowercase when called
                let lower_fn = Function {
                    name: "to_lower".to_string(),
                    parameters: vec![],
                    body: None, // No body for built-in methods
                    env: Rc::new(RefCell::new(Environment::new())),
                    is_builtin: true,
                };
                // Store the string value in the environment for later access
                lower_fn.env.borrow_mut().define("__self".to_string(), Value::String(s.clone()));
                Ok(Value::Function(Box::new(lower_fn)))
            }
            // TODO: Add Struct field access
            // Check for methods? (e.g., list.append) - This might require binding methods to values
            _ => Err(RuntimeError::InvalidOperation(format!("Member access for '.{}' not implemented on type {}", member.name, base_val)))
        }
    }

    // --- Helper Methods ---

    fn is_truthy(&self, value: Value) -> bool {
        match value {
            Value::Null => false,
            Value::Boolean(b) => b,
            Value::Int(i) => i != 0,
            Value::Float(f) => f != 0.0 && !f.is_nan(),
            Value::String(s) => !s.is_empty(),
            Value::Function(_) => true,
            Value::List(list) => !list.is_empty(),
        }
    }

    fn compare_values(&self, left: Value, right: Value) -> Result<Option<std::cmp::Ordering>, RuntimeError> {
        match (left, right) {
            (Value::Int(l), Value::Int(r)) => Ok(l.partial_cmp(&r)),
            (Value::Float(l), Value::Float(r)) => Ok(l.partial_cmp(&r)),
            (Value::Int(l), Value::Float(r)) => Ok((l as f64).partial_cmp(&r)),
            (Value::Float(l), Value::Int(r)) => Ok(l.partial_cmp(&(r as f64))),
            (Value::String(l), Value::String(r)) => Ok(l.partial_cmp(&r)),
            // Lists can be compared if their elements are comparable
            (Value::List(l), Value::List(r)) => {
                // Basic length comparison first for efficiency
                if l.len() != r.len() {
                    return Ok(l.len().partial_cmp(&r.len()));
                }
                
                // Compare each element
                for (left_item, right_item) in l.iter().zip(r.iter()) {
                    match self.compare_values(left_item.clone(), right_item.clone())? {
                        Some(std::cmp::Ordering::Equal) => continue, // Elements are equal, continue
                        Some(order) => return Ok(Some(order)), // Found a non-equal pair
                        None => return Ok(None), // Incomparable elements found
                    }
                }
                
                // All elements equal
                Ok(Some(std::cmp::Ordering::Equal))
            }
            // Incomparable types
            (l, r) => Err(RuntimeError::TypeError(format!("Cannot compare {} and {}", l, r))),
        }
    }

    fn values_equal(&self, left: Value, right: Value) -> Result<bool, RuntimeError> {
        match (left, right) {
            (Value::Null, Value::Null) => Ok(true),
            (Value::Boolean(l), Value::Boolean(r)) => Ok(l == r),
            (Value::Int(l), Value::Int(r)) => Ok(l == r),
            (Value::Float(l), Value::Float(r)) => Ok(l == r), // Precision issues?
            (Value::Int(l), Value::Float(r)) => Ok((l as f64) == r),
            (Value::Float(l), Value::Int(r)) => Ok(l == (r as f64)),
            (Value::String(l), Value::String(r)) => Ok(l == r),
            (Value::Function(l), Value::Function(r)) => Ok(*l == *r),
            (Value::List(l), Value::List(r)) => {
                // Check if lists have same length
                if l.len() != r.len() {
                    return Ok(false);
                }
                
                // Check each element for equality
                for (left_item, right_item) in l.iter().zip(r.iter()) {
                    if !self.values_equal(left_item.clone(), right_item.clone())? {
                        return Ok(false);
                    }
                }
                
                // All elements are equal
                Ok(true)
            }
            // Different types are not equal
            _ => Ok(false),
        }
    }
} 