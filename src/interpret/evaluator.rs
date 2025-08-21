use crate::lexer::astgen::{
    BinaryOperator, BlockStatement, Expression, Identifier, Literal, Program, Statement, UnaryOperator,
    IfAlternative, ImportDeclaration, ImportSource, ExportDeclaration // Ensure IfAlternative, ImportDeclaration, and ImportSource are imported if used
};
use crate::runtime; // Import the runtime module (for stdlib)
use crate::interpret::value::{Value, Function, StructDefinitionValue, StructInstanceValue, BoundMethodValue};
use crate::interpret::environment::Environment;
use crate::interpret::error::RuntimeError;

use std::rc::Rc;
use std::cell::RefCell;
use std::fs; // Import fs for file reading
use crate::lexer::{Tokenizer, Parser}; // Import Tokenizer and Parser
use std::path::{Path, PathBuf}; // Import Path and PathBuf
use std::collections::HashMap; // Add HashMap

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

        // DEBUG: Print contents of stdlib env after loading
        // println!("--- Stdlib Environment Contents After Loading ---");
        // stdlib_env.borrow().debug_print("");
        // println!("-----------------------------------------------");

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

                            // println!("DEBUG: Attempting to load exported file: {}", potential_file_path.display()); // DEBUG

                            if potential_dir_path.is_dir() {
                                // It's a subdirectory, load its entry point (mod.oxy)
                                Self::load_oxygen_module(&potential_dir_path, Rc::clone(&target_env))?;
                            } else if potential_file_path.exists() {
                                // It's a sibling file, load and execute it.
                                // println!("DEBUG: Found file, loading: {}", potential_file_path.display()); // DEBUG
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

                                // println!("DEBUG: Interpreting file: {}", potential_file_path.display()); // DEBUG
                                // Evaluate the file's program using the *same* temporary interpreter
                                match temp_interpreter.interpret_program_in_place(file_program) {
                                    Ok(_) => { 
                                        // println!("DEBUG: Successfully interpreted file: {}", potential_file_path.display()); // DEBUG
                                        /* File loaded successfully */ 
                                    }
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
                let iterable_val = self.eval_expression(iterable)?;

                // Create a loop scope environment that persists across iterations
                let loop_env = Rc::new(RefCell::new(Environment::new_with_outer(Rc::clone(&self.environment))));
                let previous_env = std::mem::replace(&mut self.environment, Rc::clone(&loop_env));

                // Helper to set or define the loop variable
                let set_loop_var = |env: &Rc<RefCell<Environment>>, name: &str, value: Value| {
                    if env.borrow().get(name).is_some() {
                        let _ = env.borrow_mut().assign(name, value);
                    } else {
                        env.borrow_mut().define(name.to_string(), value);
                    }
                };

                let result = match iterable_val {
                    Value::List(items) => {
                        let mut out: Result<(), RuntimeError> = Ok(());
                        for item in items.into_iter() {
                            set_loop_var(&loop_env, &variable.name, item);
                            match self.eval_block_statement(body) {
                                Ok(_) => {}
                                Err(RuntimeError::Break) => { out = Ok(()); break; }
                                Err(RuntimeError::Return(v)) => { out = Err(RuntimeError::Return(v)); break; }
                                Err(e) => { out = Err(e); break; }
                            }
                        }
                        out
                    }
                    Value::String(s) => {
                        let mut out: Result<(), RuntimeError> = Ok(());
                        for ch in s.chars() {
                            set_loop_var(&loop_env, &variable.name, Value::String(std::borrow::Cow::Owned(ch.to_string())));
                            match self.eval_block_statement(body) {
                                Ok(_) => {}
                                Err(RuntimeError::Break) => { out = Ok(()); break; }
                                Err(RuntimeError::Return(v)) => { out = Err(RuntimeError::Return(v)); break; }
                                Err(e) => { out = Err(e); break; }
                            }
                        }
                        out
                    }
                    Value::Dict(map) => {
                        let keys: Vec<String> = map.keys().cloned().collect();
                        let mut out: Result<(), RuntimeError> = Ok(());
                        for key in keys {
                            set_loop_var(&loop_env, &variable.name, Value::String(std::borrow::Cow::Owned(key)));
                            match self.eval_block_statement(body) {
                                Ok(_) => {}
                                Err(RuntimeError::Break) => { out = Ok(()); break; }
                                Err(RuntimeError::Return(v)) => { out = Err(RuntimeError::Return(v)); break; }
                                Err(e) => { out = Err(e); break; }
                            }
                        }
                        out
                    }
                    other => Err(RuntimeError::TypeError(format!(
                        "Cannot iterate over type {}",
                        other.type_name()
                    ))),
                };

                // Restore previous environment
                self.environment = previous_env;
                result
            },
            Statement::StructDeclaration(struct_def) => {
                // Store the struct definition itself in the environment
                let def_value = StructDefinitionValue {
                    name: struct_def.name.clone(),
                    fields: struct_def.fields.clone(),
                    methods: Rc::new(RefCell::new(HashMap::new())), // Initialize empty methods map
                };
                self.environment.borrow_mut().define(struct_def.name.name.clone(), Value::StructDefinition(def_value));
                Ok(())
            },
            Statement::ImplBlock { struct_name, methods } => {
                // Find the struct definition
                let def_val = self.environment.borrow().get(&struct_name.name)
                    .ok_or_else(|| RuntimeError::TypeError(format!("Cannot impl for undefined struct '{}'.", struct_name.name)))?;

                match def_val {
                    Value::StructDefinition(struct_def_value) => {
                        // Add methods to the definition's map
                        let mut methods_map = struct_def_value.methods.borrow_mut();
                        for method_def in methods {
                            methods_map.insert(method_def.name.name.clone(), method_def.clone());
                        }
                        Ok(())
                    }
                    _ => Err(RuntimeError::TypeError(format!("Cannot impl for non-struct type '{}'.", struct_name.name))),
                }
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
                // Implement short-circuiting for And/Or
                match op {
                    BinaryOperator::And => {
                        let left_val = self.eval_expression(left)?;
                        if !self.is_truthy(left_val.clone()) {
                            return Ok(Value::Boolean(false));
                        }
                        let right_val = self.eval_expression(right)?;
                        return Ok(Value::Boolean(self.is_truthy(right_val)));
                    }
                    BinaryOperator::Or => {
                        let left_val = self.eval_expression(left)?;
                        if self.is_truthy(left_val.clone()) {
                            return Ok(Value::Boolean(true));
                        }
                        let right_val = self.eval_expression(right)?;
                        return Ok(Value::Boolean(self.is_truthy(right_val)));
                    }
                    _ => {
                        let left_val = self.eval_expression(left)?;
                        let right_val = self.eval_expression(right)?;
                        self.eval_binary_operation(op, left_val, right_val)
                    }
                }
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
            Expression::StructInitializer { name, fields } => self.eval_struct_initializer(name, fields),
        }
    }

    // --- Specific Expression Evaluators ---

    fn eval_literal(&self, literal: &Literal) -> Result<Value, RuntimeError> {
        match literal {
            Literal::Int(s) => s.parse::<i64>().map(Value::Int).map_err(|e| RuntimeError::InvalidOperation(format!("Invalid integer literal '{}': {}", s, e))),
            Literal::Float(s) => s.parse::<f64>().map(Value::Float).map_err(|e| RuntimeError::InvalidOperation(format!("Invalid float literal '{}': {}", s, e))),
            Literal::String(s) => Ok(Value::String(std::borrow::Cow::Owned(s.clone()))),
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
                (Value::String(l), Value::String(r)) => Ok(Value::String(std::borrow::Cow::Owned(l.into_owned() + &r))),
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
           // Logical (short-circuiting is handled earlier)
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
        let mut arg_values = Vec::with_capacity(arguments.len());
        for arg_expr in arguments {
            arg_values.push(self.eval_expression(arg_expr)?);
        }

        match callee_val {
            Value::Function(func) => {
                // Check if it's a built-in method captured via member access
                if func.is_builtin && func.env.borrow().get("__self").is_some() {
                    // Retrieve the captured 'self' value
                    let self_val = func.env.borrow().get("__self")
                        .ok_or_else(|| RuntimeError::InvalidOperation("Internal error: Missing '__self' in built-in method env".to_string()))?;

                    // Execute the specific built-in method logic
                    match (&self_val, func.name.as_str()) {
                        (Value::String(s), "to_upper") => Ok(Value::String(std::borrow::Cow::Owned(s.to_uppercase()))),
                        (Value::String(s), "to_lower") => Ok(Value::String(std::borrow::Cow::Owned(s.to_lowercase()))),
                        (Value::String(_), "trim_end") => { // String captured in __self
                            // We need the *original* arguments passed to the call
                            // Call the actual Rust implementation
                            // Prepend self to the args for the builtin call
                            let mut final_args = vec![self_val];
                            final_args.extend(arg_values); 
                            runtime::stdlib::builtin_string_trim_end(final_args)                            
                        },
                        (Value::List(l), "contains") => {
                            if arg_values.len() != 1 {
                                return Err(RuntimeError::TypeError(format!(
                                    ".contains() expects 1 argument, got {}", arg_values.len()
                                )));
                            }
                            let value_to_find = &arg_values[0];
                            // Perform equality check using values_equal helper
                            let mut found = false;
                            for item in l.iter() {
                                if self.values_equal(item.clone(), value_to_find.clone())? {
                                    found = true;
                                    break;
                                }
                            }
                            Ok(Value::Boolean(found))
                        }
                        // Add other built-in methods here (e.g., list.push, string.split)
                        _ => Err(RuntimeError::InvalidOperation(format!(
                            "Internal error: Unhandled built-in method '{}' for type {}", func.name, self_val
                        )))
                    }
                } else if func.is_builtin {
                    // Regular Rust built-in function (like print, len, type, abs, max, min)
                    // Note: We use the function name stored in the Function struct now.
                    self.call_rust_builtin_function(&func.name, arg_values)
                } else {
                    // User-defined Oxygen function
                    self.call_user_function(*func, arg_values)
                }
            }
            Value::BoundMethod(bound_method) => {
                // Calling a method like instance.method()
                self.call_struct_method(bound_method, arg_values)
            }
            _ => Err(RuntimeError::TypeError(format!("Cannot call non-function value: {}", callee_val))),
        }
    }

    // Renamed from call_builtin_function to be more specific
    fn call_rust_builtin_function(&mut self, name: &str, args: Vec<Value>) -> Result<Value, RuntimeError> {
         match name {
            "print" => runtime::stdlib::builtin_print(args),
            "println" => runtime::stdlib::builtin_println(args),
            "len" => runtime::stdlib::builtin_len(args),
            "type" => runtime::stdlib::builtin_type(args),
            "to_string" | "__to_string" => runtime::stdlib::builtin_to_string(args),
            "__tcp_connect" => runtime::stdlib::builtin_tcp_connect(args),
            "__tcp_connect_with_timeout" => runtime::stdlib::builtin_tcp_connect_with_timeout(args),
            "__socket_write" => runtime::stdlib::builtin_socket_write(args),
            "__socket_read" => runtime::stdlib::builtin_socket_read(args),
            "__http_get" => runtime::stdlib::builtin_http_get(args),
            // TCP Listener
            "__tcp_bind" => runtime::stdlib::builtin_tcp_listener_bind(args),
            "__tcp_accept" => runtime::stdlib::builtin_tcp_listener_accept(args),
            // UDP Socket
            "__udp_bind" => runtime::stdlib::builtin_udp_bind(args),
            "__udp_send_to" => runtime::stdlib::builtin_udp_send_to(args),
            "__udp_recv_from" => runtime::stdlib::builtin_udp_recv_from(args),
            // String built-ins
            "__string_trim_end" => runtime::stdlib::builtin_string_trim_end(args),
            // Note: abs, max, min are loaded from Oxygen stdlib now, not handled here.
            _ => Err(RuntimeError::InvalidOperation(format!("Unknown Rust built-in function: {}", name))),
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
        let mut map = std::collections::HashMap::new();
        for (key_expr, value_expr) in pairs {
            let key_val = self.eval_expression(key_expr)?;
            let key_string = match key_val {
                Value::String(s) => s.into_owned(),
                other => {
                    return Err(RuntimeError::TypeError(format!(
                        "Dict keys must be strings, got {}",
                        other.type_name()
                    )))
                }
            };
            let value_val = self.eval_expression(value_expr)?;
            map.insert(key_string, value_val);
        }
        Ok(Value::Dict(map))
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
                    Some(ch) => Ok(Value::String(std::borrow::Cow::Owned(ch.to_string()))),
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
            (Value::Dict(map), Value::String(key)) => {
                match map.get(key.as_ref()) {
                    Some(v) => Ok(v.clone()),
                    None => Err(RuntimeError::InvalidOperation(format!(
                        "Key '{}' not found in dict",
                        key
                    )))
                }
            }
            (b, i) => Err(RuntimeError::TypeError(format!("Cannot index type {} with type {}", b, i))),
        }
    }

    fn eval_member_access(&mut self, base_expr: &Expression, member: &Identifier) -> Result<Value, RuntimeError> {
        let base_val = self.eval_expression(base_expr)?;
        let member_name = member.name.as_str();

        match base_val {
            Value::String(s) => match member_name {
                "length" => {
                    // Return the length directly as an Int
                    Ok(Value::Int(s.chars().count() as i64))
                }
                "to_upper" | "to_lower" | "trim_end" => {
                    // Methods returning a function
                    let func = Function {
                        name: member_name.to_string(),
                        parameters: if member_name == "trim_end" { 
                            vec![(Identifier { name: "pattern".to_string() }, None)] // trim_end takes pattern
                        } else {
                            vec![] // to_upper/to_lower take no args
                        },
                        body: None,
                        env: Rc::new(RefCell::new(Environment::new())), // Env to capture self
                        is_builtin: true, // Mark as special built-in method
                    };
                    // Capture the string value
                    func.env.borrow_mut().define("__self".to_string(), Value::String(s));
                    Ok(Value::Function(Box::new(func)))
                }
                 _ => Err(RuntimeError::InvalidOperation(format!(
                    "String has no member or method named '{}'", member_name
                )))
            },

            Value::List(list) => match member_name {
                "length" => {
                    // Return the length directly as an Int
                    Ok(Value::Int(list.len() as i64))
                }
                "contains" => {
                    // Return a closure (Function) that checks for containment
                    let func = Function {
                        name: "contains".to_string(),
                        // Expects one argument: the value to search for
                        parameters: vec![(Identifier { name: "value".to_string() }, None)],
                        body: None,
                        env: Rc::new(RefCell::new(Environment::new())), // Env to capture self
                        is_builtin: true, // Mark as special built-in method
                    };
                    // Capture the list value
                    func.env.borrow_mut().define("__self".to_string(), Value::List(list));
                    Ok(Value::Function(Box::new(func)))
                }
                 _ => Err(RuntimeError::InvalidOperation(format!(
                    "List has no member or method named '{}'", member_name
                )))
            },

            Value::Dict(map) => match member_name {
                "length" => Ok(Value::Int(map.len() as i64)),
                _ => Err(RuntimeError::InvalidOperation(format!(
                    "Dict has no member or method named '{}'",
                    member_name
                )))
            },

            // Added: Struct Instance Field Access
            Value::StructInstance(instance) => {
                // 1. Check for fields first
                let fields_map = instance.fields.borrow();
                if let Some(value) = fields_map.get(member_name) {
                    return Ok(value.clone());
                }

                // 2. If not a field, check for methods on the struct definition
                // Need to find the struct definition again (or store Rc<StructDef> in instance?)
                let def_val = self.environment.borrow().get(&instance.type_name.name)
                   .or_else(|| self.stdlib_environment.borrow().get(&instance.type_name.name))
                   .ok_or_else(|| RuntimeError::TypeError(format!(
                       "Internal error: Struct definition '{}' not found for instance.", instance.type_name.name
                   )))?;

                if let Value::StructDefinition(struct_def) = def_val {
                    let methods_map = struct_def.methods.borrow();
                    if let Some(method_def) = methods_map.get(member_name) {
                        // Return a BoundMethod value, capturing the instance and method def
                        Ok(Value::BoundMethod(BoundMethodValue {
                            instance: instance.clone(), // Clone the instance info
                            method: method_def.clone(), // Clone the method definition
                        }))
                    } else {
                        Err(RuntimeError::InvalidOperation(format!(
                            "Struct '{}' has no field or method named '{}'",
                            instance.type_name.name,
                            member_name
                        )))
                    }
                } else {
                     Err(RuntimeError::TypeError(format!(
                        "Internal error: Expected struct definition for type '{}', found {}.",
                        instance.type_name.name,
                        def_val.type_name()
                    )))
                }
            }

            other_type => Err(RuntimeError::InvalidOperation(format!(
                "Member access ('.{}') not supported on type {}", member_name, other_type.type_name()
            )))
        }
    }

    // Added: Handles Struct Initialization
    fn eval_struct_initializer(&mut self, name: &Identifier, fields: &[(Identifier, Expression)]) -> Result<Value, RuntimeError> {
        // 1. Find the struct definition in the environment
        let def_val = self.environment.borrow().get(&name.name)
            .ok_or_else(|| RuntimeError::TypeError(format!("Struct type '{}' not found.", name.name)))?;

        let struct_def = match def_val {
            Value::StructDefinition(def) => def,
            _ => return Err(RuntimeError::TypeError(format!("'{}' is not a struct type.", name.name))),
        };

        // 2. Evaluate provided field values
        let mut instance_fields = HashMap::new();
        for (field_ident, field_expr) in fields {
            let field_value = self.eval_expression(field_expr)?;
            instance_fields.insert(field_ident.name.clone(), field_value);
        }

        // 3. Check if all declared fields are present (optional, could allow partial init?)
        // For now, let's require all fields defined in the struct to be initialized.
        for defined_field in &struct_def.fields {
            if !instance_fields.contains_key(&defined_field.name.name) {
                return Err(RuntimeError::InvalidOperation(format!(
                    "Missing field '{}' in initializer for struct '{}'",
                    defined_field.name.name,
                    name.name
                )));
            }
        }
        // Optional: Check for extra fields provided in initializer?

        // 4. Create and return the StructInstanceValue
        Ok(Value::StructInstance(StructInstanceValue {
            type_name: name.clone(),
            fields: Rc::new(RefCell::new(instance_fields)),
        }))
    }

    // --- New function to handle calling struct methods ---
    fn call_struct_method(&mut self, bound_method: BoundMethodValue, args: Vec<Value>) -> Result<Value, RuntimeError> {
        let method = bound_method.method;
        let instance = bound_method.instance;

        // Parameter count check (excluding self)
        let non_self_params: Vec<_> = method.parameters.iter().filter(|(p, _)| p.name != "self").collect();
        if args.len() != non_self_params.len() {
             return Err(RuntimeError::TypeError(format!(
                 "Method '{}.{}' expected {} arguments (excluding self) but got {}",
                 instance.type_name.name, method.name.name, non_self_params.len(), args.len()
             )));
        }

        // Create method execution environment, enclosing the definition environment (likely global/stdlib for now)
        // TODO: Does the method capture the struct definition env? Or just global?
        // Let's assume global/stdlib for now via stdlib_environment
        let method_env = Rc::new(RefCell::new(Environment::new_with_outer(Rc::clone(&self.stdlib_environment))));

        // Define 'self' in the method environment
        method_env.borrow_mut().define("self".to_string(), Value::StructInstance(instance));

        // Define other arguments
        let mut arg_idx = 0;
        for (param_ident, _) in &method.parameters {
            if param_ident.name == "self" { continue; } // Skip self
            method_env.borrow_mut().define(param_ident.name.clone(), args[arg_idx].clone());
            arg_idx += 1;
        }

        // Execute method body in the method environment
        let previous_env = std::mem::replace(&mut self.environment, method_env);
        let result = self.execute_function_body(&method.body);
        self.environment = previous_env; // Restore environment

        result // Return value or propagate error (like Return)
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
            Value::Dict(map) => !map.is_empty(),
            // Struct definitions/instances are considered truthy by default
            Value::StructDefinition(_) => true,
            Value::StructInstance(_) => true,
            Value::BoundMethod(_) => true,
            Value::NativeResource(_) => true, // Native resources are truthy
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
            // Dicts are not comparable by default
            (Value::Dict(_), Value::Dict(_)) => Err(RuntimeError::TypeError("Cannot compare dicts".to_string())),
            // Structs are not comparable by default
            (Value::StructDefinition(l), Value::StructDefinition(r)) => Err(RuntimeError::TypeError(format!("Cannot compare struct definitions: {} and {}", l.name.name, r.name.name))),
            (Value::StructInstance(l), Value::StructInstance(r)) => Err(RuntimeError::TypeError(format!("Cannot compare struct instances of type {}", l.type_name.name))),
            // Bound methods are not comparable
            (Value::BoundMethod(_), _) | (_, Value::BoundMethod(_)) => Err(RuntimeError::TypeError("Cannot compare bound methods".to_string())),
            // Native resources are not comparable
            (Value::NativeResource(_), _) | (_, Value::NativeResource(_)) => Err(RuntimeError::TypeError("Cannot compare native resources".to_string())),
            // Incomparable types
            (l, r) => Err(RuntimeError::TypeError(format!("Cannot compare {} and {}", l.type_name(), r.type_name()))),
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
            (Value::Dict(l), Value::Dict(r)) => Ok(l == r),
            (Value::StructInstance(l), Value::StructInstance(r)) => {
                 // Instances are equal if they are the same type and fields are equal
                 Ok(l.type_name == r.type_name &&
                 *l.fields.borrow() == *r.fields.borrow())
             }
             // Struct definitions are generally not comparable for equality
             (Value::StructDefinition(_), Value::StructDefinition(_)) => Ok(false),
            // Bound methods are not equal unless identical object + method
            (Value::BoundMethod(l), Value::BoundMethod(r)) => {
                // Using ptr equality for method definition comparison
                // Instance comparison uses the PartialEq impl for StructInstanceValue
                 Ok(std::ptr::eq(&l.method, &r.method) && l.instance == r.instance)
            },
            // Native resources only equal if same pointer
            (Value::NativeResource(l), Value::NativeResource(r)) => Ok(Rc::ptr_eq(&l, &r)),
            // Different types are not equal
            _ => Ok(false),
        }
    }
}

// Add type_name helper method to Value if not already present
impl Value {
    fn type_name(&self) -> &'static str {
        match self {
            Value::Int(_) => "int",
            Value::Float(_) => "float",
            Value::String(_) => "string",
            Value::Boolean(_) => "boolean",
            Value::Null => "null",
            Value::Function(_) => "function",
            Value::List(_) => "list",
            Value::Dict(_) => "dict",
            Value::StructDefinition(_) => "struct definition",
            Value::StructInstance(_) => "struct instance",
            Value::BoundMethod(_) => "bound method",
            Value::NativeResource(_) => "native resource",
        }
    }
} 