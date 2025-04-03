use crate::lexer::astgen::{
    BinaryOperator, BlockStatement, Expression, Identifier, Literal, Program, Statement, UnaryOperator,
    IfAlternative // Ensure IfAlternative is imported if used
};
use crate::runtime; // Import the runtime module (for stdlib)
use crate::interpret::value::{Value, Function};
use crate::interpret::environment::Environment;
use crate::interpret::error::RuntimeError;

use std::rc::Rc;
use std::cell::RefCell;

// --- Interpreter ---

pub struct Interpreter {
    environment: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut global_env = Environment::new();

        // Register built-in functions
        global_env.define(
            "print".to_string(),
            Value::Function(Box::new(Function {
                name: "print".to_string(),
                parameters: vec![], // Parameters are dynamic for built-ins like print
                body: None,
                env: Rc::new(RefCell::new(Environment::new())), // Built-ins don't need outer env
                is_builtin: true,
            }))
        );

        // TODO: Register other built-ins (len, type, etc.) here

        Interpreter {
            environment: Rc::new(RefCell::new(global_env)),
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
                // TODO: Implement proper looping
                eprintln!("Warning: While statement encountered but looping logic not implemented.");
                self.eval_expression(condition)?; // Evaluate condition once
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
            Statement::ImportStatement { .. } => {
                 // TODO: Implement module loading
                eprintln!("Warning: Import statement encountered but not interpreted.");
               Ok(())
            },
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
        self.environment.borrow().get(&identifier.name)
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
             // "len" => runtime::stdlib::builtin_len(args), // Example
             // TODO: Add other built-ins
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
        // TODO: Implement actual List value type
        for item_expr in items { self.eval_expression(item_expr)?; }
        eprintln!("Warning: List initializer evaluated but list value not created.");
        Ok(Value::Null) // Placeholder
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
            // TODO: Add List/Array indexing
            (b, i) => Err(RuntimeError::TypeError(format!("Cannot index type {} with type {}", b, i))),
        }
    }

    fn eval_member_access(&mut self, base_expr: &Expression, member: &Identifier) -> Result<Value, RuntimeError> {
        let base_val = self.eval_expression(base_expr)?;
        match base_val {
            Value::String(s) if member.name == "length" => {
                Ok(Value::Int(s.chars().count() as i64))
            }
            // TODO: Add List/Array .length
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
        }
    }

    fn compare_values(&self, left: Value, right: Value) -> Result<Option<std::cmp::Ordering>, RuntimeError> {
        match (left, right) {
            (Value::Int(l), Value::Int(r)) => Ok(l.partial_cmp(&r)),
            (Value::Float(l), Value::Float(r)) => Ok(l.partial_cmp(&r)),
            (Value::Int(l), Value::Float(r)) => Ok((l as f64).partial_cmp(&r)),
            (Value::Float(l), Value::Int(r)) => Ok(l.partial_cmp(&(r as f64))),
            (Value::String(l), Value::String(r)) => Ok(l.partial_cmp(&r)),
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
            // Different types are not equal
            _ => Ok(false),
        }
    }
} 