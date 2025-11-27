pub mod lexer;
pub mod interpret;
pub mod runtime;

use lexer::{Parser, Tokenizer};
use interpret::Interpreter;
use std::env;
use std::fs;
use std::process;

pub fn main() {

    let mut source_code = String::new();
    let mut source_origin = "<embedded>".to_string();

    let file_path = "code.txt";
    source_origin = file_path.to_string();
    match fs::read_to_string("code.txt") {
        Ok(contents) => {
            source_code = contents;
        }
        Err(e) => {
            eprintln!("Error reading file '{}': {}", file_path, e);
            process::exit(1);
        }
    }

    println!("--- Running Source: {} ---", source_origin);
    // Optionally print source code if desired
    println!("{}", source_code);

    let debug_enabled = std::env::var_os("OXY_DEBUG").is_some();
    if debug_enabled {
        println!("\n--- Tokenization ---");
    }
    let tokenizer = Tokenizer::new(&source_code);
    if debug_enabled {
        println!("{:#?}", tokenizer);
        println!("\n--- Parsing ---");
    }
    let mut parser = Parser::new(tokenizer);
    let program = parser.parse_program();

    if !parser.errors().is_empty() {
        println!("\n--- Parse Errors ---");
        for error in parser.errors() {
            println!("{:?}", error);
        }
        println!("--------------------");
        // Don't proceed to interpretation if parsing failed
        process::exit(1); // Exit with error code if parsing fails
    }

    if debug_enabled {
        println!("\n--- AST ---");
        println!("{:#?}", program);
    }

    if debug_enabled { println!("\n--- Interpretation --- "); }
    let mut interpreter = Interpreter::new();
    let start_time = std::time::Instant::now();
    match interpreter.interpret(program) {
        Ok(_) => {
            let end_time = std::time::Instant::now();
            let duration = end_time.duration_since(start_time);
            if debug_enabled {
                println!("\n--- Execution Finished ---");
                println!("Execution time: {:?}", duration);
            } else {
                println!("Execution time: {:?}", duration);
            }
        }
        Err(errors) => {
            println!("\n--- Runtime Errors ---");
            for error in errors {
                println!("{}", error); // Use Display for runtime errors
            }
            println!("----------------------");
        }
    }
}
