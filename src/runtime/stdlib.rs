use crate::interpret::{Value, RuntimeError, environment::Environment};
use crate::interpret::value::{Function, StructInstanceValue, BuiltinId};
use crate::lexer::astgen::Identifier;
use std::io::{self, Write, Read};
use std::net::{TcpStream, TcpListener};
use std::any::Any;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

/// Built-in `print` function.
/// Takes any number of arguments, converts them to strings,
/// joins them with spaces, and prints to stdout followed by a newline.
pub fn builtin_println(args: Vec<Value>) -> Result<Value, RuntimeError> {
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

/// New function: prints without a newline
pub fn builtin_print(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let mut output = String::new();
    for (i, arg) in args.iter().enumerate() {
        if i > 0 {
            output.push(' ');
        }
        output.push_str(&arg.to_string());
    }
    print!("{}", output); // Use print! macro
    io::stdout().flush().map_err(|e| RuntimeError::InvalidOperation(format!("Failed to flush stdout: {}", e)))?;
    Ok(Value::Null)
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
        Value::Dict(map) => Ok(Value::Int(map.len() as i64)),
        _ => Err(RuntimeError::TypeError(format!(
            "len() expects a string, list, or dict, got {}",
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
        Value::Dict(_) => "dict",
        Value::StructInstance(inst) => inst.type_name.name.as_str(),
        Value::BoundMethod(_) => "bound method",
        Value::NativeResource(_) => "native resource",
    };

    Ok(Value::String(std::borrow::Cow::Owned(type_str.to_string())))
}
/*
// --- Networking Built-ins ---

// Connects to a TCP server
pub fn builtin_tcp_connect(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::TypeError("tcp_connect expects 2 arguments (address: string, port: int)".to_string()));
    }
    let address = match &args[0] {
        Value::String(s) => s,
        _ => return Err(RuntimeError::TypeError("tcp_connect argument 1 must be a string address".to_string())),
    };
    let port = match &args[1] {
        Value::Int(i) => *i as u16, // TODO: Add range check for port?
        _ => return Err(RuntimeError::TypeError("tcp_connect argument 2 must be an integer port".to_string())),
    };

    let addr_str = format!("{}:{}", address, port);
    match TcpStream::connect(&addr_str) {
        Ok(stream) => {
            // Wrap the TcpStream in Rc<RefCell<dyn Any>>
            let resource: Rc<RefCell<dyn Any>> = Rc::new(RefCell::new(stream));
            Ok(Value::NativeResource(resource))
        }
        Err(e) => {
            // Instead of returning RuntimeError, return Ok(Value::Null) for common connection errors
            // This allows the Oxygen script to handle failed connection attempts gracefully.
            eprintln!("[Network Info] Connection attempt to {} failed: {}", addr_str, e);
            Ok(Value::Null) // Indicate failure without halting the script
        }
    }
}

// Connects to a TCP server with a specified timeout
pub fn builtin_tcp_connect_with_timeout(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 3 {
        return Err(RuntimeError::TypeError("__tcp_connect_with_timeout expects 3 arguments (address: string, port: int, timeout_ms: int)".to_string()));
    }
    let address = match &args[0] {
        Value::String(s) => s,
        _ => return Err(RuntimeError::TypeError("tcp_connect_timeout argument 1 must be a string address".to_string())),
    };
    let port = match &args[1] {
        Value::Int(i) => *i as u16,
        _ => return Err(RuntimeError::TypeError("tcp_connect_timeout argument 2 must be an integer port".to_string())),
    };
    let timeout_ms = match &args[2] {
         Value::Int(i) if *i > 0 => *i as u64,
         _ => return Err(RuntimeError::TypeError("tcp_connect_timeout argument 3 must be a positive integer timeout in milliseconds".to_string())),
    };

    let addr_str = format!("{}:{}", address, port);
    let timeout_duration = std::time::Duration::from_millis(timeout_ms);

    match TcpStream::connect_timeout(&addr_str.parse().map_err(|_| RuntimeError::InvalidOperation("Failed to parse socket address".to_string()))?, timeout_duration) {
        Ok(stream) => {
            let resource: Rc<RefCell<dyn Any>> = Rc::new(RefCell::new(stream));
            Ok(Value::NativeResource(resource))
        }
        Err(e) => {
            // Return Null on expected connection failures (timeout, refused, etc.)
            // eprintln!("[Network Info] Connection attempt ({}ms timeout) to {} failed: {}", timeout_ms, addr_str, e);
            Ok(Value::Null)
        }
    }
}

// Writes data to a native resource (expected to be TcpStream)
pub fn builtin_socket_write(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::TypeError("socket_write expects 2 arguments (resource, data: string)".to_string()));
    }
    let resource_val = match &args[0] {
        Value::NativeResource(res) => Rc::clone(res),
        _ => return Err(RuntimeError::TypeError("socket_write argument 1 must be a native resource".to_string())),
    };
    let data = match &args[1] {
        Value::String(s) => s.as_bytes(),
        _ => return Err(RuntimeError::TypeError("socket_write argument 2 must be a string".to_string())),
    };

    // Attempt to downcast and write
    let mut borrowed_resource = resource_val.borrow_mut();
    if let Some(stream) = borrowed_resource.downcast_mut::<TcpStream>() {
        match stream.write(data) {
            Ok(bytes_written) => Ok(Value::Int(bytes_written as i64)),
            Err(e) => Err(RuntimeError::InvalidOperation(format!("Socket write error: {}", e)))
        }
    } else {
        Err(RuntimeError::TypeError("socket_write expects a TcpStream resource".to_string()))
    }
}

// Reads data from a native resource (expected to be TcpStream)
pub fn builtin_socket_read(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::TypeError("socket_read expects 2 arguments (resource, buffer_size: int)".to_string()));
    }
     let resource_val = match &args[0] {
        Value::NativeResource(res) => Rc::clone(res),
        _ => return Err(RuntimeError::TypeError("socket_read argument 1 must be a native resource".to_string())),
    };
    let buffer_size = match &args[1] {
        Value::Int(i) if *i > 0 => *i as usize,
        _ => return Err(RuntimeError::TypeError("socket_read argument 2 must be a positive integer buffer size".to_string())),
    };

    // Attempt to downcast and read
    let mut borrowed_resource = resource_val.borrow_mut();
    if let Some(stream) = borrowed_resource.downcast_mut::<TcpStream>() {
        let mut buffer = vec![0; buffer_size];
        match stream.read(&mut buffer) {
            Ok(bytes_read) => {
                // Convert read bytes to string (potentially lossy)
                buffer.truncate(bytes_read); // Important: only use read bytes
                let result_string = String::from_utf8_lossy(&buffer).to_string();
                Ok(Value::String(std::borrow::Cow::Owned(result_string)))
            }
            Err(e) => Err(RuntimeError::InvalidOperation(format!("Socket read error: {}", e)))
        }
    } else {
        Err(RuntimeError::TypeError("socket_read expects a TcpStream resource".to_string()))
    }
}

// Closes a native resource (expected to be TcpStream)
// TcpStream closes on drop, but explicit close might be useful?
// For now, we don't need an explicit built-in, drop handles it.
// fn builtin_socket_close(args: Vec<Value>) -> Result<Value, RuntimeError> { ... }

// Basic HTTP GET request using TcpStream
pub fn builtin_http_get(args: Vec<Value>) -> Result<Value, RuntimeError> {
     if args.len() != 1 {
        return Err(RuntimeError::TypeError("http_get expects 1 argument (url: string)".to_string()));
    }
    let url_str = match &args[0] {
        Value::String(s) => s,
        _ => return Err(RuntimeError::TypeError("http_get argument 1 must be a string URL".to_string())),
    };

    // VERY basic URL parsing (no external crates like `url`)
    let (host, raw_path_part) = match url_str.strip_prefix("http://") {
        Some(rest) => rest.split_once('/').unwrap_or((rest, "")), // Get host and whatever is after first '/'
        None => return Err(RuntimeError::InvalidOperation("http_get only supports http:// URLs".to_string())),
    };
    // Ensure the path always starts with a forward slash
    let path = format!("/{}", raw_path_part);

    let port = 80;
    let addr_str = format!("{}:{}", host, port);

    match TcpStream::connect(&addr_str) {
        Ok(mut stream) => {
            // Construct basic HTTP/1.0 GET request
            let request = format!(
                "GET {} HTTP/1.0\r\nHost: {}\r\nConnection: close\r\n\r\n",
                path, host
            );

            if let Err(e) = stream.write_all(request.as_bytes()) {
                 return Err(RuntimeError::InvalidOperation(format!("HTTP request write error: {}", e)));
            }

            // Read the response into a buffer
            let mut buffer = Vec::new();
            if let Err(e) = stream.read_to_end(&mut buffer) { // Read whole response
                 return Err(RuntimeError::InvalidOperation(format!("HTTP response read error: {}", e)));
            }

            // --- Basic HTTP Response Parsing ---
            let mut status_code = 0;
            let mut body_start_index = 0;
            let mut headers_processed = false;

            // Find header/body split (double CRLF)
            for i in 0..buffer.len().saturating_sub(3) {
                if buffer[i..i+4] == *b"\r\n\r\n" {
                    body_start_index = i + 4;
                    headers_processed = true;
                    break;
                }
            }

            if !headers_processed {
                 return Err(RuntimeError::InvalidOperation("Failed to parse HTTP response: No header/body separator found".to_string()));
            }

            // Parse status line (e.g., "HTTP/1.0 200 OK")
            if let Some(first_line_end) = buffer[..body_start_index].windows(2).position(|w| w == b"\r\n") {
                let status_line = String::from_utf8_lossy(&buffer[..first_line_end]);
                let parts: Vec<&str> = status_line.splitn(3, ' ').collect();
                if parts.len() >= 2 {
                    if let Ok(code) = parts[1].parse::<i64>() {
                        status_code = code;
                    }
                }
            } else {
                return Err(RuntimeError::InvalidOperation("Failed to parse HTTP status line".to_string()));
            }

            // Extract body
            let body_bytes = &buffer[body_start_index..];
            let body_string = String::from_utf8_lossy(body_bytes).to_string();

            // TODO: Parse headers into a Dict when available

            // Create the StructInstanceValue for HttpResponse
            let mut fields = HashMap::new();
            fields.insert("status_code".to_string(), Value::Int(status_code));
            fields.insert("body".to_string(), Value::String(std::borrow::Cow::Owned(body_string)));

            Ok(Value::StructInstance(StructInstanceValue {
                // Assume HttpResponse struct name is available globally or via lookup?
                // For simplicity, hardcode it here - needs improvement
                type_name: Identifier { name: "HttpResponse".to_string() },
                fields: Rc::new(RefCell::new(fields)),
            }))
        }
        Err(e) => Err(RuntimeError::InvalidOperation(format!(
            "Failed to connect to {}: {}", addr_str, e
        )))
    }
}

// Binds a TCP listener to an address and port
// Returns a NativeResource containing the TcpListener
pub fn builtin_tcp_listener_bind(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::TypeError("tcp_bind expects 2 arguments (address: string, port: int)".to_string()));
    }
    let address = match &args[0] {
        Value::String(s) => s,
        _ => return Err(RuntimeError::TypeError("tcp_bind argument 1 must be a string address".to_string())),
    };
    let port = match &args[1] {
        Value::Int(i) => *i as u16,
        _ => return Err(RuntimeError::TypeError("tcp_bind argument 2 must be an integer port".to_string())),
    };

    let addr_str = format!("{}:{}", address, port);
    match TcpListener::bind(&addr_str) {
        Ok(listener) => {
            let resource: Rc<RefCell<dyn Any>> = Rc::new(RefCell::new(listener));
            Ok(Value::NativeResource(resource))
        }
        Err(e) => Err(RuntimeError::InvalidOperation(format!(
            "Failed to bind listener to {}: {}", addr_str, e
        )))
    }
}

// Accepts a connection on a TcpListener resource
// Returns a List containing [TcpStreamResource, peer_address_string]
pub fn builtin_tcp_listener_accept(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::TypeError("tcp_accept expects 1 argument (listener_resource)".to_string()));
    }
    let resource_val = match &args[0] {
        Value::NativeResource(res) => Rc::clone(res),
        _ => return Err(RuntimeError::TypeError("tcp_accept argument 1 must be a native listener resource".to_string())),
    };

    // Attempt to downcast to TcpListener and accept
    let borrowed_resource = resource_val.borrow();
    if let Some(listener) = borrowed_resource.downcast_ref::<TcpListener>() {
        match listener.accept() {
            Ok((stream, socket_addr)) => {
                // Wrap the new TcpStream
                let stream_resource: Rc<RefCell<dyn Any>> = Rc::new(RefCell::new(stream));
                let stream_val = Value::NativeResource(stream_resource);
                // Get peer address as string
                let addr_val = Value::String(std::borrow::Cow::Owned(socket_addr.to_string()));
                // Return as a list [stream, address]
                Ok(Value::List(vec![stream_val, addr_val]))
            }
            Err(e) => Err(RuntimeError::InvalidOperation(format!("Socket accept error: {}", e)))
        }
    } else {
        Err(RuntimeError::TypeError("tcp_accept expects a TcpListener resource".to_string()))
    }
}
*/

// String trim_end implementation
pub fn builtin_string_trim_end(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::TypeError("trim_end expects 2 arguments (string, pattern)".to_string()));
    }
    let s = match &args[0] {
        Value::String(s_val) => s_val,
        _ => return Err(RuntimeError::TypeError("trim_end argument 1 must be a string".to_string())),
    };
    let pattern = match &args[1] {
        Value::String(p_val) => p_val,
        _ => return Err(RuntimeError::TypeError("trim_end argument 2 must be a string pattern".to_string())),
    };

    // Perform the trim
    // Note: Rust's trim_end_matches trims *all* characters from the pattern, not the pattern as a whole sequence.
    // This matches typical trim behavior.
    let trimmed_s = s.trim_end_matches(|c| pattern.contains(c));

    Ok(Value::String(std::borrow::Cow::Owned(trimmed_s.to_string())))
}

// Generic to_string implementation
pub fn builtin_to_string(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::TypeError("to_string expects 1 argument".to_string()));
    }
    // Use the Display implementation of the Value enum
    let s = format!("{}", args[0]);
    Ok(Value::String(std::borrow::Cow::Owned(s)))
}

/// Register all stdlib functions in an Interpreter environment.
pub fn register_stdlib(env: &mut crate::interpret::environment::Environment) {
    use crate::interpret::value::Function;
    use std::rc::Rc;
    use std::cell::RefCell;

    // Combine Rust built-ins and placeholders for Oxygen functions
    let builtins: &[(&str, BuiltinId)] = &[
        ("print", BuiltinId::Print),
        ("println", BuiltinId::Println),
        ("len", BuiltinId::Len),
        ("type", BuiltinId::Type),
        ("to_string", BuiltinId::ToString),
        /*
        ("__tcp_connect", BuiltinId::TcpConnect),
        ("__tcp_connect_with_timeout", BuiltinId::TcpConnectWithTimeout),
        ("__socket_write", BuiltinId::SocketWrite),
        ("__socket_read", BuiltinId::SocketRead),
        ("__http_get", BuiltinId::HttpGet),
        ("__tcp_bind", BuiltinId::TcpBind),
        ("__tcp_accept", BuiltinId::TcpAccept),
        ("__udp_bind", BuiltinId::UdpBind),
        ("__udp_send_to", BuiltinId::UdpSendTo),
        ("__udp_recv_from", BuiltinId::UdpRecvFrom),
        */
        ("__string_trim_end", BuiltinId::StringTrimEnd),
        ("__to_string", BuiltinId::ToString),
    ];

    for (name, id) in builtins.iter() {
        env.define(
            (*name).to_string(),
            Value::Function(Box::new(Function {
                name: (*name).to_string(),
                parameters: vec![],
                body: None,
                env: Rc::new(RefCell::new(crate::interpret::environment::Environment::new())),
                is_builtin: true,
                builtin_id: Some(*id),
            }))
        );
    }
}

/*
// --- UDP Built-ins ---

// Binds a UDP socket to an address and port
pub fn builtin_udp_bind(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::TypeError("__udp_bind expects 2 arguments (address: string, port: int)".to_string()));
    }
    let address = match &args[0] {
        Value::String(s) => s,
        _ => return Err(RuntimeError::TypeError("udp_bind argument 1 must be a string address".to_string())),
    };
    let port = match &args[1] {
        Value::Int(i) => *i as u16,
        _ => return Err(RuntimeError::TypeError("udp_bind argument 2 must be an integer port".to_string())),
    };

    let addr_str = format!("{}:{}", address, port);
    match std::net::UdpSocket::bind(&addr_str) {
        Ok(socket) => {
            let resource: Rc<RefCell<dyn Any>> = Rc::new(RefCell::new(socket));
            Ok(Value::NativeResource(resource))
        }
        Err(e) => Err(RuntimeError::InvalidOperation(format!(
            "Failed to bind UDP socket to {}: {}", addr_str, e
        )))
    }
}

// Sends data from a UDP socket resource to a specified address
pub fn builtin_udp_send_to(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 3 {
        return Err(RuntimeError::TypeError("__udp_send_to expects 3 arguments (socket_resource, data: string, target_addr: string)".to_string()));
    }
    let resource_val = match &args[0] {
        Value::NativeResource(res) => Rc::clone(res),
        _ => return Err(RuntimeError::TypeError("udp_send_to argument 1 must be a native UDP socket resource".to_string())),
    };
    let data = match &args[1] {
        Value::String(s) => s.as_bytes(),
        _ => return Err(RuntimeError::TypeError("udp_send_to argument 2 must be a string".to_string())),
    };
    let target_addr = match &args[2] {
        Value::String(s) => s.as_ref(),
        _ => return Err(RuntimeError::TypeError("udp_send_to argument 3 must be a string address (e.g., \"host:port\")".to_string())),
    };

    // Attempt to downcast and send
    let borrowed_resource = resource_val.borrow();
    if let Some(socket) = borrowed_resource.downcast_ref::<std::net::UdpSocket>() {
        match socket.send_to(data, target_addr) {
            Ok(bytes_sent) => Ok(Value::Int(bytes_sent as i64)),
            Err(e) => Err(RuntimeError::InvalidOperation(format!("UDP send_to error: {}", e)))
        }
    } else {
        Err(RuntimeError::TypeError("udp_send_to expects a UdpSocket resource".to_string()))
    }
}

// Receives data on a UDP socket resource, returns [data_string, sender_addr_string]
pub fn builtin_udp_recv_from(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::TypeError("__udp_recv_from expects 2 arguments (socket_resource, buffer_size: int)".to_string()));
    }
     let resource_val = match &args[0] {
        Value::NativeResource(res) => Rc::clone(res),
        _ => return Err(RuntimeError::TypeError("udp_recv_from argument 1 must be a native UDP socket resource".to_string())),
    };
    let buffer_size = match &args[1] {
        Value::Int(i) if *i > 0 => *i as usize,
        _ => return Err(RuntimeError::TypeError("udp_recv_from argument 2 must be a positive integer buffer size".to_string())),
    };

    // Attempt to downcast and receive
    let borrowed_resource = resource_val.borrow();
    if let Some(socket) = borrowed_resource.downcast_ref::<std::net::UdpSocket>() {
        let mut buffer = vec![0; buffer_size];
        match socket.recv_from(&mut buffer) {
            Ok((bytes_read, src_addr)) => {
                // Convert read bytes to string (potentially lossy)
                buffer.truncate(bytes_read); 
                let data_string = String::from_utf8_lossy(&buffer).to_string();
                let sender_addr_string = src_addr.to_string();
                Ok(Value::List(vec![
                    Value::String(std::borrow::Cow::Owned(data_string)),
                    Value::String(std::borrow::Cow::Owned(sender_addr_string)),
                ]))
            }
            Err(e) => Err(RuntimeError::InvalidOperation(format!("UDP recv_from error: {}", e)))
        }
    } else {
        Err(RuntimeError::TypeError("udp_recv_from expects a UdpSocket resource".to_string()))
    }
}
*/

// TODO: Add other built-in functions here (e.g., len, type, input, file I/O, etc.) 