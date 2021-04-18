use std::cell::RefCell;
use std::fs;
use std::io::{self, Read, Write};
use std::process;
use std::rc::Rc;

use crate::interpreter::stdlib::Stdlib;
use crate::interpreter::{Interpreter, RuntimeError};
use crate::lexer::token::{Pos, Token, TokenType};
use crate::lexer::Lexer;
use crate::parser::Parser;

mod analyser;
mod interpreter;
mod lexer;
mod parser;

/// Runs script from file.
pub fn run_file(path: String) {
    run_file_with_streams(path, None, None, None);
}

/// Runs file with changed std streams.
///
/// Primarily used by tests to capture output, although there is nothing specific
/// to tests. Can be used to run scripts and prevent and/or capture output.
pub fn run_file_with_streams(
    path: String,
    stdout: Option<Rc<RefCell<dyn Write>>>,
    stderr: Option<Rc<RefCell<dyn Write>>>,
    stdin: Option<Rc<RefCell<dyn Read>>>,
) {
    let stdout = stdout.unwrap_or_else(|| Rc::new(RefCell::new(std::io::stdout())));
    let stderr = stderr.unwrap_or_else(|| Rc::new(RefCell::new(std::io::stderr())));
    let stdin = stdin.unwrap_or_else(|| Rc::new(RefCell::new(std::io::stdin())));

    let contents = fs::read_to_string(path).expect("Something went wrong reading the file");
    run(contents, stdout, stderr, stdin);
}

/// Runs REPL mode from stdin.
pub fn run_repl() {
    let stdin = io::stdin();

    loop {
        print!("> ");
        io::stdout()
            .flush()
            .expect("Error while flushing output to stdout.");
        let mut line: String = String::new();
        stdin
            .read_line(&mut line)
            .expect("Error while reading a line.");

        if line == "\n" {
            break;
        }

        run(
            line.to_string(),
            Rc::new(RefCell::new(std::io::stdout())),
            Rc::new(RefCell::new(std::io::stderr())),
            Rc::new(RefCell::new(std::io::stdin())),
        );
    }
}

/// Runs code from string.
fn run(
    src: String,
    stdout: Rc<RefCell<dyn Write>>,
    stderr: Rc<RefCell<dyn Write>>,
    stdin: Rc<RefCell<dyn Read>>,
) {
    let mut scanner = Lexer::new(src);
    let (tokens, invalid) = scanner.tokenize();

    if invalid {
        process::exit(1);
    }

    let mut parser = Parser::new(tokens.clone()); //wut? clone??
    let stmts = match parser.parse() {
        Ok(stmts) => stmts,
        Err(_) => {
            process::exit(1);
        }
    };

    let lib = Stdlib::env();
    let lib = match lib {
        Ok(env) => env,
        Err(e) => {
            error_runtime(&e);
            process::exit(1);
        }
    };

    let mut analyser = analyser::Analyser::new();
    println!("{:?}", analyser.analyse_statements(&stmts));

    let mut interpreter = Interpreter::new(lib, stdout, stderr, stdin);
    let res = interpreter.interpret(&stmts);

    match res {
        Ok(_) => {}
        Err(e) => {
            error_runtime(&e);
            process::exit(1);
        }
    };
}

/// Language version is taken from Cargo.toml.
const VERSION: &str = env!("CARGO_PKG_VERSION");

/// Prints language version.
pub fn print_version() {
    println!("Oxide {}", VERSION);
}

/// Prints error at position, when there is no token.
fn error_at(pos: Pos, msg: &str) {
    print_error("", msg, Some(pos));
}

/// Prints error at token.
fn error_token(token: &Token, msg: &str) {
    print_error(
        if token.token_type != TokenType::Eof {
            &token.lexeme
        } else {
            ""
        },
        msg,
        Some(token.pos),
    );
}

/// Prints error when no exact position is known.
/// Ideally should be avoided, though at times is the only option.
fn error(msg: &str) {
    print_error("", msg, None);
}

/// Internal error printing, should not be used directly.
fn error_runtime(rte: &RuntimeError) {
    if rte.token.is_some() {
        error_token(&rte.token.as_ref().unwrap(), &rte.msg);
    } else if rte.pos.is_some() {
        error_at(rte.pos.unwrap(), &rte.msg);
    } else {
        error(&rte.msg);
    }
}

/// Internal error printing, should not be used directly.
fn print_error(err_token: &str, message: &str, pos: Option<Pos>) {
    let pos = if let Some(pos) = pos {
        format!(" at [{}:{}]", pos.0, pos.1)
    } else {
        "".to_string()
    };

    eprintln!("\x1b[0;31mError {}: {}{}.\x1b[0m", err_token, message, pos);
}
