use std::cell::RefCell;
use std::io::{Read, Write};
use std::rc::Rc;
use std::{error, process};

use crate::interpreter::stdlib::Stdlib;
use crate::interpreter::{Interpreter, RuntimeError};
use crate::lexer::token::token_type::TokenType;
use crate::lexer::token::{Pos, Token};
use crate::lexer::Lexer;
use crate::parser::Parser;

pub mod interpreter;
pub mod lexer;
pub mod parser;

/// Runs code from string.
pub fn run(
    src: String,
    stdout: Option<Rc<RefCell<dyn Write>>>,
    stderr: Option<Rc<RefCell<dyn Write>>>,
    stdin: Option<Rc<RefCell<dyn Read>>>,
    top_level: bool,
) {
    let mut scanner = Lexer::new(src);
    let (tokens, errors) = scanner.tokenize();

    if !errors.is_empty() {
        for error in errors {
            eprint_error(error);
        }
        process::exit(1);
    }

    let mut parser = Parser::new(tokens.to_vec());
    let stmts = match parser.parse() {
        Ok(stmts) => stmts,
        Err(errors) => {
            for error in errors {
                eprint_error(error);
            }
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

    let mut interpreter = if top_level {
        Interpreter::top_level(lib, stdout, stderr, stdin)
    } else {
        Interpreter::new(lib, stdout, stderr, stdin)
    };
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
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

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
    let err_token = if !err_token.is_empty() {
        format!("\"{}\"", err_token)
    } else {
        err_token.to_string()
    };

    let pos = if let Some(pos) = pos {
        format!(" at [{}:{}]", pos.0, pos.1)
    } else {
        "".to_string()
    };

    eprintln!("\x1b[0;31mError {}: {}{}.\x1b[0m", err_token, message, pos);
}

fn eprint_error(err: impl error::Error) {
    eprintln!("\x1b[0;31m{}\x1b[0m", err);
}
