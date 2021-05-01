use std::cell::RefCell;
use std::io::{Read, Write};
use std::rc::Rc;
use std::{error, process};

use crate::interpreter::stdlib::Stdlib;
use crate::interpreter::Interpreter;
use crate::lexer::token::Token;
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
            eprint_error(e);
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
            eprint_error(e);
            process::exit(1);
        }
    };
}

/// Language version is taken from Cargo.toml.
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

fn eprint_error(err: impl error::Error) {
    eprintln!("\x1b[0;31m{}\x1b[0m", err);
}
