use std::{error, process};

use crate::interpreter::env::Env;
use crate::interpreter::stdlib::Stdlib;
use crate::interpreter::val::Val;
use crate::interpreter::{Interpreter, StdStreams};
use crate::lexer::Lexer;
use crate::parser::{Ast, Parser};

pub struct Engine;

impl Engine {
    /// Language version is taken from Cargo.toml.
    pub const VERSION: &'static str = env!("CARGO_PKG_VERSION");

    pub fn ast(src: String) -> Ast {
        let mut scanner = Lexer::new(src);
        let (tokens, errors) = scanner.tokenize();

        if !errors.is_empty() {
            for error in errors {
                eprint_error(error);
            }
            process::exit(1);
        }

        let mut parser = Parser::new(tokens.to_vec());
        let ast = match parser.parse() {
            Ok(ast) => ast,
            Err(errors) => {
                for error in errors {
                    eprint_error(error);
                }
                process::exit(1);
            }
        };

        ast
    }

    pub fn run(ast: &Ast, streams: Option<StdStreams>) -> Val {
        let mut i = Interpreter::new(Self::provide_stdlib(), streams);
        Self::run_code(ast, &mut i)
    }

    pub fn run_top_level(ast: &Ast, streams: Option<StdStreams>) -> Val {
        let mut i = Interpreter::top_level(Self::provide_stdlib(), streams);
        Self::run_code(ast, &mut i)
    }

    fn run_code(ast: &Ast, i: &mut Interpreter) -> Val {
        match i.interpret(ast) {
            Ok(val) => val,
            Err(e) => {
                eprint_error(e);
                process::exit(1);
            }
        }
    }

    fn provide_stdlib() -> Env {
        let lib = Stdlib::env();
        match lib {
            Ok(lib) => lib,
            Err(e) => {
                eprint_error(e);
                process::exit(1);
            }
        }
    }
}

fn eprint_error(err: impl error::Error) {
    eprintln!("\x1b[0;31m{}\x1b[0m", err);
}
