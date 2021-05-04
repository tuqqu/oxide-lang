use std::error;

use crate::interpreter::env::Env;
use crate::interpreter::stdlib::Stdlib;
use crate::interpreter::val::Val;
use crate::interpreter::{Interpreter, StdStreams};
use crate::lexer::Lexer;
use crate::parser::{Ast, Parser};

type ErrorHandler = fn(Vec<Box<dyn error::Error>>) -> !;

pub struct Engine {
    on_error: ErrorHandler,
}

impl Engine {
    /// Language version is taken from Cargo.toml.
    pub const VERSION: &'static str = env!("CARGO_PKG_VERSION");

    pub fn new(on_error: ErrorHandler) -> Self {
        Self { on_error }
    }

    pub fn ast(&self, src: String) -> Ast {
        let mut lexer = Lexer::new(src);
        let (tokens, errors) = lexer.tokenize();
        if !errors.is_empty() {
            (self.on_error)(Self::box_errors(errors.to_vec()));
        }

        let mut parser = Parser::new(tokens.to_vec());
        let ast = match parser.parse() {
            Ok(ast) => ast,
            Err(errors) => {
                (self.on_error)(Self::box_errors(errors.to_vec()));
            }
        };

        ast
    }

    pub fn run(&self, ast: &Ast, streams: Option<StdStreams>) -> Val {
        let mut i = Interpreter::new(self.provide_stdlib(), streams);
        self.run_code(ast, &mut i)
    }

    fn run_code(&self, ast: &Ast, i: &mut Interpreter) -> Val {
        match i.interpret(ast) {
            Ok(val) => val,
            Err(e) => {
                (self.on_error)(Self::box_errors(vec![e]));
            }
        }
    }

    fn provide_stdlib(&self) -> Env {
        let lib = Stdlib::env();
        match lib {
            Ok(lib) => lib,
            Err(e) => {
                (self.on_error)(Self::box_errors(vec![e]));
            }
        }
    }

    fn box_errors<'a, E: error::Error + 'a>(errs: Vec<E>) -> Vec<Box<dyn error::Error + 'a>> {
        errs.into_iter()
            .map(|e| Box::new(e) as Box<dyn error::Error>)
            .collect()
    }
}
