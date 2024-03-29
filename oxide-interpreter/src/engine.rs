use std::error;

use oxide_parser::{Ast, Lexer, Parser};

use crate::env::Env;
use crate::interpreter::Interpreter;
use crate::val::Val;
use crate::{StdStreamProvider, StreamProvider};

type ErrorHandler = fn(&[Box<dyn error::Error>]) -> !;

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
            (self.on_error)(&Self::box_errors(errors.to_vec()));
        }

        let mut parser = Parser::new(tokens.to_vec());
        let ast = match parser.parse() {
            Ok(ast) => ast,
            Err(errors) => {
                (self.on_error)(&Self::box_errors(errors.to_vec()));
            }
        };

        ast
    }

    pub fn run(&self, ast: &Ast, argv: &[String], streams: Option<Box<dyn StreamProvider>>) -> Val {
        let streams = streams.unwrap_or_else(|| Box::new(StdStreamProvider::new(None)));
        let mut inter = Interpreter::new(Env::new_stdlib(), streams, argv);

        self.run_code(ast, &mut inter)
    }

    fn run_code(&self, ast: &Ast, inter: &mut Interpreter) -> Val {
        match inter.interpret(ast) {
            Ok(val) => val,
            Err(e) => {
                (self.on_error)(&Self::box_errors(vec![e]));
            }
        }
    }

    fn box_errors<'a, E: error::Error + 'a>(errs: Vec<E>) -> Vec<Box<dyn error::Error + 'a>> {
        errs.into_iter()
            .map(|e| Box::new(e) as Box<dyn error::Error>)
            .collect()
    }
}
