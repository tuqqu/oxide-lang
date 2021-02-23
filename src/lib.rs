use std::cell::RefCell;
use std::fs;
use std::io::{self, Read, Write};
use std::process;
use std::rc::Rc;

use crate::interpreter::stdlib::Stdlib;
use crate::interpreter::Interpreter;
use crate::lexer::token::{Token, TokenType};
use crate::lexer::Lexer;
use crate::parser::Parser;

mod interpreter;
mod lexer;
mod parser;

pub fn run_file(path: String) {
    run_file_with_streams(path, None, None, None)
}

pub fn run_file_with_streams(
    path: String,
    stdout: Option<Rc<RefCell<dyn Write>>>,
    stderr: Option<Rc<RefCell<dyn Write>>>,
    stdin: Option<Rc<RefCell<dyn Read>>>,
) {
    let stdout = stdout.unwrap_or(Rc::new(RefCell::new(std::io::stdout())));
    let stderr = stderr.unwrap_or(Rc::new(RefCell::new(std::io::stderr())));
    let stdin = stdin.unwrap_or(Rc::new(RefCell::new(std::io::stdin())));

    let contents = fs::read_to_string(path).expect("Something went wrong reading the file");
    run(contents, stdout, stderr, stdin);
}

pub fn run_repl() {
    let stdin = io::stdin();

    loop {
        print!("> ");
        io::stdout().flush().expect("Error while stdout flushing.");
        let mut line: String = String::new();
        stdin
            .read_line(&mut line)
            .expect("Error while line reading.");

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
            if e.token.is_some() {
                error_token(&e.token.unwrap(), e.msg);
            } else {
                error(0, e.msg);
            }
            process::exit(1);
        }
    };

    let mut interpreter = Interpreter::new(lib, stdout, stderr, stdin);
    let res = interpreter.interpret(&stmts);

    match res {
        Ok(_) => {}
        Err(e) => {
            if e.token.is_some() {
                error_token(&e.token.unwrap(), e.msg);
            } else {
                error(0, e.msg);
            }
            process::exit(1);
        }
    };
}

fn error(line: usize, msg: String) {
    print_error(line, "".to_string(), msg);
}

fn error_token(token: &Token, msg: String) {
    print_error(token.line, if token.token_type != TokenType::Eof { token.lexeme.clone() } else { "".to_string() }, msg);
}

fn print_error(line: usize, err_token: String, message: String) {
    eprintln!(
        "\x1b[0;31mError{}: {} at line {}\x1b[0m",
        err_token, message, line
    );
}
