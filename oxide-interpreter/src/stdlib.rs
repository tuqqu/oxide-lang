#![allow(clippy::ptr_arg)]

use std::fs::File;
use std::io;
use std::io::Write;
use std::sync::Arc;
use std::time::{SystemTime, UNIX_EPOCH};

use oxide_parser::valtype::Generics;
use oxide_parser::{Token, TokenPos, TokenType, ValType};

use self::builtin::BuiltinFn;
use crate::env::{self, Env};
use crate::interpreter::{InterpretedResult, Interpreter};
use crate::val::{Callable, Val};

impl Env {
    pub fn new_stdlib() -> Self {
        use ValType::*;
        let mut lib = Self::new();

        lib.define_std_function("timestamp", builtin::timestamp, vec![], Int);
        lib.define_std_function("dbg", builtin::dbg, vec![Any], Nil);
        lib.define_std_function("println", builtin::println, vec![Str], Nil);
        lib.define_std_function("print", builtin::print, vec![Str], Nil);
        lib.define_std_function("eprintln", builtin::eprintln, vec![Str], Nil);
        lib.define_std_function("eprint", builtin::eprint, vec![Str], Nil);
        lib.define_std_function("file_write", builtin::file_write, vec![Str, Str], Nil);
        lib.define_std_function("read_line", builtin::read_line, vec![Str], Nil);
        lib.define_std_function("typeof", builtin::typeof_, vec![Any], Str);
        lib.define_std_function("args", builtin::args, vec![], Vec(Generics::new(vec![Str])));

        lib
    }

    fn define_std_function(
        &mut self,
        name: &str,
        callable: BuiltinFn,
        param_types: Vec<ValType>,
        ret_type: ValType,
    ) {
        let token = Token::new(
            TokenType::Identifier,
            name.to_string(),
            String::from(""),
            TokenPos(0, 0),
        );
        self.define_function(env::Function::without_struct(
            token,
            Val::Callable(*Callable::new(param_types, ret_type, Arc::new(callable))),
        ))
        .unwrap();
    }
}

/// Std library
mod builtin {
    use super::*;

    pub(super) type BuiltinFn = fn(&mut Interpreter, &Vec<Val>) -> InterpretedResult<Val>;

    /// Returns the current Unix Epoch timestamp as an integer.
    ///
    /// `fn timestamp() -> int;`
    pub fn timestamp(_inter: &mut Interpreter, _args: &Vec<Val>) -> InterpretedResult<Val> {
        let since_the_epoch = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("Error while trying to retrieve current timestamp.");

        Ok(Val::Float(since_the_epoch.as_secs_f64()))
    }

    /// Dumps the value to stdout.
    ///
    /// `fn dbg(value: any);`
    pub fn dbg(inter: &mut Interpreter, args: &Vec<Val>) -> InterpretedResult<Val> {
        let value = args
            .first()
            .expect("Error while trying to retrieve function arguments.");

        writeln!(inter.stdout.borrow_mut(), "{}", value.debug())
            .expect("Error while trying to write to the output stream.");

        Ok(Val::Nil)
    }

    /// Prints the string to stdout with a newline.
    ///
    /// `fn println(message: str);`
    pub fn println(inter: &mut Interpreter, args: &Vec<Val>) -> InterpretedResult<Val> {
        let value = args
            .first()
            .expect("Error while trying to retrieve function arguments.");

        writeln!(inter.stdout.borrow_mut(), "{}", value.as_string()?)
            .expect("Error while trying to write to the output stream.");
        Ok(Val::Nil)
    }

    /// Prints the string to stdout
    ///
    /// `fn print(message: str);`
    pub fn print(inter: &mut Interpreter, args: &Vec<Val>) -> InterpretedResult<Val> {
        let value = args
            .first()
            .expect("Error while trying to retrieve function arguments.");

        write!(inter.stdout.borrow_mut(), "{}", value.as_string()?)
            .expect("Error while trying to write to the output stream.");
        Ok(Val::Nil)
    }

    /// Prints the string to stderr with a newline.
    ///
    /// `fn eprintln(message: str);`
    pub(crate) fn eprintln(inter: &mut Interpreter, args: &Vec<Val>) -> InterpretedResult<Val> {
        let value = args
            .first()
            .expect("Error while trying to retrieve function arguments.");

        writeln!(inter.stderr.borrow_mut(), "{}", value.as_string()?)
            .expect("Error while trying to write to the output stream.");
        Ok(Val::Nil)
    }

    /// Prints the string to stdout
    ///
    /// `fn print(message: str);`
    pub fn eprint(inter: &mut Interpreter, args: &Vec<Val>) -> InterpretedResult<Val> {
        let value = args
            .first()
            .expect("Error while trying to retrieve function arguments.");

        write!(inter.stdout.borrow_mut(), "{}", value.as_string()?)
            .expect("Error while trying to write to the output stream.");
        Ok(Val::Nil)
    }

    /// Writes `contents` to a `file_path`. If no file exists, creates the file.
    ///
    /// `fn file_write(file_path: str, contents: str);`
    pub fn file_write(_inter: &mut Interpreter, args: &Vec<Val>) -> InterpretedResult<Val> {
        if let [file_name, content] = &args[..] {
            let (file_name, content) = match (file_name, content) {
                (Val::Str(file_name), Val::Str(content)) => (file_name, content),
                _ => return Ok(Val::Nil),
            };

            let file = File::create(file_name);
            let mut file = if let Ok(file) = file {
                file
            } else {
                return Ok(Val::Nil);
            };

            let res = file.write_all(content.as_bytes());

            if res.is_err() {
                return Ok(Val::Nil);
            }
        }

        Ok(Val::Nil)
    }

    /// Prints prompt and waits for an input from stdin.
    ///
    /// `fn read_line(prompt: str);`
    pub fn read_line(_inter: &mut Interpreter, args: &Vec<Val>) -> InterpretedResult<Val> {
        let prompt = match args.first().unwrap() {
            Val::Str(prompt) => prompt,
            _ => return Ok(Val::Nil),
        };

        let stdin = io::stdin();
        print!("{}", prompt);
        io::stdout().flush().expect("Error while stdout flushing.");
        let mut line: String = String::new();
        stdin
            .read_line(&mut line)
            .expect("Error while line reading.");

        Ok(Val::Str(line))
    }

    /// Returns type of a given value.
    ///
    /// `fn typeof(value: any) -> str;`
    pub fn typeof_(_inter: &mut Interpreter, args: &Vec<Val>) -> InterpretedResult<Val> {
        let value = args.first().expect("Cannot retrieve function argument.");

        Ok(Val::Str(value.get_type()))
    }

    /// Returns an array of arguments passed to script.
    ///
    /// `fn args() -> vec<str>;`
    pub fn args(inter: &mut Interpreter, _args: &Vec<Val>) -> InterpretedResult<Val> {
        Ok(Val::new_vec_instance(inter.args(), ValType::Str))
    }
}
