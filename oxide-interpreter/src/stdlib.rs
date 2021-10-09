#![allow(unused_doc_comments)]

use std::fs::File;
use std::io;
use std::io::Write;
use std::sync::Arc;
use std::time::{SystemTime, UNIX_EPOCH};

use oxide_parser::{Token, TokenPos, TokenType, ValType};

use crate::env::{self, Env};
use crate::interpreter::InterpretedResult;
use crate::val::{Callable, Func, Val};

pub struct Stdlib;

impl Stdlib {
    pub fn env() -> InterpretedResult<Env> {
        let mut std = Env::new();

        /// Returns the current Unix Epoch timestamp as an integer.
        ///
        /// `fn timestamp() -> int;`
        Self::define_function(
            &mut std,
            "timestamp",
            vec![],
            ValType::Int,
            Arc::new(|_, _| {
                let since_the_epoch = SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .expect("Error while trying to retrieve current timestamp.");
                Ok(Val::Float(since_the_epoch.as_secs_f64()))
            }),
        )?;

        /// Dumps the value to stdout.
        ///
        /// `fn dbg(value: any);`
        Self::define_function(
            &mut std,
            "dbg",
            vec![ValType::Any],
            ValType::Nil,
            Arc::new(|inter, args| {
                let value = args
                    .first()
                    .expect("Error while trying to retrieve function arguments.");

                writeln!(inter.stdout.borrow_mut(), "{}", value.debug())
                    .expect("Error while trying to write to the output stream.");
                Ok(Val::Nil)
            }),
        )?;

        /// Prints the string to stdout with a newline.
        ///
        /// `fn println(message: str);`
        Self::define_function(
            &mut std,
            "println",
            vec![ValType::Str],
            ValType::Nil,
            Arc::new(|inter, args| {
                let value = args
                    .first()
                    .expect("Error while trying to retrieve function arguments.");

                writeln!(inter.stdout.borrow_mut(), "{}", value.as_string()?)
                    .expect("Error while trying to write to the output stream.");
                Ok(Val::Nil)
            }),
        )?;

        /// Prints the string to stdout.
        ///
        /// `fn print(message: str);`
        Self::define_function(
            &mut std,
            "print",
            vec![ValType::Str],
            ValType::Nil,
            Arc::new(|inter, args| {
                let value = args
                    .first()
                    .expect("Error while trying to retrieve function arguments.");

                write!(inter.stdout.borrow_mut(), "{}", value.as_string()?)
                    .expect("Error while trying to write to the output stream.");
                Ok(Val::Nil)
            }),
        )?;

        /// Prints the string to stderr with a newline.
        ///
        /// `fn eprintln(message: str);`
        Self::define_function(
            &mut std,
            "eprintln",
            vec![ValType::Str],
            ValType::Nil,
            Arc::new(|inter, args| {
                let value = args
                    .first()
                    .expect("Error while trying to retrieve function arguments.");

                writeln!(inter.stderr.borrow_mut(), "{}", value.as_string()?)
                    .expect("Error while trying to write to the output stream.");
                Ok(Val::Nil)
            }),
        )?;

        /// Prints the string to stderr.
        ///
        /// `fn eprint(message: str);`
        Self::define_function(
            &mut std,
            "eprint",
            vec![ValType::Str],
            ValType::Nil,
            Arc::new(|i, args| {
                let value = args.first().unwrap();

                writeln!(i.stderr.borrow_mut(), "{}", value.as_string()?)
                    .expect("Error while trying to write to the output stream.");
                Ok(Val::Nil)
            }),
        )?;

        /// Writes `contents` to a `file_path`. If no file exists, creates the file.
        ///
        /// `fn file_write(file_path: str, contents: str);`
        Self::define_function(
            &mut std,
            "file_write",
            vec![ValType::Str, ValType::Str],
            ValType::Nil,
            Arc::new(|_, args| {
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
            }),
        )?;

        /// Prints prompt and waits for an input from stdin.
        ///
        /// `fn read_line(prompt: str);`
        Self::define_function(
            &mut std,
            "read_line",
            vec![ValType::Str],
            ValType::Nil,
            Arc::new(|_, args| {
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
            }),
        )?;

        /// Returns type of a given value.
        ///
        /// `fn typeof(value: any) -> str;`
        Self::define_function(
            &mut std,
            "typeof",
            vec![ValType::Any],
            ValType::Nil,
            Arc::new(|_, args| {
                let value = args.first().expect("Cannot retrieve function argument.");

                Ok(Val::Str(value.get_type()))
            }),
        )?;

        Ok(std)
    }

    fn define_function(
        lib: &mut Env,
        name: &str,
        param_types: Vec<ValType>,
        ret_type: ValType,
        callable: Func,
    ) -> InterpretedResult<()> {
        let token = Token::new(
            TokenType::Identifier,
            name.to_string(),
            String::from(""),
            TokenPos(0, 0),
        );
        lib.define_function(env::Function::without_struct(
            token,
            Val::Callable(*Callable::new(param_types, ret_type, callable)),
        ))?;

        Ok(())
    }
}
