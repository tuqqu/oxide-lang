use std::io::{self, Write};
use std::{env, fs, process};

use oxide_interpreter::Engine;

fn main() {
    let args: Vec<String> = env::args().collect();
    let engine = Engine::new(on_error_handler);
    if args.len() == 1 {
        eprint_error("Arguments not found.");
        process::exit(1);
    }

    match args[1].as_str() {
        "-v" | "--version" => {
            print_version();
            process::exit(0);
        }
        "-h" | "--help" => {
            print_help();
            process::exit(0);
        }
        "-r" | "--repl" => {
            repl(&engine);
            process::exit(0);
        }
        _ => {}
    }

    let allow_top_level =
        args.contains(&"--allow-top-level".to_string()) || args.contains(&"-t".to_string());
    let args: Vec<String> = args
        .into_iter()
        .filter(|arg| !arg.starts_with('-'))
        .collect();
    if args.len() == 1 {
        eprint_error("Filename not found.");
        process::exit(1);
    }

    let contents = fs::read_to_string(&args[1]).unwrap_or_else(|_| {
        panic!(
            "Something went wrong while reading the file \"{}\"",
            &args[1]
        )
    });
    let ast = engine.ast(contents);

    if ast.top_level && !allow_top_level {
        eprintln!(
            "Top-level instructions are not allowed. To allow them run command with a \"-t\" flag."
        );
        process::exit(1);
    }

    let _val = engine.run(&ast, None);
}

/// Runs REPL mode from stdin.
fn repl(engine: &Engine) {
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

        let ast = engine.ast(line);
        let _val = engine.run(&ast, None);
    }
}

fn print_help() {
    println!(
        "{}",
        format!(
            r"Oxide {}

USAGE:
    oxide [FLAGS] [FILE]

FLAGS:
    -h, --help              Prints help
    -v, --version           Prints version
    -r, --repl              Run REPL
    -t, --allow-top-level   Allow top-level instructions

ARGS:
    <FILE>   Script file to run

EXAMPLE:
    oxide script.ox",
            Engine::VERSION
        )
    );
}

fn print_version() {
    println!("Oxide {}", Engine::VERSION);
}

fn eprint_error(msg: &str) {
    eprintln!("{}", msg);
    eprintln!("Run the command with \"--help\" to see help information.");
}

fn on_error_handler(errs: Vec<Box<dyn std::error::Error>>) -> ! {
    for err in errs {
        eprintln!("\x1b[0;31m{}\x1b[0m", err);
    }

    process::exit(1);
}
