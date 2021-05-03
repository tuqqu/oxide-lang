use std::io::{self, Write};
use std::{env, fs, process};

use oxide::Engine;

fn main() {
    let args: Vec<String> = env::args().collect();
    let argc = args.len();
    let file: &str;
    let mut top_level = false;

    if argc > 3 {
        println!("oxide \"filename.ox\"");
        process::exit(1);
    } else if args[1] == "--version" || args[1] == "-v" {
        println!("Oxide {}", Engine::VERSION);
        process::exit(0);
    } else if argc == 2 {
        file = &args[1];
    } else if argc == 3 && args[1] == "--no-entry-point" || args[1] == "-n" {
        top_level = true;
        file = &args[2];
    } else {
        run_repl();
        process::exit(0);
    }

    let contents = fs::read_to_string(file)
        .unwrap_or_else(|_| panic!("Something went wrong while reading the file \"{}\"", file));

    let ast = Engine::ast(contents);

    if top_level {
        let _val = Engine::run_top_level(&ast, None);
    } else {
        let _val = Engine::run(&ast, None);
    }
}

/// Runs REPL mode from stdin.
fn run_repl() {
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

        let ast = Engine::ast(line);
        let _val = Engine::run_top_level(&ast, None);
    }
}
