use std::env;
use std::process;

use oxide::{run_file, run_repl};

fn main() {
    let args: Vec<String> = env::args().collect();
    let argc = args.len();

    if argc > 2 {
        println!("oxide \"filename.ox\"");
        process::exit(1);
    } else if argc == 2 {
        run_file(args[1].to_string())
    } else {
        run_repl();
    }
}
