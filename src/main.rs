use std::env;
use std::process;

use oxide::{print_version, run_file, run_repl};

fn main() {
    let args: Vec<String> = env::args().collect();
    let argc = args.len();

    if argc > 2 {
        println!("oxide \"filename.ox\"");
        process::exit(1);
    } else if args[1] == "version" {
        print_version();
    } else if argc == 2 {
        run_file(args[1].clone())
    } else {
        run_repl();
    }
}
