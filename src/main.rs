use std::cmp::Ordering;
use std::env;
use std::process;

use oxide::{print_version, run_file, run_repl};

fn main() {
    let args: Vec<String> = env::args().collect();
    let argc = args.len();

    match argc.cmp(&2) {
        Ordering::Greater => {
            println!("oxide \"filename.ox\"");
            process::exit(1);
        }
        Ordering::Equal => {
            if args[1] == "version" {
                print_version();
            } else {
                run_file(args[1].clone());
            }
        }
        Ordering::Less => {
            run_repl();
        }
    }
}
