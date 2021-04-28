use std::cmp::Ordering;
use std::{env, process};

use oxide::{print_version, run_file, run_repl, run_file_top_level};

fn main() {
    let args: Vec<String> = env::args().collect();
    let argc = args.len();

    match argc.cmp(&2) {
        Ordering::Greater => {
            println!("oxide \"filename.ox\"");
            process::exit(1);
        }
        Ordering::Equal => {
            if args[1] == "--version" || args[1] == "-v" {
                print_version();
            } else if args[1] == "--no-entry-point" || args[1] == "-n" {
                run_file_top_level(&args[1]);
            } else {
                run_file(&args[1]);
            }
        }
        Ordering::Less => {
            run_repl();
        }
    }
}
