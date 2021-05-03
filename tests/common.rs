use std::cell::RefCell;
use std::fs;
use std::rc::Rc;

use oxide::Engine;

pub fn compare_output(sample_file: &str, output_file: &str, top_level: bool) {
    let stdout = Rc::new(RefCell::new(Vec::<u8>::new()));
    let stderr = Rc::new(RefCell::new(Vec::<u8>::new()));

    let vecout = Rc::clone(&stdout);
    let vecerr = Rc::clone(&stderr);

    let script = fs::read_to_string(sample_file).expect("Error while reading file.");

    let ast = Engine::ast(script);
    let _val = if top_level {
        Engine::run_top_level(&ast, Some((Some(stdout), Some(vecerr), None)))
    } else {
        Engine::run(&ast, Some((Some(stdout), Some(vecerr), None)))
    };

    let actual = &*vecout.borrow();
    let actual = String::from_utf8_lossy(actual);

    let expected = fs::read_to_string(output_file).expect("Error while reading file.");

    assert_eq!(actual, expected);
}
