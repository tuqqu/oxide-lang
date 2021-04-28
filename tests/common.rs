use std::cell::RefCell;
use std::fs;
use std::rc::Rc;

use oxide::run;

pub fn compare_output(sample_file: &str, output_file: &str, top_level: bool) {
    let stdout = Rc::new(RefCell::new(Vec::<u8>::new()));
    let stderr = Rc::new(RefCell::new(Vec::<u8>::new()));

    let vecout = Rc::clone(&stdout);
    let vecerr = Rc::clone(&stderr);

    let script = fs::read_to_string(sample_file).expect("Error while reading file.");

    run(script, Some(stdout), Some(vecerr), None, top_level);

    let expected = &*vecout.borrow();
    let expected = String::from_utf8_lossy(expected);

    let actual = fs::read_to_string(output_file).expect("Error while reading file.");

    assert_eq!(expected, actual);
}
