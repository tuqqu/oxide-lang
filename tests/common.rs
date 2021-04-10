use std::cell::RefCell;
use std::fs;
use std::rc::Rc;

use oxide::run_file_with_streams;

pub fn compare_output(sample_file: String, output_file: String) {
    let stdout = Rc::new(RefCell::new(Vec::<u8>::new()));
    let stderr = Rc::new(RefCell::new(Vec::<u8>::new()));

    let vecout = Rc::clone(&stdout);
    let vecerr = Rc::clone(&stderr);

    run_file_with_streams(sample_file, Some(stdout), Some(vecerr), None);

    let expected = &*vecout.borrow();
    let expected = String::from_utf8_lossy(expected);

    let actual = fs::read_to_string(&output_file).expect("Error while reading file.");

    assert_eq!(expected, actual);
}
