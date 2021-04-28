use std::cell::RefCell;
use std::fs;
use std::io::{Read, Write};
use std::rc::Rc;

pub fn compare_output(
    sample_file: &str,
    output_file: &str,
    runner: fn(
        &str,
        Option<Rc<RefCell<dyn Write>>>,
        Option<Rc<RefCell<dyn Write>>>,
        Option<Rc<RefCell<dyn Read>>>,
    ),
) {
    let stdout = Rc::new(RefCell::new(Vec::<u8>::new()));
    let stderr = Rc::new(RefCell::new(Vec::<u8>::new()));

    let vecout = Rc::clone(&stdout);
    let vecerr = Rc::clone(&stderr);

    runner(sample_file, Some(stdout), Some(vecerr), None);

    let expected = &*vecout.borrow();
    let expected = String::from_utf8_lossy(expected);

    let actual = fs::read_to_string(output_file).expect("Error while reading file.");

    assert_eq!(expected, actual);
}
