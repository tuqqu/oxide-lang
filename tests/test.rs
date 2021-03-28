use oxide::run_file_with_streams;
use std::cell::RefCell;
use std::fs;
use std::rc::Rc;

const SAMPLE_PATH: &str = "./tests/scripts";
const OUTPUT_PATH: &str = "./tests/output";

pub fn test_script(script: &str) {
    let sample_file: String = format!("{}/{}.ox", SAMPLE_PATH, script);
    let output_file: String = format!("{}/{}.output", OUTPUT_PATH, script);

    compare_output(sample_file, output_file)
}

const EXAMPLES_PATH: &str = "./examples";
const EXAMPLES_OUTPUT_PATH: &str = "./tests/examples_output";

pub fn test_example(script: &str) {
    let sample_file: String = format!("{}/{}.ox", EXAMPLES_PATH, script);
    let output_file: String = format!("{}/{}.output", EXAMPLES_OUTPUT_PATH, script);

    compare_output(sample_file, output_file);
}

fn compare_output(sample_file: String, output_file: String) {
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