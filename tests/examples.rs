mod common;

use common::compare_output;

const EXAMPLES_PATH: &str = "./examples";
const EXAMPLES_OUTPUT_PATH: &str = "./tests/examples_output";

fn test_example(script: &str) {
    let sample_file: String = format!("{}/{}.ox", EXAMPLES_PATH, script);
    let output_file: String = format!("{}/{}.output", EXAMPLES_OUTPUT_PATH, script);

    compare_output(sample_file, output_file);
}

#[test]
fn test_insertion_sort() {
    test_example("insertion_sort");
}

#[test]
fn test_selection_sort() {
    test_example("selection_sort");
}

#[test]
fn test_matrix_multiplication() {
    test_example("matrices");
}

#[test]
fn test_gcd() {
    test_example("gcd");
}

#[test]
fn test_fib() {
    test_example("fib");
}
