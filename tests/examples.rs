mod common;

use common::test_example;

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
    test_example("matrix_multiplication");
}

#[test]
fn test_gcd() {
    test_example("gcd");
}
