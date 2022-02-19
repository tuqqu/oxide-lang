mod common;

use common::compare_output;

const SAMPLE_PATH: &str = "./tests/scripts";
const OUTPUT_PATH: &str = "./tests/output";

fn test_script(script: &str) {
    let sample_file: &str = &format!("{}/{}.ox", SAMPLE_PATH, script);
    let output_file: &str = &format!("{}/{}.output", OUTPUT_PATH, script);

    compare_output(sample_file, output_file);
}

#[test]
fn test_operator() {
    test_script("operator");
}

#[test]
fn test_if() {
    test_script("if");
}

#[test]
fn test_match() {
    test_script("match");
}

#[test]
fn test_variable() {
    test_script("variable");
}

#[test]
fn test_constant() {
    test_script("constant");
}

#[test]
fn test_types() {
    test_script("types");
}

#[test]
fn test_types_union() {
    test_script("types_union");
}

#[test]
fn test_type_decl() {
    test_script("type_decl");
}

#[test]
fn test_type_inference() {
    test_script("type_inference");
}

#[test]
fn test_type_casting() {
    test_script("type_casting");
}

#[test]
fn test_while() {
    test_script("while");
}

#[test]
fn test_for() {
    test_script("for");
}

#[test]
fn test_for_in() {
    test_script("for_in");
}

#[test]
fn test_range() {
    test_script("range");
}

#[test]
fn test_loop() {
    test_script("loop");
}

#[test]
fn test_scope() {
    test_script("scope");
}

#[test]
fn test_stdlib() {
    test_script("stdlib");
}

#[test]
fn test_comment() {
    test_script("comment");
}

#[test]
fn test_fn() {
    test_script("fn");
}

#[test]
fn test_recursive_fn() {
    test_script("recursive_fn");
}

#[test]
fn test_closure() {
    test_script("closure");
}

#[test]
fn test_lambda() {
    test_script("lambda");
}

#[test]
fn test_vec() {
    test_script("vec");
}

#[test]
fn test_struct_properties() {
    test_script("struct_properties");
}

#[test]
fn test_struct_methods() {
    test_script("struct_methods");
}

#[test]
fn test_struct_public_access() {
    test_script("struct_public_access");
}

#[test]
fn test_struct_constants() {
    test_script("struct_constants");
}

#[test]
fn test_struct_static_self() {
    test_script("struct_static_self");
}

#[test]
fn test_struct_static_call_non_static() {
    test_script("struct_static_call_non_static");
}

#[test]
fn test_enum_values() {
    test_script("enum_values");
}

#[test]
fn test_enum_impl() {
    test_script("enum_impl");
}

#[test]
fn test_traits() {
    test_script("traits");
}

#[test]
fn test_trait_static_call_non_static() {
    test_script("trait_static_call_non_static");
}
