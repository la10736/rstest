use rstest_reuse;

use rstest::rstest;
use rstest_reuse::*;

#[template]
#[rstest]
#[case(2, 2)]
#[case(4/2, 2)]
fn copy_cases(#[case] a: u32, #[case] b: u32) {}

#[apply(copy_cases)]
fn it_works(a: u32, b: u32) {
    assert!(a == b);
}

#[apply(copy_cases)]
fn should_not_copy_attributes_if_already_present(#[case] a: u32, b: u32) {
    assert!(a == b);
}

#[apply(copy_cases)]
#[case::more(8/4, 2)]
fn should_add_a_case(a: u32, b: u32) {
    assert!(a == b);
}

#[apply(copy_cases)]
fn can_add_values(a: u32, b: u32, #[values(1, 2, 3)] _add_some_tests: u32) {
    assert!(a == b);
}

#[apply(copy_cases)]
fn should_copy_cases_also_from_underscored_attrs(_a: u32, _b: u32) {}
