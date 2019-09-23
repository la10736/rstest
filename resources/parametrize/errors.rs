use rstest::*;
#[fixture]
pub fn fixture() -> u32 { 42 }

#[rstest_parametrize(f, case(42))]
fn error_inner(f: i32) {
    let a: u32 = "";
}

#[rstest_parametrize(f, case(42))]
fn error_cannot_resolve_fixture(no_fixture: u32, f: u32) {}

#[rstest_parametrize(f, case(42))]
fn error_fixture_wrong_type(fixture: String, f: u32) {}

#[rstest_parametrize(f, case(42))]
fn error_param_wrong_type(f: &str) {}

#[rstest_parametrize(condition,
    case(vec![1,2,3].contains(2)))
]
fn error_in_arbitrary_rust_code(condition: bool) {
    assert!(condition)
}

#[rstest_parametrize(f, case(42), not_a_fixture(24))]
fn error_inject_an_invalid_fixture(f: u32) {
}

#[fixture]
fn n() -> u32 {
    24
}

#[fixture]
fn f(n: u32) -> u32 {
    2*n
}

#[rstest_parametrize(f, f(42), case(12))]
fn error_inject_a_fixture_that_is_already_a_case(f: u32) {
}

#[rstest_parametrize(f(42), f, case(12))]
fn error_define_case_that_is_already_an_injected_fixture(f: u32) {
}

#[rstest_parametrize(v, f(42), f(42), case(12))]
fn error_inject_a_fixture_more_than_once(v: u32, f: u32) {
}
