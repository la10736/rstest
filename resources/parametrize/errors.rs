use rstest::rstest_parametrize;

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
    case(Unwrap(r#"vec![1,2,3].contains(2)"#)))
]
fn error_in_arbitrary_rust_code(condition: bool) {
    assert!(condition)
}
