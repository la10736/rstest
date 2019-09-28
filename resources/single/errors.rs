use rstest::*;

#[fixture]
fn fixture() -> u32 { 42 }

#[rstest]
fn error_inner(fixture: u32) {
    let a: u32 = "";
}

#[rstest]
fn error_cannot_resolve_fixture(no_fixture: u32) {
}

#[rstest]
fn error_fixture_wrong_type(fixture: String) {
}

#[rstest(not_a_fixture(24))]
fn error_inject_an_invalid_fixture(fixture: String) {
}

#[fixture]
fn name() -> &'static str {
    "name"
}

#[fixture]
fn f(name: &str) -> String {
    name.to_owned()
}

#[rstest(f("first"), f("second"))]
fn error_inject_a_fixture_more_than_once(f: String) {
}

#[fixture]
fn first() -> u32 {
    42
}

#[fixture]
fn second() -> &'static str {
    "foo"
}

#[fixture]
fn double(first: u32, second: &str) -> u32 {
    0
}

#[rstest(double("bar"))]
fn should_show_type_error(double: u32) {}
