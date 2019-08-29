use rstest::*;

#[fixture]
pub fn fixture() -> u32 { 42 }

#[fixture]
fn error_inner(fixture: u32) {
    let a: u32 = "";
}

#[fixture]
fn error_cannot_resolve_fixture(no_fixture: u32) {
}

#[fixture]
fn error_fixture_wrong_type(fixture: String) {
}

#[fixture(not_a_fixture(24))]
fn error_inject_an_invalid_fixture(fixture: String) {
}
