use rstest::*;

#[fixture]
pub fn fixture() -> u32 {
    42
}

#[fixture]
fn error_cannot_resolve_fixture(no_fixture: u32) {}
