extern crate rstest;

use rstest::rstest;

pub fn fixture() -> u32 { 42 }

#[rstest(trace)]
fn should_fail(fixture: u32) {
    assert_ne!(fixture, 42);
}
