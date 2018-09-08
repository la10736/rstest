extern crate rstest;

use rstest::rstest;

pub fn fixture() -> u32 { 42 }

#[rstest]
fn should_success(mut fixture: u32) {
    fixture += 1;
    assert_eq!(fixture, 43);
}

#[rstest]
fn should_fail(mut fixture: u32) {
    fixture += 1;
    assert_ne!(fixture, 43);
}
