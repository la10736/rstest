use rstest::rstest;

pub fn fixture() -> u32 { 42 }

#[rstest]
#[should_panic]
fn should_success(fixture: u32) {
    assert_ne!(fixture, 42);
}

#[rstest]
#[should_panic]
fn should_fail(fixture: u32) {
    assert_eq!(fixture, 42);
}
