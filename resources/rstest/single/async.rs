use rstest::*;

#[fixture]
fn fixture() -> u32 { 42 }

#[rstest]
async fn should_pass(fixture: u32) {
    assert_eq!(fixture, 42);
}

#[rstest]
async fn should_fail(fixture: u32) {
    assert_ne!(fixture, 42);
}

#[rstest]
#[should_panic]
async fn should_panic_pass(fixture: u32) {
    panic!(format!("My panic -> fixture = {}", fixture));
}

#[rstest]
#[should_panic]
async fn should_panic_fail(fixture: u32) {
    assert_eq!(fixture, 42);
}
