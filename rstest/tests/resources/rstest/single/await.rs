use rstest::*;

#[fixture]
async fn fixture() -> u32 {
    42
}

#[rstest]
async fn should_pass(#[await_future] fixture: u32) {
    assert_eq!(fixture, 42);
}

#[rstest]
async fn should_fail(#[await_future] fixture: u32) {
    assert_ne!(fixture, 42);
}

#[rstest]
#[should_panic]
async fn should_panic_pass(#[await_future] fixture: u32) {
    panic!(format!("My panic -> fixture = {}", fixture));
}

#[rstest]
#[should_panic]
async fn should_panic_fail(#[await_future] fixture: u32) {
    assert_eq!(fixture, 42);
}
