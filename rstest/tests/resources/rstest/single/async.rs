use rstest::*;

#[fixture]
async fn fixture() -> u32 {
    42
}

#[rstest]
#[async_std::test]
async fn should_pass(#[future] fixture: u32) {
    assert_eq!(fixture.await, 42);
}

#[rstest]
#[async_std::test]
async fn should_fail(#[future] fixture: u32) {
    assert_ne!(fixture.await, 42);
}

#[rstest]
#[async_std::test]
#[should_panic]
async fn should_panic_pass(#[future] fixture: u32) {
    panic!(format!("My panic -> fixture = {}", fixture.await));
}

#[rstest]
#[async_std::test]
#[should_panic]
async fn should_panic_fail(#[future] fixture: u32) {
    assert_eq!(fixture.await, 42);
}
