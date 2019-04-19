use rstest::{rstest, fixture};

#[fixture]
fn my_fixture() -> u32 { 42 }

#[test]
fn resolve_new() {
    assert_eq!(42, my_fixture::new().take());
}

#[test]
fn resolve_default() {
    assert_eq!(42, my_fixture::default().take());
}
