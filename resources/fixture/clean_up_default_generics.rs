use rstest::*;

#[fixture]
pub fn s() -> &'static str {
    "42"
}

#[fixture]
pub fn fx<S: ToString>(s: S) -> usize {
    s.to_string().len()
}

#[test]
fn resolve() {
    assert_eq!(2, fx::default())
}
