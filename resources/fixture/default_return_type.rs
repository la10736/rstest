use rstest::*;

#[fixture]
pub fn i() -> u32 {
    42
}

#[fixture(::default<impl Iterator<Item=u32>>)]
pub fn fx<I>(i: I) -> impl Iterator<Item=I> {
    std::iter::once(i)
}

#[test]
fn resolve() {
    assert_eq!(42, fx::default().next().unwrap())
}
