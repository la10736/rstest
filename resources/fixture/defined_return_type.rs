use rstest::*;

#[fixture]
pub fn i() -> u32 {
    42
}

#[fixture]
pub fn j() -> i32 {
    -42
}

#[fixture(::default<impl Iterator<Item=(u32, i32)>>::partial_1<impl Iterator<Item=(I,i32)>>)]
pub fn fx<I, J>(i: I, j: J) -> impl Iterator<Item=(I, J)> {
    std::iter::once((i, j))
}

#[test]
fn resolve() {
    assert_eq!((42, -42), fx::default().next().unwrap())
}

#[test]
fn resolve_partial() {
    assert_eq!((42.0, -42), fx::partial_1(42.0).next().unwrap())
}
