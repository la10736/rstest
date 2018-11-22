extern crate rstest;

use rstest::rstest_parametrize;

#[rstest_parametrize(
    expected, input,
    case(4, Unwrap(r#"String::from("ciao")"#)),
    case(3, "Foo")
)]
fn strlen_test(expected: usize, input: impl AsRef<str>) {
    assert_eq!(expected, input.as_ref().len());
}
