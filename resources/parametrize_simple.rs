extern crate rstest;

use rstest::rstest_parametrize;

#[rstest_parametrize(
    expected, input,
    case(4, "ciao"),
    case(3, "Foo")
)]
fn strlen_test(expected: usize, input: &str) {
    assert_eq!(expected, input.len());
}
