use rstest::rstest_parametrize;

#[rstest_parametrize(
    expected, input,
    case(4, String::from("ciao")),
    case(3, "Foo")
)]
fn strlen_test<S: AsRef<str>>(expected: usize, input: S) {
    assert_eq!(expected, input.as_ref().len());
}
