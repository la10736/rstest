use rstest::rstest;

#[rstest]
#[case::ciao(4, "ciao")]
#[case::foo(3, "Foo")]
#[case::panic(42, "Foo")]
#[should_panic]
fn strlen_test(#[case] expected: usize, #[case] input: &str) {
    assert_eq!(expected, input.len());
}
