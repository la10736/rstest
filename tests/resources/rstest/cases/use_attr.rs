use rstest::rstest;

#[rstest]
#[case::ciao(4, "ciao")]
#[case::panic(42, "Foo")]
#[should_panic]
#[case::foo(3, "Foo")]
fn all(#[case] expected: usize, #[case] input: &str) {
    assert_eq!(expected, input.len());
}

#[rstest(expected, input)]
#[case::ciao(4, "ciao")]
#[case::foo(3, "Foo")]
#[case::panic(42, "Foo")]
#[should_panic]
fn just_cases(expected: usize, input: &str) {
    assert_eq!(expected, input.len());
}

#[rstest(
    case::ciao(4, "ciao"),
    case::foo(3, "Foo"),
    #[should_panic]
    case::panic(42, "Foo"),
)]
fn just_args(#[case] expected: usize, #[case] input: &str) {
    assert_eq!(expected, input.len());
}

#[rstest]
#[should_panic]
#[case(0, "ciao")]
#[case(0, "Foo")]
fn all_panic(#[case] expected: usize, #[case] input: &str) {
    assert_eq!(expected, input.len());
}
