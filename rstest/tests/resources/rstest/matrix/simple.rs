use rstest::rstest;

#[rstest(
    expected => [4, 2*3-2],
    input => ["ciao", "buzz"],
)]
fn strlen_test(expected: usize, input: &str) {
    assert_eq!(expected, input.len());
}

#[rstest(
    expected => [
        /// len_four
        4,
        /// len_four_alt
        2*3-2,
    ],
    input => [
        /// greet_ciao
        "ciao",
        /// greet_buzz
        "buzz",
    ],
)]
fn doc_comment_strlen_test(expected: usize, input: &str) {
    assert_eq!(expected, input.len());
}
