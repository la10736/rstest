use rstest::rstest;

#[rstest]
fn both(#[values(4, 2*3-2)] expected: usize, #[values("ciao", "buzz")] input: &str) {
    assert_eq!(expected, input.len());
}

#[rstest(
    input => ["ciao", "buzz"]
)]
fn first(#[values(4, 2*3-2)] expected: usize, input: &str) {
    assert_eq!(expected, input.len());
}

#[rstest(
    expected => [4, 2*3-2]
)]
fn second(expected: usize, #[values("ciao", "buzz")] input: &str) {
    assert_eq!(expected, input.len());
}

#[rstest]
fn doc_comment_both(
    #[values(
        /// len_four
        4,
        /// len_four_alt
        2*3-2
    )]
    expected: usize,
    #[values(
        /// greet_ciao
        "ciao",
        /// greet_buzz
        "buzz"
    )]
    input: &str,
) {
    assert_eq!(expected, input.len());
}

#[rstest(
    input => [
        /// greet_ciao
        "ciao",
        /// greet_buzz
        "buzz",
    ]
)]
fn doc_comment_first(
    #[values(
        /// len_four
        4,
        /// len_four_alt
        2*3-2
    )]
    expected: usize,
    input: &str,
) {
    assert_eq!(expected, input.len());
}

#[rstest(
    expected => [
        /// len_four
        4,
        /// len_four_alt
        2*3-2,
    ]
)]
fn doc_comment_second(
    expected: usize,
    #[values(
        /// greet_ciao
        "ciao",
        /// greet_buzz
        "buzz"
    )]
    input: &str,
) {
    assert_eq!(expected, input.len());
}
