use rstest::rstest_matrix;

#[rstest_matrix(
    expected => [4, 2*3-2],
    input => ["ciao", "buzz"],
)]
fn strlen_test(expected: usize, input: &str) {
    assert_eq!(expected, input.len());
}
