use rstest::rstest_matrix;

#[rstest_matrix(
    u => [42, 24],
    s => ["str", "trs"],
    t => [("ss", -12), ("tt", -24)]
    ::trace
)]
fn should_fail(u: u32, s: &str, t: (&str, i32)) {
    assert!(false);
}
