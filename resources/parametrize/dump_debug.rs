use rstest::rstest_parametrize;

#[rstest_parametrize(u, s, t,
    case(42, "str", ("ss", -12)),
    case(24, "trs", ("tt", -24))
    ::trace
)]
fn should_fail(u: u32, s: &str, t: (&str, i32)) {
    assert!(false);
}
