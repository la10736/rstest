use rstest::rstest_parametrize;

#[rstest_parametrize(u, s, t,
    case(42, "str", Unwrap(r#"("ss", -12)"#)),
    case(24, "trs", Unwrap(r#"("tt", -24)"#))
    ::trace
)]
fn should_fail(u: u32, s: &str, t: (&str, i32)) {
    assert!(false);
}
