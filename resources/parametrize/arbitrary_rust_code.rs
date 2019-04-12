use rstest::rstest_parametrize;

#[rstest_parametrize(
    condition,
    case(Unwrap(r#""".len()==0"#)),
    case(Unwrap(r#"vec![1,5,7].contains(&23)"#)),
)]
fn arbitrary(condition: bool) {
    assert!(condition);
}
