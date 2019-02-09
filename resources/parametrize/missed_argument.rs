use rstest::rstest_parametrize;

#[rstest_parametrize(f, case(42))]
fn error_param_not_exist() {}
