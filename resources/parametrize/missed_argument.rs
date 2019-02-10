use rstest::rstest_parametrize;

#[cfg(test)]
#[rstest_parametrize(f, case(42), case(24))]
fn error_param_not_exist() {}
