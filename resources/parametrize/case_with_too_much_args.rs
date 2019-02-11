use rstest::rstest_parametrize;

#[cfg(test)]
#[rstest_parametrize(a, case(42, 43), case(12), case(24, 34))]
fn error_too_much_arguments(a: u32) {}
