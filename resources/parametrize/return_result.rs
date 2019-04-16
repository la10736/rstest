use rstest::rstest_parametrize;

#[rstest_parametrize(ret
    case::should_success(Ok(())),
    case::should_fail(Err("Return Error"))
)]
fn return_type(ret: Result<(), &'static str>) -> Result<(), &'static str> {
    ret
}
