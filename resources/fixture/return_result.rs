use rstest::rstest;

#[rstest]
fn should_success() -> Result<(), &'static str> {
    Ok(())
}

#[rstest]
fn should_fail() -> Result<(), &'static str> {
    Err("Return Error")
}
