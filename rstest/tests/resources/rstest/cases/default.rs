use rstest::*;

#[rstest]
#[default]
#[case(42, 42)]
#[case(0)]
fn my_default_test(#[case] expected: u32, #[case] value: u32) {
    assert_eq!(expected, value);
}

#[rstest]
#[case()]
#[case(false)]
#[case(false, 0)]
#[case(false, 0, None)]
#[default]
fn multiple_default_value(#[case] bin: bool, #[case] int: i32, #[case] opt: Option<f64>) {
    assert!(!bin);
    assert_eq!(int, 0);
    assert_eq!(opt, None);
}

#[rstest]
#[case()]
#[case(false)]
#[case(true, true)]
#[default]
fn different_default_value(#[case] expected: bool, #[case] value: bool) {
    assert_eq!(expected, value);
}

#[rstest]
#[case()]
#[case(false)]
#[case(true, Some(false))]
#[case(true, Some(true))]
#[default]
fn option_default_value(#[case] expected: bool, #[case] value: Option<bool>) {
    assert_eq!(expected, value.is_some());
}
