use rstest::*;
use rstest_reuse::{self, *};

#[template]
#[rstest]
#[case(1, 1)]
#[case(0)]
fn template_with_fallback(#[case] expected: u32, #[case(default)] value: u32) {}

#[apply(template_with_fallback)]
fn apply_without_case_attributes(expected: u32, value: u32) {
    assert_eq!(expected, value);
}

#[apply(template_with_fallback)]
#[case(0, _)]
fn apply_and_add_omitting_case(expected: u32, value: u32) {
    assert_eq!(expected, value);
}

#[template]
#[rstest]
#[case(1, 1)]
fn plain_template(#[case] expected: u32, #[case] value: u32) {}

#[apply(plain_template)]
#[case(7)]
fn apply_and_add_modifier(expected: u32, #[case(default(7))] value: u32) {
    assert_eq!(expected, value);
}

#[apply(plain_template)]
#[case(0, _)]
fn apply_and_add_optional(expected: u32, #[case(opt)] value: Option<u32>) {
    assert_eq!(expected, value.unwrap_or_default());
}
