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

#[apply(template_with_fallback)]
fn apply_with_plain_case_attributes(#[case] expected: u32, #[case] value: u32) {
    // The template's `#[case(default)]` is not replaced by the plain `#[case]`
    assert_eq!(expected, value);
}

#[template]
#[rstest]
#[case(Some(1), 1)]
fn plain_optional_template(#[case] expected: Option<u32>, #[case] value: Option<u32>) {}

#[apply(plain_optional_template)]
#[case(None, _)]
fn apply_and_add_optional(expected: Option<u32>, #[case(opt)] value: Option<u32>) {
    assert_eq!(expected, value);
}
