use rstest::*;
use std::net::Ipv4Addr;

#[rstest]
#[case(42, 42)]
#[case(0)]
#[case(0, _)]
fn default_value(#[case] expected: u32, #[case(default)] value: u32) {
    assert_eq!(expected, value);
}

#[rstest]
#[case(10, 10)]
#[case(42)]
#[case(42, _)]
fn default_expression(#[case] expected: u32, #[case(default(40 + 2))] value: u32) {
    assert_eq!(expected, value);
}

#[rstest]
#[case(1.0, "1.0")]
#[case(2.5)]
fn default_with_magic_conversion(#[case] expected: f64, #[case(default("2.5"))] value: f64) {
    assert_eq!(expected, value);
}

#[rstest]
#[case(Some(2), 2)]
#[case(None)]
#[case(None, _)]
fn optional_value(#[case] expected: Option<u32>, #[case(opt)] value: Option<u32>) {
    assert_eq!(expected, value);
}

#[rstest]
#[case(Some(Ipv4Addr::new(127, 0, 0, 1)), "127.0.0.1")]
#[case(None)]
fn optional_with_magic_conversion(
    #[case] expected: Option<Ipv4Addr>,
    #[case(opt)] value: Option<Ipv4Addr>,
) {
    assert_eq!(expected, value);
}

#[rstest]
#[case((false, 0, None))]
#[case((true, 0, None), true)]
#[case((false, 1, None), _, 1)]
#[case((false, 0, Some(1.5)), false, 0, Some(1.5))]
#[case((false, 0, None), _, _, _)]
fn all_defaults(
    #[case] expected: (bool, i32, Option<f64>),
    #[case(default)] bin: bool,
    #[case(default)] int: i32,
    #[case(default)] opt: Option<f64>,
) {
    assert_eq!(expected, (bin, int, opt));
}

#[rstest]
#[case((1, Some(2), 3), _, 2)]
#[case((5, None, 3), 5, _, 3)]
#[case((1, None, 6), _, _, 6)]
fn skip_in_the_middle(
    #[case] expected: (u32, Option<u32>, u32),
    #[case(default(1))] a: u32,
    #[case(opt)] b: Option<u32>,
    #[case(default(3))] c: u32,
) {
    assert_eq!(expected, (a, b, c));
}

#[fixture]
fn fixture() -> u32 {
    42
}

#[fixture]
fn other(#[default(7)] v: u32) -> u32 {
    v
}

#[rstest]
#[case(1, 1)]
#[case(0)]
fn with_fixtures(
    #[case] expected: u32,
    #[case(default)] value: u32,
    fixture: u32,
    #[with(3)] other: u32,
) {
    assert_eq!(expected, value);
    assert_eq!(42, fixture);
    assert_eq!(3, other);
}

#[rstest]
#[case(5, 5)]
#[case(0)]
fn omitted_value_named_as_a_fixture(#[case] expected: u32, #[case(default)] fixture: u32) {
    // Should use the default value and not the `fixture()` one
    assert_eq!(expected, fixture);
}

#[rstest]
#[case(1, 1)]
#[case(0)]
fn with_values(#[case] expected: u32, #[case(default)] value: u32, #[values(5, 6)] v: u32) {
    assert_eq!(expected, value);
    assert!(v == 5 || v == 6);
}

#[rstest]
#[case(3, Some(4), 2, 3)]
#[case(1, None)]
fn mutable(
    #[case] expected: u32,
    #[case] expected_opt: Option<u32>,
    #[case(default)] mut value: u32,
    #[case(opt)] mut opt: Option<u32>,
) {
    value += 1;
    if let Some(o) = opt.as_mut() {
        *o += 1;
    }
    assert_eq!(expected, value);
    assert_eq!(expected_opt, opt);
}

struct T(u32, u32);

impl Default for T {
    fn default() -> Self {
        T(1, 2)
    }
}

#[rstest]
#[case(3, T(1, 2))]
#[case(3)]
fn destruct(#[case] expected: u32, #[case(default)] T(a, b): T) {
    assert_eq!(expected, a + b);
}

#[rstest]
#[case(42, 42)]
#[case(0)]
fn by_ref(#[case] expected: u32, #[by_ref] #[case(default)] value: &u32) {
    assert_eq!(expected, *value);
}

#[rstest]
#[case(Some(42), 42)]
#[case(None)]
#[trace]
fn trace(#[case] expected: Option<u32>, #[case(opt)] value: Option<u32>) {
    assert_eq!(expected, value);
}

#[rstest]
#[case::given(1, 1)]
#[case::omitted(0)]
#[case::infer(0, _)]
fn named_cases(#[case] expected: u32, #[case(default)] value: u32) {
    assert_eq!(expected, value);
}

#[rstest]
#[case(1, async { 1 })]
#[case(42)]
#[async_std::test]
async fn future(
    #[case] expected: u32,
    #[future]
    #[case(default(async { 42 }))]
    value: u32,
) {
    assert_eq!(expected, value.await);
}

#[rstest]
#[case(1, 1)]
#[case(0)]
#[async_std::test]
async fn async_test(#[case] expected: u32, #[case(default)] value: u32) {
    assert_eq!(expected, value);
}

#[rstest]
#[case(2, 1)]
#[case(1)]
#[async_std::test]
async fn async_mutable(#[case] expected: u32, #[case(default)] mut value: u32) {
    value += 1;
    assert_eq!(expected, value);
}
