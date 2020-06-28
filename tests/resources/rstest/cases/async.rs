use rstest::*;

#[rstest(expected, value,
    case::pass(42, 42),
    case::fail(42, 41),
    #[should_panic]
    case::pass_panic(42, 41),
    #[should_panic]
    case::fail_panic(42, 42),
)]
async fn my_async_test(expected: u32, value: u32) {
    assert_eq!(expected, value);
}