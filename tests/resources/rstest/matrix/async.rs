use rstest::*;

#[rstest(
    first => [1, 2],
    second => [42, 21]
)]
async fn my_async_test(first: u32, second: u32) {
    assert_eq!(42, first * second);
}