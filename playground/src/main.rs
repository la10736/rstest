async fn my_async_test(expected: u32, a: u32, b: u32) {
    assert_eq!(expected, async_sum(a, b).await);
}
