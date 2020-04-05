use rstest::*;

async fn async_sum(a: u32, b: u32) -> u32 {
    a + b
}

#[rstest(expected, a, b, 
    case(5, 2, 3),
    #[should_panic]
    case(42, 40, 1),
    )
 ]
async fn my_async_test(expected: u32, a: u32, b: u32) {
    assert_eq!(expected, async_sum(a, b).await);
}
