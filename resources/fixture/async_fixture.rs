use std::future::Future;

use rstest::*;

#[fixture]
async fn async_u32() -> u32 {
    42
}

#[rstest]
async fn default_is_async() {
    assert_eq!(42, async_u32::default().await);
}

#[rstest]
async fn use_async_fixture(async_u32: impl Future<Output=u32>) {
    assert_eq!(42, async_u32.await);
}
