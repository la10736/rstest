use std::future::Future;
use std::io::prelude::*;

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

#[fixture]
async fn async_impl_output() -> impl Read {
    std::io::Cursor::new(vec![1, 2, 3, 4, 5])
}

#[rstest]
async fn use_async_impl_output<T: Read>(async_impl_output: impl Future<Output = T>) {
    let reader = async_impl_output.await;
}
