use std::future::Future;
use std::io::prelude::*;

use rstest::*;

#[fixture]
async fn async_u32() -> u32 {
    42
}

#[fixture]
async fn nest_fixture(async_u32: impl Future<Output=u32>) -> u32 {
    async_u32.await
}

#[fixture(fortytwo = async { 42 })]
async fn nest_fixture_with_default(fortytwo: impl Future<Output=u32>) -> u32 {
    fortytwo.await
}

#[rstest]
async fn default_is_async() {
    assert_eq!(42, async_u32::default().await);
}

#[rstest]
async fn use_async_nest_fixture_default(nest_fixture: impl Future<Output=u32>) {
    assert_eq!(42, nest_fixture.await);
}

#[rstest(nest_fixture(async { 24 }))]
async fn use_async_nest_fixture_injected(nest_fixture: impl Future<Output=u32>) {
    assert_eq!(24, nest_fixture.await);
}

#[rstest]
async fn use_async_nest_fixture_with_default(nest_fixture_with_default: impl Future<Output=u32>) {
    assert_eq!(42, nest_fixture_with_default.await);
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

#[fixture(four = async { 4 }, two = 2)]
async fn two_args_mix_fixture(four: impl Future<Output=u32>, two: u32) -> u32 {
    four.await * 10 + two
}

#[rstest]
async fn use_two_args_mix_fixture(two_args_mix_fixture: impl Future<Output=u32>) {
    assert_eq!(42, two_args_mix_fixture.await);
}

#[rstest(two_args_mix_fixture(async { 5 }))]
async fn use_two_args_mix_fixture_inject_first(two_args_mix_fixture: impl Future<Output=u32>) {
    assert_eq!(52, two_args_mix_fixture.await);
}

#[rstest(two_args_mix_fixture(async { 3 }, 1))]
async fn use_two_args_mix_fixture_inject_both(two_args_mix_fixture: impl Future<Output=u32>) {
    assert_eq!(31, two_args_mix_fixture.await);
}