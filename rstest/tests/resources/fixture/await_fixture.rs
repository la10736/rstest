use std::io::prelude::*;

use rstest::*;

#[fixture]
async fn async_u32() -> u32 {
    42
}

#[fixture]
async fn nest_fixture(#[await_future] async_u32: u32) -> u32 {
    async_u32
}

#[fixture(fortytwo = async { 42 })]
async fn nest_fixture_with_default(#[await_future] fortytwo: u32) -> u32 {
    fortytwo
}

#[rstest]
async fn default_is_async() {
    assert_eq!(42, async_u32::default().await);
}

#[rstest]
async fn use_async_nest_fixture_default(#[await_future] nest_fixture: u32) {
    assert_eq!(42, nest_fixture);
}

#[rstest(nest_fixture(async { 24 }))]
async fn use_async_nest_fixture_injected(#[await_future] nest_fixture: u32) {
    assert_eq!(24, nest_fixture);
}

#[rstest]
async fn use_async_nest_fixture_with_default(#[await_future] nest_fixture_with_default: u32) {
    assert_eq!(42, nest_fixture_with_default);
}

#[rstest]
async fn use_async_fixture(#[await_future] async_u32: u32) {
    assert_eq!(42, async_u32);
}

#[fixture]
async fn async_impl_output() -> impl Read {
    std::io::Cursor::new(vec![1, 2, 3, 4, 5])
}

#[rstest]
async fn use_async_impl_output<T: Read>(#[await_future] async_impl_output: T) {
    let reader = async_impl_output;
}

#[fixture(four = async { 4 }, two = 2)]
async fn two_args_mix_fixture(#[await_future] four: u32, two: u32) -> u32 {
    four * 10 + two
}

#[rstest]
async fn use_two_args_mix_fixture(#[await_future] two_args_mix_fixture: u32) {
    assert_eq!(42, two_args_mix_fixture);
}

#[rstest(two_args_mix_fixture(async { 5 }))]
async fn use_two_args_mix_fixture_inject_first(#[await_future] two_args_mix_fixture: u32) {
    assert_eq!(52, two_args_mix_fixture);
}

#[rstest(two_args_mix_fixture(async { 3 }, 1))]
async fn use_two_args_mix_fixture_inject_both(#[await_future] two_args_mix_fixture: u32) {
    assert_eq!(31, two_args_mix_fixture);
}
