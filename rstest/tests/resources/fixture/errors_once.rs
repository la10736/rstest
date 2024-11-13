use rstest::*;

#[fixture]
#[once]
async fn error_async_once_fixture() {
}

#[fixture]
#[once]
fn error_generics_once_fixture<T: core::fmt::Debug>() -> T {
    42
}

#[fixture]
#[once]
fn error_generics_once_fixture() -> impl Iterator<Item = u32> {
    core::iter::once(42)
}

#[fixture]
#[once]
fn error_once_fixture_not_sync() -> core::cell::Cell<u32> {
    core::cell::Cell::new(42)
}
