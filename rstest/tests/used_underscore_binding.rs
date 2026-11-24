#![forbid(clippy::used_underscore_binding)]

use rstest::rstest;

#[rstest]
#[trace]
#[case::narrow(0_i32)]
#[case::wide(0_i64)]
fn infer_type_from_unused_case<T>(#[case] _marker: T, #[values(-2, 2)] scale: i8)
where
    T: From<i32> + Into<i64>,
{
    let value: i64 = T::from(i32::from(scale)).into();
    assert_eq!(value, i64::from(scale));
}
