#![feature(proc_macro)]
extern crate rstest;

use rstest::rstest_parametrize;

#[rstest_parametrize(
    expected, val,
    case(4, 3),
    case(3, 2)
)]
fn add_test(expected: u32, mut val: u32) {
    val += 1;

    assert_eq!(expected, val);
}