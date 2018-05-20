#![feature(proc_macro)]
extern crate rstest;

use rstest::rstest_parametrize;

#[rstest_parametrize(
    expected,
    case(true),
    case(false)
)]
fn bool(expected: bool) {
    assert!(expected);
}