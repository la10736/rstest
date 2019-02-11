#![cfg_attr(feature = "trace_all", feature(specialization))]
extern crate rstest;

use rstest::{rstest, rstest_parametrize};
use std::fmt::Debug;

#[rstest_parametrize(a, case(1,2))]
fn example(a: u32) {
    assert!(true)
}
