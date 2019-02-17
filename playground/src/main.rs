#![cfg_attr(feature = "trace_all", feature(specialization))]
extern crate rstest;

use rstest::{rstest, rstest_parametrize};
use std::fmt::Debug;

#[rstest_parametrize(condition, case(Unwrap(r#"vec![1,2,3].contains(2)"#)))]
fn example(condition: bool) {
    assert!(condition)
}
