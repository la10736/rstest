#![deny(missing_docs)]

//! We should provide also a docs for the crate itself if we set `#![deny(missing_docs)]`.

use rstest::rstest;
use rstest_reuse::{self, *};

#[template]
#[export]
#[hidden]
#[rstest(a,  b, case(2, 2), case(4/2, 2))]
fn public_two_simple_cases(a: u32, b: u32) {}

#[apply(public_two_simple_cases)]
fn public_it_works(a: u32, b: u32) {
    assert!(a == b);
}

#[template]
#[hidden]
#[rstest(a,  b, case(2, 2), case(4/2, 2))]
fn private_two_simple_cases(a: u32, b: u32) {}

#[apply(private_two_simple_cases)]
fn private_it_works(a: u32, b: u32) {
    assert!(a == b);
}
