use rstest_reuse;

use rstest::rstest;
use rstest_reuse::*;

#[template]
#[rstest]
#[case(2, 2)]
#[case(4/2, 2)]
fn copy_cases(#[case] a: u32, #[case] b: u32) {}

#[apply(copy_cases)]
fn it_works(a: u32, b: u32) {
    assert!(a == b);
}
