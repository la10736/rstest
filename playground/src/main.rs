use rstest::*;
use rstest_reuse::{self, *};

// Here we define the template. This define
// * The test set name to `two_simple_cases`
// * cases: here two cases that feed the `a`, `b` values
#[template]
#[rstest]
#[case(2, 2)]
#[case(4/2, 2)]
fn two_simple_cases(#[case] a: u32, #[case] b: u32) {}

// Here we apply the `two_simple_cases` template: That is expanded in
// #[template]
// #[rstest(a,  b,
//     case(2, 2),
//     case(4/2, 2),
//     )
// ]
// fn it_works(a: u32, b: u32) {
//     assert!(a == b);
// }
#[apply(two_simple_cases)]
fn it_works(#[case] a: u32, #[case] b: u32) {
    assert!(a == b);
}

// Here we reuse the `two_simple_cases` template to create two other tests
#[apply(two_simple_cases)]
#[should_panic]
fn it_fail(#[case] a: u32, #[case] b: u32) {
    assert!(a != b);
}

#[fixture(a = 42)]
fn f(a: u32) -> u32 {
    a
}

#[fixture(f(42))]
fn fix(f: u32) -> u32 {
    f
}

#[rstest]
fn aaa(fix: u32) {
    assert_eq!(42, fix);
}

use std::net::SocketAddr;

#[rstest]
#[case("1.2.3.4:8080", 8080)]
#[case("127.0.0.1:9000", 9000)]
fn check_port(#[case] addr: SocketAddr, #[case] expected: u16) {
    assert_eq!(expected, addr.port());
}
