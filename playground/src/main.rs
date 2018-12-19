#![cfg_attr(feature = "trace_all", feature(specialization))]
extern crate rstest;

use rstest::{rstest, rstest_parametrize};
use std::fmt::Debug;

fn pippo() -> u32 {
    42
}

struct Foo(i32);

fn foo() -> Foo {
    Foo(42)
}
fn foo2() -> Foo {
    Foo(22)
}

#[rstest]
fn ok(pippo: u32) {
    assert_eq!(42, pippo);
}

#[rstest]
fn fail(pippo: u32) {
    assert_eq!(43, pippo);
}

#[rstest]
fn ok_foo(foo: Foo) {
    assert_eq!(42, foo.0);
}

#[rstest]
fn fail_foo(foo: Foo) {
    assert_eq!(43, foo.0);
}

#[rstest(autotrace::notrace(foo, foo2))]
fn fail_exclude_autotrace(pippo: u32, foo: Foo, foo2: Foo) {

    assert_eq!(42, pippo);
    assert_eq!(42, foo.0);
    assert_eq!(22, foo2.0);
    assert!(false);
}

#[rstest(autotrace)]
fn fail_autotrace_no_fixtures() {
    assert!(false);
}

#[derive(Debug)]
struct Baz(i32);
struct NoDeb(i32);

#[rstest_parametrize(i, baz, no_deb,
    case(1, Unwrap("Baz(2)"), Unwrap("NoDeb(1)")),
    case(3, Unwrap("Baz(4)"), Unwrap("NoDeb(2)"))
    :: autotrace :: notrace(no_deb)
)]
fn param_fail(i: i32, baz: Baz, no_deb: NoDeb) {
    assert_eq!(1, 2);
}

/// Should reject something like this
/// #[rstest_parametrize(first, case(1, 2), second, case(2, 4))]

#[rstest_parametrize(first, second, case(1, 2), case(2, 4))]
fn half(first: i32, second: i32) {
    assert_eq!(first*2, second);
}


trait DisplayString {
    fn display_string(&self) -> String {
        format!("@{:p}", self)
    }
}

#[cfg(feature = "trace_all")]
impl<T> DisplayString for T {}

impl<T: Debug> DisplayString for T {
    fn display_string(&self) -> String {
        format!("{:?}", self)
    }
}

fn dump<V: DisplayString>(name: &str, val: &V) {
    eprintln!("INPUT: {} = {}", name, val.display_string());
}

fn main() {
    let v = 32;
    struct O {};

    dump(stringify!(v), &v);
    #[cfg(feature = "trace_all")]
        {
            let o = O {};
            dump(stringify!(o), &o);
        }
}
