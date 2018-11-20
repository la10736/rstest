#![cfg_attr(feature = "trace_all", feature(specialization))]
extern crate rstest;

use rstest::rstest;
use std::fmt::Debug;

fn pippo() -> u32 {
    42
}

struct Foo(i32);

fn foo() -> Foo {
    Foo(42)
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
