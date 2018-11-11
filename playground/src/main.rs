#![feature(specialization)]
extern crate rstest;

use rstest::rstest;
use std::fmt::{Debug, Formatter, Error};
use std::fmt::Display;

fn pippo() -> u32 {
    42
}

#[rstest]
fn ok(pippo: u32) {
    assert_eq!(42, pippo);
}

#[rstest]
fn fail(pippo: u32) {
    assert_eq!(43, pippo);
}

trait DisplayString {
    fn display_string(&self) -> String {
        format!("@{:p}", self)
    }
}

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
    let o = O {};

    dump(stringify!(v), &v);
    dump(stringify!(o), &o);
}
