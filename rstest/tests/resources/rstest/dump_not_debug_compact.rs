struct S;
#[rustfmt::skip] mod _skip_format {
use rstest::*; use super::*;

#[fixture]
fn fixture() -> S { S {} }

#[rstest(
    ::trace)]
fn single(fixture: S) {}

#[rstest(s,
    case(S{})
    ::trace)]
fn cases(s: S) {}

#[rstest(
    s => [S{}]
    ::trace)]
fn matrix(s: S) {}
}
