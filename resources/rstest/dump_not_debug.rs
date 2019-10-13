use rstest::*;

struct S;

#[fixture]
fn fixture() -> S { S {} }

#[rstest(::trace)]
fn single(fixture: S) {}

#[rstest(s,
    case(S{})
    ::trace
)]
fn cases(s: S) {}
