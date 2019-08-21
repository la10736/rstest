use rstest::*;

struct S;

#[fixture]
fn fixture() -> S { S {} }

#[rstest(::trace)]
fn test_function(fixture: S) {}
