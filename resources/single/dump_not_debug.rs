use rstest::rstest;

struct S;

fn fixture() -> S { S {} }

#[rstest(trace)]
fn test_function(fixture: S) {}
