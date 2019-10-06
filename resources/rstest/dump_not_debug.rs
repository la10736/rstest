use rstest::rstest;

struct S;

#[rstest(s,
    case(Unwrap("S{}"))
    ::trace
)]
fn test_function(s: S) {}
