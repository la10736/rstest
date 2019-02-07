use rstest::rstest_parametrize;

struct S;

#[rstest_parametrize(s,
    case(Unwrap("S{}"))
    ::trace
)]
fn test_function(s: S) {}
