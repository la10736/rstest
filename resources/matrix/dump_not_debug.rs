use rstest::rstest_matrix;

struct S;

#[rstest_matrix(
    s => [S{}]
    ::trace
)]
fn test_function(s: S) {}
