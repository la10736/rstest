use rstest::rstest_parametrize;

#[rstest_parametrize(a,
    case(incorrect(some)))
]
fn error_in_case_syntax(a: u32) {
    assert!(true)
}
