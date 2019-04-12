use rstest::rstest_parametrize;


fn incorrect(v: u32) -> u32 {
    v * 2
}

#[rstest_parametrize(a,
    case(incorrect(45)))
]
fn error_in_case_syntax(a: u32) {
    assert!(true)
}

#[rstest_parametrize(a,
case(Unwrap("45")),
)
]
fn deprecate(a: u32) {
    assert!(false)
}
