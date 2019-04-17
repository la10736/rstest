use rstest::rstest_parametrize;


#[rstest_parametrize(a, b
    case(45))
]
fn one_more_arg(a: u32) {
    assert!(true)
}

