use rstest::*;

#[fixture]
fn fixture() -> u32 {
    42
}

#[rstest_parametrize(
    expected, val,
    case(45, 3),
    case(44, 2)
)]
fn sum(expected: u32, fixture: u32, val: u32) {

    assert_eq!(expected, fixture + val);
}
