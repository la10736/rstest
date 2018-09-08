extern crate rstest;

use rstest::rstest_parametrize;

#[rstest_parametrize(
    expected, input,
    case(4, 5),
    case(3, 2),
    case(3, 3)
)]
#[should_panic]
fn fail(expected: i32, input: i32) {
    assert_eq!(expected, input);
}
