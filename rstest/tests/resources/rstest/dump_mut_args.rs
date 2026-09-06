use rstest::*;

#[fixture]
fn fu32() -> u32 {
    42
}

#[rstest]
#[trace]
fn single_mut_fail(mut fu32: u32) {
    fu32 += 1;
    assert!(false);
}

#[rstest]
#[case(42, "str")]
#[trace]
fn cases_mut_fail(#[case] mut u: u32, #[case] s: &str) {
    u += 1;
    assert!(false);
}

#[rstest]
#[trace]
fn matrix_mut_fail(#[values(1, 2)] mut u: u32, #[values("a", "b")] s: &str) {
    u += 1;
    assert!(false);
}
