use rstest::*;

#[rstest]
#[case::first_no_dump("Please don't trace me")]
#[case::dump_me("Trace it!")]
#[trace]
#[case::last_no_dump("Please don't trace me")]
fn cases(#[case] s: &str) {
    assert!(false);
}
