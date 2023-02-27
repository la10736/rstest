use rstest::*;

#[fixture]
#[awt]
async fn two_args_mix_fixture(
    #[future]
    #[default(async { 4 })]
    four: u32,
    #[default(2)] two: u32,
) -> u32 {
    four * 10 + two
}

// #[rstest]
// #[awt]
// async fn use_two_args_mix_fixture(#[future] two_args_mix_fixture: u32) {
//     assert_eq!(42, two_args_mix_fixture);
// }

// #[rstest]
// #[awt]
// async fn use_two_args_mix_fixture_inject_first(
//     #[future]
//     #[with(async { 5 })]
//     two_args_mix_fixture: u32,
// ) {
//     assert_eq!(52, two_args_mix_fixture);
// }

#[rstest]
#[awt]
async fn use_two_args_mix_fixture_inject_both(
    #[future]
    #[with(async { 3 }, 1)]
    two_args_mix_fixture: u32,
) {
    assert_eq!(31, two_args_mix_fixture);
}
