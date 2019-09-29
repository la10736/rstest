use rstest::rstest_parametrize;

#[rstest_parametrize(
    condition,
    case("".len()==0),
    case(vec![1,5,7].contains(&23)),
)]
fn arbitrary(condition: bool) {
    assert!(condition);
}
