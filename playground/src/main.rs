use rstest::*;
fn valid_user(name: &str, age: u8) -> bool {
    true
}

#[rstest]
fn should_accept_all_corner_cases(
    #[values("J", "A", "A________________________________________21")] name: &str,
    #[values(14, 100)] age: u8,
) {
    assert!(valid_user(name, age))
}
