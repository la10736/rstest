use rstest::*;


#[fixture]
fn double(first: u32, second: &str) -> u32 {
    0
}


#[rstest(double("pippo"))]
fn should_show_type_error(double: u32) {}
