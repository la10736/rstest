use rstest::*;

#[fixture]
fn first() -> u32 {
    42
}

#[fixture]
fn second() -> &'static str {
    "foo"
}

#[fixture]
fn double(first: u32, second: &str) -> u32 {
    0
}


#[rstest(double("pippo"))]
fn should_show_type_error(double: u32) {}
