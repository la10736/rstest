use rstest::rstest_parametrize;

#[rstest_parametrize(f, case(42))]
struct Foo;

#[rstest_parametrize(f, case(42))]
impl Foo {}

#[rstest_parametrize(f, case(42))]
mod mod_baz {}

