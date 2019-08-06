use rstest::rstest_matrix;

#[rstest_matrix(f => [42])]
struct Foo;

#[rstest_matrix(f => [42])]
impl Foo {}

#[rstest_matrix(f => [42])]
mod mod_baz {}

