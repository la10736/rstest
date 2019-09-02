use rstest::rstest_parametrize;

struct A;
#[derive(Debug)]
struct D;

#[rstest_parametrize(u,a,d,
    case(42, A{}, D{})
    ::trace::notrace(a))
]
fn should_fail(u: u32, a: A, d: D) {
    assert!(false);
}
