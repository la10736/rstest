use rstest::rstest;
use rstest_reuse::apply;
use rstest_reuse::template;

mod foo {
    use rstest::rstest;
    use rstest_reuse::apply;
    use rstest_reuse::template;

    #[template]
    #[export]
    #[hidden]
    #[rstest]
    #[case("bar")]
    fn public_template(#[case] s: &str) {}

    #[apply(public_template)]
    fn test(#[case] s: &str) {
        assert_eq!(s, "bar");
    }
}

use foo::public_template;

#[apply(public_template)]
fn public_test(#[case] s: &str) {
    assert_eq!(s, "bar");
}

#[template]
#[hidden]
#[rstest]
#[case("bar")]
fn private_template(#[case] s: &str) {}

#[apply(private_template)]
fn private_test(#[case] s: &str) {
    assert_eq!(s, "bar");
}
