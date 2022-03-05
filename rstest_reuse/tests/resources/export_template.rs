use rstest_reuse;

mod foo {
    mod bar {
        use rstest::rstest;
        use rstest_reuse::{self, *};

        #[template]
        #[export]
        #[rstest]
        #[case("bar")]
        fn my_template(#[case] s: &str) {}

        #[apply(my_template)]
        fn test(#[case] s: &str) {
            assert_eq!("bar", s);
        }
    }
}

use rstest::rstest;
use rstest_reuse::apply;

#[apply(my_template)]
fn test(#[case] s: &str) {
    assert_eq!("bar", s);
}
