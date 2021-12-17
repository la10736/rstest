use rstest_reuse;

mod foo {
    mod bar {
        use rstest::rstest;
        use rstest_reuse::{self, *};

        #[template(local)]
        #[rstest(a, case(1))]
        pub fn local_template(a: i32) {}

        #[apply(local_template)]
        fn it_works(a: u32) {
            assert!(a == 1);
        }
    }
    mod baz {
        use rstest::rstest;
        use rstest_reuse::{self, *};

        #[template(local)]
        #[rstest(a, case(2))]
        pub fn local_template(a: i32) {}

        #[apply(local_template)]
        fn it_works(a: u32) {
            assert!(a == 2);
        }
    }
}
