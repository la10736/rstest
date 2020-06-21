use rstest_reuse;

#[macro_use]
mod template {
    use rstest::rstest;
    use rstest_reuse::template;

    #[template]
    #[rstest(a,  b, case(2, 2), case(4/2, 2))]
    fn two_simple_cases(a: u32, b: u32) {}
}

mod user {
    use rstest::rstest;
    use rstest_reuse::apply;

    #[apply(two_simple_cases)]
    fn it_works(a: u32, b: u32) {
        assert!(a == b);
    }
}
