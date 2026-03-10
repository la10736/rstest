#[rstest_reuse::template_group]
pub mod template_example {
    use rstest::rstest;
    use rstest_reuse::replace;

    #[replace]
    fn prepare() -> u32 {
        unimplemented!()
    }

    #[rstest]
    fn test() {
        let value = prepare();
        assert_eq!(value, 0);
    }
}

#[rstest_reuse::apply_group(template_example)]
pub mod inner {
    fn prepare() -> u32 {
        0
    }
}
