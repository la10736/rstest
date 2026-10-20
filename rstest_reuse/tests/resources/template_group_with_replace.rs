#[rstest_reuse::template_group(template_inner)]
mod inner1 {
    use rstest::rstest;
    use rstest_reuse::replace;

    #[replace]
    fn prepare() -> u8 {
        unimplemented!()
    }

    #[rstest]
    fn test() {
        let value = prepare();
        assert!(value > 0, "{} is not greater than 0", value)
    }
}

#[rstest_reuse::apply_group(template_inner)]
mod inner {
    fn prepare() -> u8 {
        1
    }
}
