#[rstest_reuse::template_group(template_inner)]
mod inner1 {
    use rstest::{fixture, rstest};
    use rstest_reuse::replace;

    #[fixture]
    #[replace]
    fn fixture() -> u8 {
        unimplemented!()
    }

    #[rstest]
    fn test(fixture: u8) {
        assert!(fixture > 0, "{} is not greater than 0", fixture)
    }
}

#[rstest_reuse::apply_group(template_inner)]
mod inner {
    #[fixture]
    fn fixture() -> u8 {
        1
    }
}
