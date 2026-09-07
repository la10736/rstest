use rstest_reuse;

mod cases {
    use rstest::rstest;
    use rstest_reuse::*;

    #[template]
    #[rstest]
    #[case((2, 21))]
    #[case((6, 7))]
    fn tuple_template(#[case] pair: (u32, u32)) {}

    // A destructured argument has no identifier of its own, so it is linked to
    // the template argument with `#[from(...)]`.
    #[apply(tuple_template)]
    fn destruct_tuple(#[from(pair)] (a, b): (u32, u32)) {
        assert!(a * b == 42);
    }

    struct S {
        a: u32,
        b: u32,
    }

    #[template]
    #[rstest]
    #[case(S { a: 2, b: 21 })]
    #[case(S { a: 6, b: 7 })]
    fn struct_template(#[case] s: S) {}

    #[apply(struct_template)]
    fn destruct_struct(#[from(s)] S { a, b }: S) {
        assert!(a * b == 42);
    }
}
