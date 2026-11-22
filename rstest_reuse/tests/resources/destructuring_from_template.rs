use rstest_reuse;

mod cases {
    use rstest::{fixture, rstest};
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

    #[fixture]
    fn double(#[default(1)] v: u32) -> u32 {
        v * 2
    }

    #[template]
    #[rstest]
    #[case(42)]
    fn fixture_template(#[case] expected: u32, #[from(double)] #[with(21)] value: u32) {}

    // `#[from(double)]` doesn't name a template argument: the attributes come from the
    // template argument with the same name.
    #[apply(fixture_template)]
    fn renamed_fixture(#[case] expected: u32, #[from(double)] value: u32) {
        assert_eq!(expected, value);
    }
}
