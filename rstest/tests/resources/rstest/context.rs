use rstest::*;

#[rstest]
#[case::description(42)]
fn with_case(#[context] ctx: Context, #[case] _c: u32) {
    assert_eq!("with_case", ctx.name);
    assert_eq!(Some("description"), ctx.description);
    assert_eq!(Some(0), ctx.case);
}

#[rstest]
fn without_case(#[context] ctx: Context) {
    assert_eq!("without_case", ctx.name);
    assert_eq!(None, ctx.description);
    assert_eq!(None, ctx.case);
}
