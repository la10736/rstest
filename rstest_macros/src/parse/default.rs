use syn::{visit_mut::VisitMut, ItemFn};

use crate::error::ErrorsVec;

use super::just_once::{AttrBuilder, JustOnceFnAttributeExtractor, Validator};

pub(crate) fn extract_default(item_fn: &mut ItemFn) -> Result<bool, ErrorsVec> {
    let mut extractor = JustOnceFnAttributeExtractor::<DefaultBuilder>::new("default");

    extractor.visit_item_fn_mut(item_fn);
    extractor.take().map(|inner| inner.is_some())
}

struct DefaultBuilder;

impl AttrBuilder<ItemFn> for DefaultBuilder {
    type Out = ();

    fn build(_attr: syn::Attribute, _ident: &ItemFn) -> syn::Result<Self::Out> {
        Ok(())
    }
}

impl Validator<ItemFn> for DefaultBuilder {
    // Maybe check if all items in the fn has default or not, or let compiler make that error
}

#[cfg(test)]
mod should {
    use super::*;
    use crate::test::{assert_eq, *};
    use rstest_test::assert_in;

    #[rstest]
    #[case("fn simple(a: u32) {}")]
    #[case("fn more(a: u32, b: &str) {}")]
    #[case("fn gen<S: AsRef<str>>(a: u32, b: S) {}")]
    #[case("fn attr(#[case] a: u32, #[values(1,2)] b: i32) {}")]
    fn not_change_anything_if_no_default_attribute_found(#[case] item_fn: &str) {
        let mut item_fn: ItemFn = item_fn.ast();
        let orig = item_fn.clone();

        let composed_tuple!(default) = extract_default(&mut item_fn).unwrap();

        assert_eq!(orig, item_fn);
        assert!(!default);
    }

    #[rstest]
    #[case::simple("fn f(a: u32) {}", "fn f(a: u32) {}", false)]
    #[case::global_awt("#[default] fn f(a: u32) {}", "fn f(a: u32) {}", true)]
    #[case::global_awt_with_inner_function(
        "#[default] fn f(a: u32) { fn g(){} }",
        "fn f(a: u32) { fn g(){} }",
        true
    )]
    fn extract(#[case] item_fn: &str, #[case] expected: &str, #[case] expected_default: bool) {
        let mut item_fn: ItemFn = item_fn.ast();
        let expected: ItemFn = expected.ast();

        let default = extract_default(&mut item_fn).unwrap();

        assert_eq!(expected, item_fn);
        assert_eq!(expected_default, default);
    }

    #[rstest]
    #[case::base(r#"#[default] fn f(a: u32) {}"#, r#"fn f(a: u32) {}"#)]
    #[case::two(
        r#"
        #[default]
        #[default] 
        fn f(a: u32) {}
        "#,
        r#"fn f(a: u32) {}"#
    )]
    #[case::inner(
        r#"
        #[one]
        #[default] 
        #[two]
        fn f(a: u32) {}
        "#,
        r#"
        #[one]
        #[two]
        fn f(a: u32) {}
        "#
    )]
    fn remove_all_default_attributes(#[case] item_fn: &str, #[case] expected: &str) {
        let mut item_fn: ItemFn = item_fn.ast();
        let expected: ItemFn = expected.ast();

        let _ = extract_default(&mut item_fn);

        assert_eq!(item_fn, expected);
    }

    #[rstest]
    #[case::no_more_than_one("#[default] #[default] fn f(a: u32) {}", "more than once")]
    fn raise_error(#[case] item_fn: &str, #[case] message: &str) {
        let mut item_fn: ItemFn = item_fn.ast();

        let err = extract_default(&mut item_fn).unwrap_err();

        assert_in!(format!("{:?}", err), message);
    }
}
