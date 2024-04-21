use quote::ToTokens;
use syn::{visit_mut::VisitMut, FnArg, Ident, ItemFn};

use crate::{error::ErrorsVec, utils::attr_is};

use super::extract_argument_attrs;

pub(crate) fn extract_by_ref(item_fn: &mut ItemFn) -> Result<Vec<Ident>, ErrorsVec> {
    let mut extractor = ByRefFunctionExtractor::default();
    extractor.visit_item_fn_mut(item_fn);
    extractor.take()
}

/// Simple struct used to visit function attributes and extract by_ref args
#[derive(Default)]
struct ByRefFunctionExtractor {
    by_refs: Vec<Ident>,
    errors: Vec<syn::Error>,
}

impl ByRefFunctionExtractor {
    pub(crate) fn take(self) -> Result<Vec<Ident>, ErrorsVec> {
        if self.errors.is_empty() {
            Ok(self.by_refs)
        } else {
            Err(self.errors.into())
        }
    }
}

impl VisitMut for ByRefFunctionExtractor {
    fn visit_fn_arg_mut(&mut self, node: &mut FnArg) {
        if matches!(node, FnArg::Receiver(_)) {
            return;
        }
        match extract_argument_attrs(
            node,
            |a| attr_is(a, "by_ref"),
            |arg, name| Ok((arg, name.clone())),
        )
        .collect::<Result<Vec<_>, _>>()
        {
            Ok(refs) => match refs.len().cmp(&1) {
                std::cmp::Ordering::Equal => {
                    self.by_refs.push(refs[0].1.clone());
                }
                std::cmp::Ordering::Greater => {
                    self.errors
                        .extend(refs.iter().skip(1).map(|(attr, _ident)| {
                            syn::Error::new_spanned(
                                attr.into_token_stream(),
                                "Cannot use #[by_ref] more than once.".to_owned(),
                            )
                        }));
                }
                std::cmp::Ordering::Less => {}
            },
            Err(e) => {
                self.errors.push(e);
            }
        };
    }
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
    fn not_change_anything_if_no_by_ref_attribute_found(#[case] item_fn: &str) {
        let mut item_fn: ItemFn = item_fn.ast();
        let orig = item_fn.clone();

        let by_refs = extract_by_ref(&mut item_fn).unwrap();

        assert_eq!(orig, item_fn);
        assert!(by_refs.is_empty());
    }

    #[rstest]
    #[case::simple("fn f(#[by_ref] a: &u32) {}", "fn f(a: &u32) {}", &["a"])]
    #[case::more_than_one(
        "fn f(#[by_ref] a: &u32, #[by_ref] b: &String, #[by_ref] c: &std::collection::HashMap<usize, String>) {}",
        r#"fn f(a: &u32, 
                b: &String, 
                c: &std::collection::HashMap<usize, String>) {}"#,
        &["a", "b", "c"])]
    fn extract(#[case] item_fn: &str, #[case] expected: &str, #[case] expected_refs: &[&str]) {
        let mut item_fn: ItemFn = item_fn.ast();
        let expected: ItemFn = expected.ast();

        let by_refs = extract_by_ref(&mut item_fn).unwrap();

        assert_eq!(expected, item_fn);
        assert_eq!(by_refs, to_idents!(expected_refs));
    }

    #[rstest]
    #[case::no_more_than_one("fn f(#[by_ref] #[by_ref] a: u32) {}", "more than once")]
    fn raise_error(#[case] item_fn: &str, #[case] message: &str) {
        let mut item_fn: ItemFn = item_fn.ast();

        let err = extract_by_ref(&mut item_fn).unwrap_err();

        assert_in!(format!("{:?}", err), message);
    }
}
