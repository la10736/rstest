use quote::ToTokens;
use syn::{visit_mut::VisitMut, FnArg, Ident, ItemFn, PatType, Type};

use crate::{error::ErrorsVec, refident::MaybeType, utils::attr_is};

use super::extract_argument_attrs;

pub(crate) fn extract_futures(item_fn: &mut ItemFn) -> Result<Vec<Ident>, ErrorsVec> {
    let mut extractor = FutureFunctionExtractor::default();
    extractor.visit_item_fn_mut(item_fn);
    extractor.take()
}

pub(crate) trait MaybeFutureImplType {
    fn as_future_impl_type(&self) -> Option<&Type>;

    fn as_mut_future_impl_type(&mut self) -> Option<&mut Type>;
}

impl MaybeFutureImplType for FnArg {
    fn as_future_impl_type(&self) -> Option<&Type> {
        match self {
            FnArg::Typed(PatType { ty, .. }) if can_impl_future(ty.as_ref()) => Some(ty.as_ref()),
            _ => None,
        }
    }

    fn as_mut_future_impl_type(&mut self) -> Option<&mut Type> {
        match self {
            FnArg::Typed(PatType { ty, .. }) if can_impl_future(ty.as_ref()) => Some(ty.as_mut()),
            _ => None,
        }
    }
}

fn can_impl_future(ty: &Type) -> bool {
    use Type::*;
    !matches!(
        ty,
        Group(_)
            | ImplTrait(_)
            | Infer(_)
            | Macro(_)
            | Never(_)
            | Slice(_)
            | TraitObject(_)
            | Verbatim(_)
    )
}

/// Simple struct used to visit function attributes and extract future args to
/// implement the boilerplate.
#[derive(Default)]
struct FutureFunctionExtractor(Vec<Ident>, Vec<syn::Error>);
impl FutureFunctionExtractor {
    pub(crate) fn take(self) -> Result<Vec<Ident>, ErrorsVec> {
        if self.1.is_empty() {
            Ok(self.0)
        } else {
            Err(self.1.into())
        }
    }
}

impl VisitMut for FutureFunctionExtractor {
    fn visit_fn_arg_mut(&mut self, node: &mut FnArg) {
        if matches!(node, FnArg::Receiver(_)) {
            return;
        }
        match extract_argument_attrs(
            node,
            |a| attr_is(a, "future"),
            |arg, name| Ok((arg, name.clone())),
        )
        .collect::<Result<Vec<_>, _>>()
        {
            Ok(futures) => {
                if futures.len() > 1 {
                    self.1.extend(futures.iter().skip(1).map(|(attr, _ident)| {
                        syn::Error::new_spanned(
                            attr.into_token_stream(),
                            "Cannot use #[future] more than once.".to_owned(),
                        )
                    }));
                    return;
                } else if futures.len() == 1 {
                    match node.as_future_impl_type() {
                        Some(_) => self.0.push(futures[0].1.clone()),
                        None => self.1.push(syn::Error::new_spanned(
                            node.maybe_type().unwrap().into_token_stream(),
                            "This type cannot used to generete impl Future.".to_owned(),
                        )),
                    }
                }
            }
            Err(e) => {
                self.1.push(e);
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
    fn not_change_anything_if_no_future_attribute_found(#[case] item_fn: &str) {
        let mut item_fn: ItemFn = item_fn.ast();
        let orig = item_fn.clone();

        extract_futures(&mut item_fn).unwrap();

        assert_eq!(orig, item_fn)
    }

    #[rstest]
    #[case::simple("fn f(#[future] a: u32) {}", "fn f(a: u32) {}")]
    #[case::more_than_one(
        "fn f(#[future] a: u32, #[future] b: String, #[future] c: std::collection::HashMap<usize, String>) {}",
        r#"fn f(a: u32, 
                b: String, 
                c: std::collection::HashMap<usize, String>) {}"#,
    )]
    #[case::just_one(
        "fn f(a: u32, #[future] b: String) {}",
        r#"fn f(a: u32, b: String) {}"#
    )]
    fn extract(#[case] item_fn: &str, #[case] expected: &str) {
        let mut item_fn: ItemFn = item_fn.ast();
        let expected: ItemFn = expected.ast();

        extract_futures(&mut item_fn).unwrap();

        assert_eq!(expected, item_fn)
    }

    #[rstest]
    #[case::no_more_than_one("fn f(#[future] #[future] a: u32) {}", "more than once")]
    #[case::no_impl("fn f(#[future] a: impl AsRef<str>) {}", "generete impl Future")]
    #[case::no_slice("fn f(#[future] a: [i32]) {}", "generete impl Future")]
    fn raise_error(#[case] item_fn: &str, #[case] message: &str) {
        let mut item_fn: ItemFn = item_fn.ast();

        let err = extract_futures(&mut item_fn).unwrap_err();

        assert_in!(format!("{:?}", err), message);
    }
}
