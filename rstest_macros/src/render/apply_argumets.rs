use quote::ToTokens;
use syn::{parse_quote, FnArg, Generics, Lifetime, Signature};

use crate::{
    parse::{future::ImplFutureArg, ArgumentsInfo},
    refident::MaybeIdent,
};

pub(crate) trait ApplyArgumets<R: Sized = ()> {
    fn apply_argumets(&mut self, arguments: &ArgumentsInfo) -> R;
}

impl ApplyArgumets<Option<Lifetime>> for FnArg {
    fn apply_argumets(&mut self, arguments: &ArgumentsInfo) -> Option<Lifetime> {
        if self
            .maybe_ident()
            .map(|id| arguments.is_future(id))
            .unwrap_or_default()
        {
            self.impl_future_arg()
        } else {
            None
        }
    }
}

fn move_generic_list(data: &mut Generics, other: Generics) {
    data.lt_token = data.lt_token.or(other.lt_token);
    data.params = other.params;
    data.gt_token = data.gt_token.or(other.gt_token);
}

fn extend_generics_with_lifetimes<'a, 'b>(
    generics: impl Iterator<Item = &'a syn::GenericParam>,
    lifetimes: impl Iterator<Item = &'b syn::Lifetime>,
) -> Generics {
    let all = lifetimes
        .map(|lt| lt as &dyn ToTokens)
        .chain(generics.map(|gp| gp as &dyn ToTokens));
    parse_quote! {
                <#(#all),*>
    }
}

impl ApplyArgumets for Signature {
    fn apply_argumets(&mut self, arguments: &ArgumentsInfo) {
        let new_lifetimes = self
            .inputs
            .iter_mut()
            .filter_map(|arg| arg.apply_argumets(arguments))
            .collect::<Vec<_>>();
        if !new_lifetimes.is_empty() || !self.generics.params.is_empty() {
            let new_generics =
                extend_generics_with_lifetimes(self.generics.params.iter(), new_lifetimes.iter());
            move_generic_list(&mut self.generics, new_generics);
        }
    }
}

#[cfg(test)]
mod should {
    use super::*;
    use crate::test::{assert_eq, *};
    use syn::ItemFn;

    #[rstest]
    #[case("fn simple(a: u32) {}")]
    #[case("fn more(a: u32, b: &str) {}")]
    #[case("fn gen<S: AsRef<str>>(a: u32, b: S) {}")]
    #[case("fn attr(#[case] a: u32, #[values(1,2)] b: i32) {}")]
    fn no_change(#[case] item_fn: &str) {
        let mut item_fn: ItemFn = item_fn.ast();
        let orig = item_fn.clone();

        item_fn.sig.apply_argumets(&ArgumentsInfo::default());

        assert_eq!(orig, item_fn)
    }

    #[rstest]
    #[case::simple(
        "fn f(a: u32) {}",
        &["a"],
        "fn f(a: impl std::future::Future<Output = u32>) {}"
    )]
    #[case::more_than_one(
        "fn f(a: u32, b: String, c: std::collection::HashMap<usize, String>) {}",
        &["a", "b", "c"],
        r#"fn f(a: impl std::future::Future<Output = u32>, 
                b: impl std::future::Future<Output = String>, 
                c: impl std::future::Future<Output = std::collection::HashMap<usize, String>>) {}"#,
    )]
    #[case::just_one(
        "fn f(a: u32, b: String) {}",
        &["b"],
        r#"fn f(a: u32, 
                b: impl std::future::Future<Output = String>) {}"#
    )]
    #[case::generics(
        "fn f<S: AsRef<str>>(a: S) {}",
        &["a"],
        "fn f<S: AsRef<str>>(a: impl std::future::Future<Output = S>) {}"
    )]
    fn replace_future_basic_type(
        #[case] item_fn: &str,
        #[case] futures: &[&str],
        #[case] expected: &str,
    ) {
        let mut item_fn: ItemFn = item_fn.ast();
        let expected: ItemFn = expected.ast();

        let mut arguments = ArgumentsInfo::default();
        futures
            .into_iter()
            .for_each(|&f| arguments.add_future(ident(f)));

        item_fn.sig.apply_argumets(&arguments);

        assert_eq!(expected, item_fn)
    }

    #[rstest]
    #[case::base(
        "fn f(ident_name: &u32) {}",
        &["ident_name"],
        "fn f<'_ident_name>(ident_name: impl std::future::Future<Output = &'_ident_name u32>) {}"
    )]
    #[case::lifetime_already_exists(
        "fn f<'b>(a: &'b u32) {}",
        &["a"],
        "fn f<'b>(a: impl std::future::Future<Output = &'b u32>) {}"
    )]
    #[case::some_other_generics(
        "fn f<'b, IT: Iterator<Item=String + 'b>>(a: &u32, it: IT) {}",
        &["a"],
        "fn f<'_a, 'b, IT: Iterator<Item=String + 'b>>(a: impl std::future::Future<Output = &'_a u32>, it: IT) {}"
    )]
    fn replace_reference_type(
        #[case] item_fn: &str,
        #[case] futures: &[&str],
        #[case] expected: &str,
    ) {
        let mut item_fn: ItemFn = item_fn.ast();
        let expected: ItemFn = expected.ast();

        let mut arguments = ArgumentsInfo::default();
        futures
            .into_iter()
            .for_each(|&f| arguments.add_future(ident(f)));

        item_fn.sig.apply_argumets(&arguments);

        assert_eq!(expected, item_fn)
    }
}