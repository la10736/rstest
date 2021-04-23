use proc_macro2::{Span, TokenStream};
use syn::{parse_quote, Ident, ItemFn};

use quote::quote;

use super::{generics_clean_up, render_exec_call, resolve_aruments};
use crate::parse::fixture::FixtureInfo;
use crate::resolver::{self, Resolver};
use crate::utils::{fn_args, fn_args_idents};

pub(crate) fn render<'a>(fixture: ItemFn, info: FixtureInfo) -> TokenStream {
    let name = &fixture.sig.ident;
    let asyncness = &fixture.sig.asyncness.clone();
    let vargs = fn_args_idents(&fixture).cloned().collect::<Vec<_>>();
    let args = &vargs;
    let orig_args = &fixture.sig.inputs;
    let orig_attrs = &fixture.attrs;
    let generics = &fixture.sig.generics;
    let default_output = info
        .attributes
        .extract_default_type()
        .unwrap_or(fixture.sig.output.clone());
    let default_generics =
        generics_clean_up(&fixture.sig.generics, std::iter::empty(), &default_output);
    let default_where_clause = &default_generics.where_clause;
    let where_clause = &fixture.sig.generics.where_clause;
    let output = &fixture.sig.output;
    let visibility = &fixture.vis;
    let resolver = (
        resolver::fixtures::get(info.data.fixtures()),
        resolver::values::get(info.data.values()),
    );
    let generics_idents = generics
        .type_params()
        .map(|tp| &tp.ident)
        .cloned()
        .collect::<Vec<_>>();
    let inject = resolve_aruments(fixture.sig.inputs.iter(), &resolver, &generics_idents);
    let partials =
        (1..=orig_args.len()).map(|n| render_partial_impl(&fixture, n, &resolver, &info));

    let call_get = render_exec_call(parse_quote! { Self::get }, args, asyncness.is_some());
    let call_impl = render_exec_call(parse_quote! { #name }, args, asyncness.is_some());

    quote! {
        #[allow(non_camel_case_types)]
        #visibility struct #name {}

        impl #name {
            #(#orig_attrs)*
            #[allow(unused_mut)]
            pub #asyncness fn get #generics (#orig_args) #output #where_clause {
                #call_impl
            }

            pub #asyncness fn default #default_generics () #default_output #default_where_clause {
                #inject
                #call_get
            }

            #(#partials)*
        }

        #[allow(dead_code)]
        #fixture
    }
}

fn render_partial_impl(
    fixture: &ItemFn,
    n: usize,
    resolver: &impl Resolver,
    info: &FixtureInfo,
) -> TokenStream {
    let output = info
        .attributes
        .extract_partial_type(n)
        .unwrap_or(fixture.sig.output.clone());

    let generics = generics_clean_up(&fixture.sig.generics, fn_args(fixture).take(n), &output);
    let where_clause = &generics.where_clause;
    let asyncness = &fixture.sig.asyncness;

    let genercs_idents = generics
        .type_params()
        .map(|tp| &tp.ident)
        .cloned()
        .collect::<Vec<_>>();
    let inject = resolve_aruments(fixture.sig.inputs.iter().skip(n), resolver, &genercs_idents);

    let sign_args = fn_args(fixture).take(n);
    let fixture_args = fn_args_idents(fixture).cloned().collect::<Vec<_>>();
    let name = Ident::new(&format!("partial_{}", n), Span::call_site());

    let call_get = render_exec_call(
        parse_quote! { Self::get },
        &fixture_args,
        asyncness.is_some(),
    );

    quote! {
        #[allow(unused_mut)]
        pub #asyncness fn #name #generics (#(#sign_args),*) #output #where_clause {
            #inject
            #call_get
        }
    }
}

#[cfg(test)]
mod should {
    use syn::{
        parse::{Parse, ParseStream},
        parse2, parse_str, ItemFn, ItemImpl, ItemStruct, Result,
    };

    use crate::parse::{Attribute, Attributes};

    use super::*;
    use crate::test::assert_eq;
    use mytest::*;
    use rstest_reuse::*;

    #[derive(Clone)]
    struct FixtureOutput {
        orig: ItemFn,
        fixture: ItemStruct,
        core_impl: ItemImpl,
    }

    impl Parse for FixtureOutput {
        fn parse(input: ParseStream) -> Result<Self> {
            Ok(FixtureOutput {
                fixture: input.parse()?,
                core_impl: input.parse()?,
                orig: input.parse()?,
            })
        }
    }

    fn parse_fixture<S: AsRef<str>>(code: S) -> (ItemFn, FixtureOutput) {
        let item_fn = parse_str::<ItemFn>(code.as_ref()).unwrap();

        let tokens = render(item_fn.clone(), Default::default());
        (item_fn, parse2(tokens).unwrap())
    }

    fn test_maintains_function_visibility(code: &str) {
        let (item_fn, out) = parse_fixture(code);

        assert_eq!(item_fn.vis, out.fixture.vis);
        assert_eq!(item_fn.vis, out.orig.vis);
    }

    fn select_method<S: AsRef<str>>(impl_code: ItemImpl, name: S) -> Option<syn::ImplItemMethod> {
        impl_code
            .items
            .into_iter()
            .filter_map(|ii| match ii {
                syn::ImplItem::Method(f) => Some(f),
                _ => None,
            })
            .find(|f| f.sig.ident == name.as_ref())
    }

    #[test]
    fn maintains_pub_visibility() {
        test_maintains_function_visibility(r#"pub fn test() { }"#);
    }

    #[test]
    fn maintains_no_pub_visibility() {
        test_maintains_function_visibility(r#"fn test() { }"#);
    }

    #[test]
    fn implement_a_get_method_with_input_fixture_signature() {
        let (item_fn, out) = parse_fixture(
            r#"
                    pub fn test<R: AsRef<str>, B>(mut s: String, v: &u32, a: &mut [i32], r: R) -> (u32, B, String, &str)
                            where B: Borrow<u32>
                    { }
                    "#,
        );

        let mut signature = select_method(out.core_impl, "get").unwrap().sig;

        signature.ident = item_fn.sig.ident.clone();

        assert_eq!(item_fn.sig, signature);
    }

    #[template]
    #[rstest(
        method => ["default", "get", "partial_1", "partial_2", "partial_3"])
    ]
    #[case::async_fn(true)]
    #[case::not_async_fn(false)]
    fn async_fixture_cases(#[case] is_async: bool, method: &str) {}

    #[apply(async_fixture_cases)]
    fn fixture_method_should_be_async_if_fixture_function_is_async(
        #[case] is_async: bool,
        method: &str,
    ) {
        let prefix = if is_async { "async" } else { "" };
        let (_, out) = parse_fixture(&format!(
            r#"
                    pub {} fn test(mut s: String, v: &u32, a: &mut [i32]) -> u32
                            where B: Borrow<u32>
                    {{ }}
                    "#,
            prefix
        ));

        let signature = select_method(out.core_impl, method).unwrap().sig;

        assert_eq!(is_async, signature.asyncness.is_some());
    }

    #[apply(async_fixture_cases)]
    fn fixture_method_should_use_await_if_fixture_function_is_async(
        #[case] is_async: bool,
        method: &str,
    ) {
        let prefix = if is_async { "async" } else { "" };
        let (_, out) = parse_fixture(&format!(
            r#"
                    pub {} fn test(mut s: String, v: &u32, a: &mut [i32]) -> u32
                    {{ }}
                    "#,
            prefix
        ));

        let body = select_method(out.core_impl, method).unwrap().block;
        let last_statment = body.stmts.last().unwrap();
        let is_await = match last_statment {
            syn::Stmt::Expr(syn::Expr::Await(_)) => true,
            _ => false,
        };

        assert_eq!(is_async, is_await);
    }

    #[test]
    fn implement_a_default_method_with_input_cleaned_fixture_signature_and_no_args() {
        let (item_fn, out) = parse_fixture(
            r#"
                    pub fn test<R: AsRef<str>, B, F, H: Iterator<Item=u32>>(mut s: String, v: &u32, a: &mut [i32], r: R) -> (H, B, String, &str)
                        where F: ToString,
                        B: Borrow<u32>

                    { }
                    "#,
        );

        let default_decl = select_method(out.core_impl, "default").unwrap().sig;

        let expected = parse_str::<ItemFn>(
            r#"
                    pub fn default<B, H: Iterator<Item=u32>>() -> (H, B, String, &str)
                            where B: Borrow<u32>
                    { }
                    "#,
        )
        .unwrap();

        assert_eq!(expected.sig.generics, default_decl.generics);
        assert_eq!(item_fn.sig.output, default_decl.output);
        assert!(default_decl.inputs.is_empty());
    }

    #[test]
    fn use_default_return_type_if_any() {
        let item_fn = parse_str::<ItemFn>(
            r#"
                    pub fn test<R: AsRef<str>, B, F, H: Iterator<Item=u32>>() -> (H, B)
                            where F: ToString,
                            B: Borrow<u32>
                    { }
                    "#,
        )
        .unwrap();

        let tokens = render(
            item_fn.clone(),
            FixtureInfo {
                attributes: Attributes {
                    attributes: vec![Attribute::Type(
                        parse_str("default").unwrap(),
                        parse_str("(impl Iterator<Item=u32>, B)").unwrap(),
                    )],
                }
                .into(),
                ..Default::default()
            },
        );
        let out: FixtureOutput = parse2(tokens).unwrap();

        let expected = parse_str::<syn::ItemFn>(
            r#"
                    pub fn default<B>() -> (impl Iterator<Item=u32>, B)
                            where B: Borrow<u32>
                    { }
                    "#,
        )
        .unwrap();

        let default_decl = select_method(out.core_impl, "default").unwrap().sig;

        assert_eq!(expected.sig, default_decl);
    }

    #[test]
    fn implement_partial_methods() {
        let (item_fn, out) = parse_fixture(
            r#"
                    pub fn test(mut s: String, v: &u32, a: &mut [i32]) -> usize
                    { }
                    "#,
        );

        let partials = (1..=3)
            .map(|n| {
                select_method(out.core_impl.clone(), format!("partial_{}", n))
                    .unwrap()
                    .sig
            })
            .collect::<Vec<_>>();

        // All 3 methods found

        assert!(select_method(out.core_impl, "partial_4").is_none());

        let expected_1 = parse_str::<ItemFn>(
            r#"
                    pub fn partial_1(mut s: String) -> usize
                    { }
                    "#,
        )
        .unwrap();

        assert_eq!(expected_1.sig, partials[0]);
        for p in partials {
            assert_eq!(item_fn.sig.output, p.output);
        }
    }

    #[test]
    fn clean_generics_in_partial_methods() {
        let (_, out) = parse_fixture(
            r#"
                    pub fn test<S: AsRef<str>, U: AsRef<u32>, F: ToString>(mut s: S, v: U) -> F
                    { }
                    "#,
        );

        let partials = (1..=2)
            .map(|n| {
                select_method(out.core_impl.clone(), format!("partial_{}", n))
                    .unwrap()
                    .sig
            })
            .collect::<Vec<_>>();

        let expected = vec![
                    parse_str::<ItemFn>(
                        r#"
                        pub fn partial_1<S: AsRef<str>, F: ToString>(mut s: S) -> F
                        { }
                        "#
                    ).unwrap().sig,
                    parse_str::<ItemFn>(
                        r#"
                        pub fn partial_2<S: AsRef<str>, U: AsRef<u32>, F: ToString>(mut s: S, v: U) -> F
                        { }
                        "#
                    ).unwrap().sig];

        assert_eq!(expected, partials);
    }

    #[test]
    fn use_partial_return_type_if_any() {
        let item_fn = parse_str::<ItemFn>(
            r#"
                    pub fn test<R: AsRef<str>, B, F, H: Iterator<Item=u32>>(h: H, b: B) -> (H, B)
                            where F: ToString,
                            B: Borrow<u32>
                    { }
                     "#,
        )
        .unwrap();

        let tokens = render(
            item_fn.clone(),
            FixtureInfo {
                attributes: Attributes {
                    attributes: vec![Attribute::Type(
                        parse_str("partial_1").unwrap(),
                        parse_str("(H, impl Iterator<Item=u32>)").unwrap(),
                    )],
                }
                .into(),
                ..Default::default()
            },
        );
        let out: FixtureOutput = parse2(tokens).unwrap();

        let expected = parse_str::<syn::ItemFn>(
            r#"
                    pub fn partial_1<H: Iterator<Item=u32>>(h: H) -> (H, impl Iterator<Item=u32>)
                    { }
                    "#,
        )
        .unwrap();

        let partial = select_method(out.core_impl, "partial_1").unwrap();

        assert_eq!(expected.sig, partial.sig);
    }
}
