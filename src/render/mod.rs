mod test;
mod wrapper;

use std::{borrow::Cow, collections::HashMap};

use itertools::Itertools;
use proc_macro2::{Span, TokenStream};
use syn::{
    parse_quote, Expr, FnArg, Generics, Ident, ItemFn, ReturnType, Stmt, Type, WherePredicate,
};

use quote::quote;

use crate::parse::{
    fixture::FixtureInfo,
    rstest::{RsTestAttributes, RsTestData, RsTestInfo},
    testcase::TestCase,
};
use crate::refident::MaybeIdent;
use crate::resolver::{self, Resolver};
use crate::utils::{fn_args, fn_args_idents};
use wrapper::WrapByModule;

pub(crate) fn render_single_case(test: ItemFn, info: RsTestInfo) -> TokenStream {
    let name = &test.sig.ident;
    let resolver = resolver::fixture_resolver(info.data.fixtures());

    render_fn_test(name.clone(), &test, Some(&test), resolver, &info.attributes)
}

pub(crate) fn render_parametrize_cases(test: ItemFn, info: RsTestInfo) -> TokenStream {
    let RsTestInfo { data, attributes } = info;
    let resolver_fixtures = resolver::fixture_resolver(data.fixtures());

    let rendered_cases = cases_data(&data, test.sig.ident.span())
        .map(|(name, resolver)| TestCaseRender::new(name, (resolver, &resolver_fixtures)))
        .map(|case| case.render(&test, &attributes))
        .collect();

    render_test_group(test, rendered_cases)
}

pub(crate) fn render_matrix_cases(test: ItemFn, info: RsTestInfo) -> TokenStream {
    let RsTestInfo {
        data, attributes, ..
    } = info;
    let span = test.sig.ident.span();

    let cases = cases_data(&data, span).collect::<Vec<_>>();

    let resolver = resolver::fixture_resolver(data.fixtures());
    let rendered_cases = if cases.is_empty() {
        render_inner_matrix_cases(&test, &attributes, &resolver, data.list_values())
    } else {
        cases
            .into_iter()
            .map(|(case_name, case_resolver)| {
                render_inner_matrix_cases(
                    &test,
                    &attributes,
                    &(case_resolver, &resolver),
                    data.list_values(),
                )
                .wrap_by_mod(&case_name)
            })
            .collect()
    };

    render_test_group(test, rendered_cases)
}

pub(crate) fn render_fixture<'a>(fixture: ItemFn, info: FixtureInfo) -> TokenStream {
    let name = &fixture.sig.ident;
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
    let body = &fixture.block;
    let where_clause = &fixture.sig.generics.where_clause;
    let output = &fixture.sig.output;
    let visibility = &fixture.vis;
    let resolver = resolver::fixture_resolver(info.data.fixtures());
    let inject = resolve_args(fn_args_idents(&fixture), &resolver);
    let partials =
        (1..=orig_args.len()).map(|n| render_partial_impl(&fixture, n, &resolver, &info));
    quote! {
        #[allow(non_camel_case_types)]
        #visibility struct #name {}

        impl #name {
            #(#orig_attrs)*
            pub fn get #generics (#orig_args) #output #where_clause {
                #body
            }

            pub fn default #default_generics () #default_output #default_where_clause {
                #inject
                Self::get(#(#args),*)
            }

            #(#partials)*
        }

        #[allow(dead_code)]
        #fixture
    }
}

fn render_fn_test<'a>(
    name: Ident,
    testfn: &ItemFn,
    test_impl: Option<&ItemFn>,
    resolver: impl Resolver,
    attributes: &'a RsTestAttributes,
) -> TokenStream {
    let testfn_name = &testfn.sig.ident;
    let args = fn_args_idents(&testfn).cloned().collect::<Vec<_>>();
    let attrs = &testfn.attrs;
    let output = &testfn.sig.output;
    let inject = resolve_args(fn_args_idents(&testfn), &resolver);
    let trace_args = trace_arguments(args.iter(), attributes);
    quote! {
        #[test]
        #(#attrs)*
        fn #name() #output {
            #test_impl
            #inject
            #trace_args
            println!("{:-^40}", " TEST START ");
            #testfn_name(#(#args),*)
        }
    }
}

fn render_inner_matrix_cases<'a>(
    test: &ItemFn,
    attributes: &RsTestAttributes,
    resolver: &impl Resolver,
    list_values: impl Iterator<Item = &'a crate::parse::vlist::ValueList>,
) -> TokenStream {
    let test_function_span = test.sig.ident.span();
    // Steps:
    // 1. pack data P=(ident, expr, (pos, max_len)) in one iterator for each variable
    // 2. do a cartesian product of iterators to build all cases (every case is a vector of P)
    // 3. format case by packed data vector
    let test_cases = list_values
        .map(|group| {
            group
                .values
                .iter()
                .enumerate()
                .map(move |(pos, expr)| (&group.arg, expr, (pos, group.values.len())))
        })
        .multi_cartesian_product()
        .map(|c| {
            let args_indexes = c
                .iter()
                .map(|(_, _, (index, max))| format!("{:0len$}", index + 1, len = max.display_len()))
                .collect::<Vec<_>>()
                .join("_");
            let name = format!("case_{}", args_indexes);
            let resolver_case = c
                .into_iter()
                .map(|(a, e, _)| (a.to_string(), e))
                .collect::<HashMap<_, _>>();
            TestCaseRender::new(
                Ident::new(&name, test_function_span),
                (resolver_case, resolver),
            )
        });

    let test_cases = test_cases.map(|test_case| test_case.render(test, attributes));

    quote! { #(#test_cases)* }
}

fn trace_arguments<'a>(
    args: impl Iterator<Item = &'a Ident>,
    attributes: &RsTestAttributes,
) -> Option<TokenStream> {
    let mut statements = args
        .filter(|&arg| attributes.trace_me(arg))
        .map(|arg| {
            parse_quote! {
                println!("{} = {:?}", stringify!(#arg), #arg);
            }
        })
        .map(|stmt: Stmt| stmt)
        .peekable();
    if statements.peek().is_some() {
        Some(quote! {
            println!("{:-^40}", " TEST ARGUMENTS ");
            #(#statements)*
        })
    } else {
        None
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

    let inject = resolve_args(fn_args_idents(fixture).skip(n), resolver);

    let sign_args = fn_args(fixture).take(n);
    let fixture_args = fn_args_idents(fixture);
    let name = Ident::new(&format!("partial_{}", n), Span::call_site());

    quote! {
        pub fn #name #generics (#(#sign_args),*) #output #where_clause {
            #inject
            Self::get(#(#fixture_args),*)
        }
    }
}

fn default_fixture_resolve(ident: &Ident) -> Cow<Expr> {
    Cow::Owned(parse_quote! { #ident::default() })
}

fn arg_2_fixture(ident: &Ident, resolver: &impl Resolver) -> Stmt {
    let fixture = resolver
        .resolve(ident)
        .map(|e| e.clone())
        .unwrap_or_else(|| default_fixture_resolve(ident));
    parse_quote! {
        let #ident = #fixture;
    }
}

fn resolve_args<'a>(
    args: impl Iterator<Item = &'a Ident>,
    resolver: &impl Resolver,
) -> TokenStream {
    let define_vars = args.map(|arg| arg_2_fixture(arg, resolver));
    quote! {
        #(#define_vars)*
    }
}

fn where_predicate_bounded_type(wp: &WherePredicate) -> Option<&Type> {
    match wp {
        syn::WherePredicate::Type(pt) => Some(&pt.bounded_ty),
        _ => None,
    }
}

//noinspection RsTypeCheck
fn generics_clean_up<'a>(
    original: &Generics,
    inputs: impl Iterator<Item = &'a FnArg>,
    output: &ReturnType,
) -> syn::Generics {
    use syn::visit::Visit;
    #[derive(Default, Debug)]
    struct Used(std::collections::HashSet<proc_macro2::Ident>);
    impl<'ast> syn::visit::Visit<'ast> for Used {
        fn visit_type_path(&mut self, i: &'ast syn::TypePath) {
            if i.qself.is_none() && i.path.leading_colon.is_none() && i.path.segments.len() == 1 {
                self.0
                    .insert(i.path.segments.first().unwrap().ident.clone());
            }
        }
    }
    let mut outs = Used::default();
    outs.visit_return_type(output);
    inputs.for_each(|fn_arg| outs.visit_fn_arg(fn_arg));
    let mut result: Generics = original.clone();
    result.params = result
        .params
        .into_iter()
        .filter(|p| match p {
            syn::GenericParam::Type(tp) if !outs.0.contains(&tp.ident) => false,
            _ => true,
        })
        .collect();
    result.where_clause.as_mut().map(|mut w| {
        w.predicates = w
            .predicates
            .clone()
            .into_iter()
            .filter(|wp| {
                where_predicate_bounded_type(wp)
                    .and_then(MaybeIdent::maybe_ident)
                    .map(|t| outs.0.contains(t))
                    .unwrap_or(true)
            })
            .collect()
    });
    result
}

struct TestCaseRender<R: Resolver> {
    name: Ident,
    resolver: R,
}

impl<R: Resolver> TestCaseRender<R> {
    pub fn new(name: Ident, resolver: R) -> Self {
        TestCaseRender { name, resolver }
    }

    fn render(self, testfn: &ItemFn, attributes: &RsTestAttributes) -> TokenStream {
        render_fn_test(self.name, testfn, None, self.resolver, attributes)
    }
}

fn render_test_group(test: ItemFn, rendered_cases: TokenStream) -> TokenStream {
    let fname = &test.sig.ident;

    quote! {
        #[cfg(test)]
        #test

        #[cfg(test)]
        mod #fname {
            use super::*;

            #rendered_cases
        }
    }
}

trait DisplayLen {
    fn display_len(&self) -> usize;
}

impl<D: std::fmt::Display> DisplayLen for D {
    fn display_len(&self) -> usize {
        format!("{}", self).len()
    }
}

fn format_case_name(case: &TestCase, index: usize, display_len: usize) -> String {
    let description = case
        .description
        .as_ref()
        .map(|d| format!("_{}", d))
        .unwrap_or_default();
    format!(
        "case_{:0len$}{d}",
        index,
        len = display_len,
        d = description
    )
}

fn cases_data(
    data: &RsTestData,
    name_span: Span,
) -> impl Iterator<Item = (Ident, HashMap<String, &syn::Expr>)> {
    let display_len = data.cases().count().display_len();
    data.cases().enumerate().map({
        move |(n, case)| {
            let resolver_case = data
                .case_args()
                .map(|a| a.to_string())
                .zip(case.args.iter())
                .collect::<HashMap<_, _>>();
            (
                Ident::new(&format_case_name(case, n + 1, display_len), name_span),
                resolver_case,
            )
        }
    })
}
