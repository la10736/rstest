pub(crate) mod fixture;
mod test;
mod wrapper;

use std::{borrow::Cow, collections::HashMap};

use proc_macro2::{Span, TokenStream};
use syn::{
    parse_quote, Expr, FnArg, Generics, Ident, ItemFn, ReturnType, Stmt, Type, WherePredicate,
};

use quote::quote;

use crate::parse::{
    rstest::{RsTestAttributes, RsTestData, RsTestInfo},
    testcase::TestCase,
    vlist::ValueList,
};
use crate::refident::MaybeIdent;
use crate::resolver::{self, Resolver};
use crate::utils::fn_args_idents;
use wrapper::WrapByModule;

pub(crate) use fixture::render as fixture;

pub(crate) fn single(test: ItemFn, info: RsTestInfo) -> TokenStream {
    let name = &test.sig.ident;
    let resolver = resolver::fixture_resolver(info.data.fixtures());

    single_test_case(name.clone(), &test, Some(&test), resolver, &info.attributes)
}

pub(crate) fn parametrize(test: ItemFn, info: RsTestInfo) -> TokenStream {
    let RsTestInfo { data, attributes } = info;
    let resolver_fixtures = resolver::fixture_resolver(data.fixtures());

    let rendered_cases = cases_data(&data, test.sig.ident.span())
        .map(|(name, resolver)| TestCaseRender::new(name, (resolver, &resolver_fixtures)))
        .map(|case| case.render(&test, &attributes))
        .collect();

    test_group(test, rendered_cases)
}

impl ValueList {
    fn render(
        &self,
        test: &ItemFn,
        resolver: &dyn Resolver,
        attributes: &RsTestAttributes,
    ) -> TokenStream {
        let span = test.sig.ident.span();
        let test_cases = self
            .argument_data(resolver)
            .map(|(name, r)| TestCaseRender::new(Ident::new(&name, span), r))
            .map(|test_case| test_case.render(&test, &attributes));

        quote! { #(#test_cases)* }
    }

    fn argument_data<'a>(
        &'a self,
        resolver: &'a dyn Resolver,
    ) -> impl Iterator<Item = (String, Box<(&'a dyn Resolver, (String, Expr))>)> + 'a {
        let max_len = self.values.len();
        self.values.iter().enumerate().map(move |(index, expr)| {
            let name = format!(
                "{}_{:0len$}",
                self.arg,
                index + 1,
                len = max_len.display_len()
            );
            let resolver_this = (self.arg.to_string(), expr.clone());
            (name, Box::new((resolver, resolver_this)))
        })
    }
}

fn _matrix_recursive<'a>(
    test: &ItemFn,
    list_values: &'a [&'a ValueList],
    resolver: &dyn Resolver,
    attributes: &RsTestAttributes,
) -> TokenStream {
    if list_values.len() == 0 {
        return Default::default();
    }
    let vlist = list_values[0];
    let list_values = &list_values[1..];

    if list_values.len() == 0 {
        vlist.render(test, resolver, attributes)
    } else {
        let span = test.sig.ident.span();
        let modules = vlist.argument_data(resolver).map(move |(name, resolver)| {
            _matrix_recursive(test, list_values, &resolver, attributes)
                .wrap_by_mod(&Ident::new(&name, span))
        });

        quote! { #(#modules)* }
    }
}

pub(crate) fn matrix(test: ItemFn, info: RsTestInfo) -> TokenStream {
    let RsTestInfo {
        data, attributes, ..
    } = info;
    let span = test.sig.ident.span();

    let cases = cases_data(&data, span).collect::<Vec<_>>();

    let resolver = resolver::fixture_resolver(data.fixtures());
    let rendered_cases = if cases.is_empty() {
        let list_values = data.list_values().collect::<Vec<_>>();
        _matrix_recursive(&test, &list_values, &resolver, &attributes)
    } else {
        cases
            .into_iter()
            .map(|(case_name, case_resolver)| {
                let list_values = data.list_values().collect::<Vec<_>>();
                _matrix_recursive(
                    &test,
                    &list_values,
                    &(case_resolver, &resolver),
                    &attributes,
                )
                .wrap_by_mod(&case_name)
            })
            .collect()
    };

    test_group(test, rendered_cases)
}

fn single_test_case<'a>(
    name: Ident,
    testfn: &ItemFn,
    test_impl: Option<&ItemFn>,
    resolver: impl Resolver,
    attributes: &'a RsTestAttributes,
) -> TokenStream {
    let testfn_name = &testfn.sig.ident;
    let test_impl = test_impl.map(|f| {
        let mut f = f.clone();
        f.attrs = vec![];
        f
    }); // Remove attributes
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

struct TestCaseRender<'a> {
    name: Ident,
    resolver: Box<dyn Resolver + 'a>,
}

impl<'a> TestCaseRender<'a> {
    pub fn new<R: Resolver + 'a>(name: Ident, resolver: R) -> Self {
        TestCaseRender {
            name,
            resolver: Box::new(resolver),
        }
    }

    fn render(self, testfn: &ItemFn, attributes: &RsTestAttributes) -> TokenStream {
        single_test_case(self.name, testfn, None, self.resolver, attributes)
    }
}

fn test_group(mut test: ItemFn, rendered_cases: TokenStream) -> TokenStream {
    let fname = &test.sig.ident;
    test.attrs = vec![];

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
