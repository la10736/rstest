pub(crate) mod fixture;
mod test;
mod wrapper;

use std::collections::HashMap;

use syn::token::Async;

use proc_macro2::{Span, TokenStream};
use syn::{
    parse_quote, Attribute, Expr, FnArg, Ident, ItemFn, Path, ReturnType, Stmt, Type, TypeParam,
};

use quote::{format_ident, quote, ToTokens};
use unicode_ident::is_xid_continue;

use crate::utils::attr_ends_with;
use crate::{
    parse::{
        rstest::{RsTestAttributes, RsTestData, RsTestInfo},
        testcase::TestCase,
        vlist::ValueList,
    },
    utils::attr_is,
};
use crate::{
    refident::MaybeIdent,
    resolver::{self, Resolver},
};
use wrapper::WrapByModule;

pub(crate) use fixture::render as fixture;

use self::apply_argumets::ApplyArgumets;
pub(crate) mod apply_argumets;
pub(crate) mod inject;

pub(crate) fn single(mut test: ItemFn, info: RsTestInfo) -> TokenStream {
    test.apply_argumets(&info.arguments);
    let resolver = resolver::fixtures::get(info.data.fixtures());
    let args = test.sig.inputs.iter().cloned().collect::<Vec<_>>();
    let attrs = std::mem::take(&mut test.attrs);
    let asyncness = test.sig.asyncness;
    let generic_types = test
        .sig
        .generics
        .type_params()
        .map(|tp| &tp.ident)
        .cloned()
        .collect::<Vec<_>>();

    single_test_case(
        &test.sig.ident,
        &test.sig.ident,
        &args,
        &attrs,
        &test.sig.output,
        asyncness,
        Some(&test),
        resolver,
        &info.attributes,
        &generic_types,
    )
}

pub(crate) fn parametrize(mut test: ItemFn, info: RsTestInfo) -> TokenStream {
    let RsTestInfo {
        data,
        attributes,
        arguments,
    } = info;
    test.apply_argumets(&arguments);
    let resolver_fixtures = resolver::fixtures::get(data.fixtures());

    let rendered_cases = cases_data(&data, test.sig.ident.span())
        .map(|(name, attrs, resolver)| {
            TestCaseRender::new(name, attrs, (resolver, &resolver_fixtures))
        })
        .map(|case| case.render(&test, &attributes))
        .collect();

    test_group(test, rendered_cases)
}

impl ValueList {
    fn render(
        &self,
        test: &ItemFn,
        resolver: &dyn Resolver,
        attrs: &[syn::Attribute],
        attributes: &RsTestAttributes,
    ) -> TokenStream {
        let span = test.sig.ident.span();
        let test_cases = self
            .argument_data(resolver)
            .map(|(name, r)| TestCaseRender::new(Ident::new(&name, span), attrs, r))
            .map(|test_case| test_case.render(test, attributes));

        quote! { #(#test_cases)* }
    }

    fn argument_data<'a>(
        &'a self,
        resolver: &'a dyn Resolver,
    ) -> impl Iterator<Item = (String, Box<(&'a dyn Resolver, (String, Expr))>)> + 'a {
        let max_len = self.values.len();
        self.values.iter().enumerate().map(move |(index, expr)| {
            let sanitized_expr = sanitize_ident(expr);
            let name = format!(
                "{}_{:0len$}_{sanitized_expr:.64}",
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
    attrs: &'a [syn::Attribute],
    attributes: &RsTestAttributes,
) -> TokenStream {
    if list_values.is_empty() {
        return Default::default();
    }
    let vlist = list_values[0];
    let list_values = &list_values[1..];

    if list_values.is_empty() {
        let mut attrs = attrs.to_vec();
        attrs.push(parse_quote!(
            #[allow(non_snake_case)]
        ));
        vlist.render(test, resolver, &attrs, attributes)
    } else {
        let span = test.sig.ident.span();
        let modules = vlist.argument_data(resolver).map(move |(name, resolver)| {
            _matrix_recursive(test, list_values, &resolver, attrs, attributes)
                .wrap_by_mod(&Ident::new(&name, span))
        });

        quote! { #(
            #[allow(non_snake_case)]
            #modules
        )* }
    }
}

pub(crate) fn matrix(mut test: ItemFn, info: RsTestInfo) -> TokenStream {
    let RsTestInfo {
        data,
        attributes,
        arguments,
    } = info;
    test.apply_argumets(&arguments);
    let span = test.sig.ident.span();

    let cases = cases_data(&data, span).collect::<Vec<_>>();

    let resolver = resolver::fixtures::get(data.fixtures());
    let rendered_cases = if cases.is_empty() {
        let list_values = data.list_values().collect::<Vec<_>>();
        _matrix_recursive(&test, &list_values, &resolver, &[], &attributes)
    } else {
        cases
            .into_iter()
            .map(|(case_name, attrs, case_resolver)| {
                let list_values = data.list_values().collect::<Vec<_>>();
                _matrix_recursive(
                    &test,
                    &list_values,
                    &(case_resolver, &resolver),
                    attrs,
                    &attributes,
                )
                .wrap_by_mod(&case_name)
            })
            .collect()
    };

    test_group(test, rendered_cases)
}

fn resolve_default_test_attr(is_async: bool) -> syn::Attribute {
    if is_async {
        parse_quote! { #[async_std::test] }
    } else {
        parse_quote! { #[test] }
    }
}

fn render_exec_call(fn_path: Path, args: &[Ident], is_async: bool) -> TokenStream {
    if is_async {
        quote! {#fn_path(#(#args),*).await}
    } else {
        quote! {#fn_path(#(#args),*)}
    }
}

fn render_test_call(
    fn_path: Path,
    args: &[Ident],
    timeout: Option<Expr>,
    is_async: bool,
) -> TokenStream {
    let timeout = timeout.map(|x| quote! {#x}).or_else(|| {
        std::env::var("RSTEST_TIMEOUT")
            .ok()
            .and_then(|to| Some(quote! { std::time::Duration::from_secs( (#to).parse().unwrap()) }))
    });
    match (timeout, is_async) {
        (Some(to_expr), true) => quote! {
            use rstest::timeout::*;
            execute_with_timeout_async(move || #fn_path(#(#args),*), #to_expr).await
        },
        (Some(to_expr), false) => quote! {
            use rstest::timeout::*;
            execute_with_timeout_sync(move || #fn_path(#(#args),*), #to_expr)
        },
        _ => render_exec_call(fn_path, args, is_async),
    }
}

#[derive(Debug)]
struct DesugaredFnArg {
    arg: Ident,
    ty: Type,
    generic_bound: Option<TypeParam>,
}

impl DesugaredFnArg {
    fn new(arg: Ident, ty: Type, generic_bound: Option<TypeParam>) -> Self {
        Self {
            arg,
            ty,
            generic_bound,
        }
    }

    fn from_fn_arg(arg: &FnArg) -> Option<Self> {
        match arg {
            FnArg::Receiver(_) => None,
            FnArg::Typed(t) => Some(t),
        }
        .and_then(|p| match *p.pat.clone() {
            syn::Pat::Ident(id) => Some((id, &p.ty)),
            _ => None,
        })
        .and_then(|(name, ty)| match *ty.clone() {
            Type::ImplTrait(syn::TypeImplTrait { bounds, .. }) => {
                let bounds = bounds.into_iter();
                Some(Self::new(
                    name.ident,
                    parse_quote! { T },
                    Some(parse_quote! {
                        T: #(#bounds)+*
                    }),
                ))
            }
            _ => Some(Self::new(name.ident, *ty.clone(), None)),
        })
    }
}

fn desugar_fn_impl(args: &[FnArg]) -> (Vec<Ident>, Vec<Type>, Vec<TypeParam>) {
    let mut names = Vec::with_capacity(args.len());
    let mut types = Vec::with_capacity(args.len());
    let mut generic_bounds = Vec::new();

    for (pos, arg) in args.iter().enumerate() {
        let (name, ty) = match match arg {
            FnArg::Receiver(_) => None,
            FnArg::Typed(t) => Some(t),
        }
        .and_then(|p| match *p.pat.clone() {
            syn::Pat::Ident(id) => Some((id, &p.ty)),
            _ => None,
        }) {
            Some((n, t)) => (n.ident, *t.clone()),
            None => continue,
        };
        names.push(name);
        let (ty, bounds): (Type, Option<TypeParam>) = match ty {
            Type::ImplTrait(syn::TypeImplTrait { bounds, .. }) => {
                let bounds = bounds.into_iter();
                let t_name = format_ident!("RSTEST_TYPE_{}", pos);
                (
                    parse_quote! { #t_name },
                    Some(parse_quote! { #t_name: #(#bounds)+* }),
                )
            }
            _ => (ty, None),
        };
        types.push(ty);
        match bounds {
            Some(b) => generic_bounds.push(b),
            None => {}
        }
    }

    (names, types, generic_bounds)
}

/// Render a single test case:
///
/// * `name` - Test case name
/// * `testfn_name` - The name of test function to call
/// * `args` - The arguments of the test function
/// * `attrs` - The expected test attributes
/// * `output` - The expected test return type
/// * `asyncness` - The `async` fn token
/// * `test_impl` - If you want embed test function (should be the one called by `testfn_name`)
/// * `resolver` - The resolver used to resolve injected values
/// * `attributes` - Test attributes to select test behaviour
/// * `generic_types` - The genrics type used in signature
///
// Ok I need some refactoring here but now that not a real issue
#[allow(clippy::too_many_arguments)]
fn single_test_case(
    name: &Ident,
    testfn_name: &Ident,
    fn_args: &[FnArg],
    attrs: &[Attribute],
    output: &ReturnType,
    asyncness: Option<Async>,
    test_impl: Option<&ItemFn>,
    resolver: impl Resolver,
    attributes: &RsTestAttributes,
    generic_types: &[Ident],
) -> TokenStream {
    let (attrs, trace_me): (Vec<_>, Vec<_>) =
        attrs.iter().cloned().partition(|a| !attr_is(a, "trace"));
    let mut attributes = attributes.clone();
    if !trace_me.is_empty() {
        attributes.add_trace(format_ident!("trace"));
    }
    let inject = inject::resolve_aruments(fn_args.iter(), &resolver, generic_types);
    let args = fn_args
        .iter()
        .filter_map(MaybeIdent::maybe_ident)
        .cloned()
        .collect::<Vec<_>>();
    let inner_context_fields: Vec<_> = fn_args
        .iter()
        .filter_map(|a| match a {
            FnArg::Receiver(_) => None,
            FnArg::Typed(t) => Some(t),
        })
        .filter_map(|p| match *p.pat.clone() {
            syn::Pat::Ident(id) => Some((id, &p.ty)),
            _ => None,
        })
        .map(|(name, ty)| {
            quote! {
                #name: #ty
            }
        })
        .collect::<Vec<_>>();
    let trace_args = trace_arguments(args.iter(), &attributes);

    let is_async = asyncness.is_some();
    let (attrs, timeouts): (Vec<_>, Vec<_>) =
        attrs.iter().cloned().partition(|a| !attr_is(a, "timeout"));

    let timeout = timeouts
        .into_iter()
        .last()
        .map(|attribute| attribute.parse_args::<Expr>().unwrap());

    let (test_attrs, attrs): (Vec<_>, Vec<_>) = attrs
        .into_iter()
        .partition(|a| attr_ends_with(a, &parse_quote! {test}));

    // If no injected attribute provided use the default one
    let test_attr = test_attrs
        .into_iter()
        .nth(0)
        .unwrap_or_else(|| resolve_default_test_attr(is_async));

    let desugar = fn_args
        .iter()
        .filter_map(DesugaredFnArg::from_fn_arg)
        .collect::<Vec<_>>();
    let (state_args, state_types, state_generics) = desugar_fn_impl(&fn_args);
    let (state_generics, state_generic_type) = if state_generics.len() > 0 {
        let types = state_generics.iter().map(|g| quote! {_}).cloned();
        (quote! {<#(#state_generics),*>}, quote! {::<#(#types),*>})
    } else {
        (quote! {}, quote! {})
    };

    let execute = render_test_call(testfn_name.clone().into(), &args, timeout, is_async);

    quote! {
        #[test]
        #(#attrs)*
        fn #name() #output {
            #test_impl
            #inject
            #trace_args
            #[allow(unnameable_test_items)]
            {
                struct _RstestInnerDataContext #state_generics {
                    #(#state_args: #state_types),*
                }
                thread_local! {
                    static _RSTEST_INNER_DATA_CONTEXT: std::cell::RefCell<Option<_RstestInnerDataContext>>  = std::cell::RefCell::new(None);
                }
                _RSTEST_INNER_DATA_CONTEXT.with(|r| r.replace(Some(_RstestInnerDataContext #state_generic_type {
                    #(#args),*
                })));
                #[inline]
                #test_attr
                #asyncness fn wrap (){
                    let _RstestInnerDataContext #state_generic_type {
                        #(#args),*
                    } = _RSTEST_INNER_DATA_CONTEXT.with(|r| r.replace(None)).unwrap();
                    #execute
                }
                wrap()
            }
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
            let s: Stmt = parse_quote! {
                println!("{} = {:?}", stringify!(#arg), #arg);
            };
            s
        })
        .peekable();
    if statements.peek().is_some() {
        Some(quote! {
            println!("{:-^40}", " TEST ARGUMENTS ");
            #(#statements)*
            println!("{:-^40}", " TEST START ");
        })
    } else {
        None
    }
}

struct TestCaseRender<'a> {
    name: Ident,
    attrs: &'a [syn::Attribute],
    resolver: Box<dyn Resolver + 'a>,
}

impl<'a> TestCaseRender<'a> {
    pub fn new<R: Resolver + 'a>(name: Ident, attrs: &'a [syn::Attribute], resolver: R) -> Self {
        TestCaseRender {
            name,
            attrs,
            resolver: Box::new(resolver),
        }
    }

    fn render(self, testfn: &ItemFn, attributes: &RsTestAttributes) -> TokenStream {
        let args = testfn.sig.inputs.iter().cloned().collect::<Vec<_>>();
        let mut attrs = testfn.attrs.clone();
        attrs.extend(self.attrs.iter().cloned());
        let asyncness = testfn.sig.asyncness;
        let generic_types = testfn
            .sig
            .generics
            .type_params()
            .map(|tp| &tp.ident)
            .cloned()
            .collect::<Vec<_>>();

        single_test_case(
            &self.name,
            &testfn.sig.ident,
            &args,
            &attrs,
            &testfn.sig.output,
            asyncness,
            None,
            self.resolver,
            attributes,
            &generic_types,
        )
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
        format!("{self}").len()
    }
}

fn format_case_name(case: &TestCase, index: usize, display_len: usize) -> String {
    let description = case
        .description
        .as_ref()
        .map(|d| format!("_{d}"))
        .unwrap_or_default();
    format!("case_{index:0display_len$}{description}")
}

fn cases_data(
    data: &RsTestData,
    name_span: Span,
) -> impl Iterator<Item = (Ident, &[syn::Attribute], HashMap<String, &syn::Expr>)> {
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
                case.attrs.as_slice(),
                resolver_case,
            )
        }
    })
}

fn sanitize_ident(expr: &Expr) -> String {
    expr.to_token_stream()
        .to_string()
        .chars()
        .filter(|c| !c.is_whitespace())
        .map(|c| match c {
            '"' | '\'' => "__".to_owned(),
            ':' | '(' | ')' | '{' | '}' | '[' | ']' | ',' | '.' | '*' | '+' | '/' | '-' | '%'
            | '^' | '!' | '&' | '|' => "_".to_owned(),
            _ => c.to_string(),
        })
        .collect::<String>()
        .chars()
        .filter(|&c| is_xid_continue(c))
        .collect()
}
