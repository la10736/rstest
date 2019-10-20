//! This crate will help you to write simpler tests by leveraging a software testing concept called
//! [test fixtures](https://en.wikipedia.org/wiki/Test_fixture#Software). A fixture is something
//! that you can use in your tests to encapsulate a test's dependencies.
//!
//! The general idea is to have smaller tests that only describe the thing you're testing while you
//! hide the auxiliary utilities your tests make use of somewhere else.
//! For instance, if you have an application that has many tests with users, shopping baskets, and
//! products, you'd have to create a user, a shopping basket, and product every single time in
//! every test which becomes unwieldy quickly. In order to cut down on that repetition, you can
//! instead use fixtures to declare that you need those objects for your function and the fixtures
//! will take care of creating those by themselves. Focus on the important stuff in your tests!
//!
//! In `rstest` a fixture is a function that can return any kind of valid Rust type. This
//! effectively means that your fixtures are not limited by the kind of data they can return.
//! A test can consume an arbitrary number of fixtures at the same time.
//!
//! ## What
//!
//! The `rstest` crate defines the following procedural macros:
//!
//! - [`[rstest]`](attr.rstest.html): A normal Rust test that may additionally take fixtures.
//! - [`[rstest_parametrize]`](attr.rstest_parametrize.html): Like `[rstest]` above but with the
//! added ability to also generate new test cases based on input tables.
//! - [`[rstest_matrix]`](attr.rstest_matrix.html): Like `[rstest]` above but with the
//! added ability to also generate new test cases for every combination of given values.
//! - [`[fixture]`](attr.fixture.html): To mark a function as a fixture.
//!
//! ## Why
//!
//! Very often in Rust we write tests like this
//!
//! ```
//! #[test]
//! fn should_process_two_users() {
//!     let mut repository = create_repository();
//!     repository.add("Bob", 21);
//!     repository.add("Alice", 22);
//!
//!     let processor = string_processor();
//!     processor.send_all(&repository, "Good Morning");
//!
//!     assert_eq!(2, processor.output.find("Good Morning").count());
//!     assert!(processor.output.contains("Bob"));
//!     assert!(processor.output.contains("Alice"));
//! }
//! ```
//!
//! By making use of [`[rstest]`](attr.rstest.html) we can isolate the dependencies `empty_repository` and
//! `string_processor` by passing them as fixtures:
//!
//! ```
//! # use rstest::*;
//! #[rstest]
//! fn should_process_two_users(mut empty_repository: impl Repository,
//!                             string_processor: FakeProcessor) {
//!     empty_repository.add("Bob", 21);
//!     empty_repository.add("Alice", 22);
//!
//!     string_processor.send_all("Good Morning");
//!
//!     assert_eq!(2, string_processor.output.find("Good Morning").count());
//!     assert!(string_processor.output.contains("Bob"));
//!     assert!(string_processor.output.contains("Alice"));
//! }
//! ```
//!
//! ... or if you use `"Alice"` and `"Bob"` in other tests, you can isolate `alice_and_bob` fixture
//! and use it directly:
//!
//! ```
//! # use rstest::*;
//! # trait Repository { fn add(&mut self, name: &str, age: u8); }
//! # struct Rep;
//! # impl Repository for Rep { fn add(&mut self, name: &str, age: u8) {} }
//! # #[fixture]
//! # fn empty_repository() -> Rep {
//! #     Rep
//! # }
//! #[fixture]
//! fn alice_and_bob(mut empty_repository: impl Repository) -> impl Repository {
//!     empty_repository.add("Bob", 21);
//!     empty_repository.add("Alice", 22);
//!     empty_repository
//! }
//!
//! #[rstest]
//! fn should_process_two_users(alice_and_bob: impl Repository,
//!                             string_processor: FakeProcessor) {
//!     string_processor.send_all("Good Morning");
//!
//!     assert_eq!(2, string_processor.output.find("Good Morning").count());
//!     assert!(string_processor.output.contains("Bob"));
//!     assert!(string_processor.output.contains("Alice"));
//! }
//! ```
//!
//! ## Injecting fixtures as function arguments
//!
//! `rstest` functions can receive fixtures by using them as an input argument. A function decorated
//! with [`[rstest]`](attr.rstest.html) will resolve each argument name by call the fixture
//! function. Fixtures should be annotated with the [`[fixture]`](attr.fixture.html) attribute.
//!
//! Fixtures will be resolved like function calls by following the standard resolution rules.
//! Therefore, an identically named fixture can be use in different context.
//!
//! ```
//! # use rstest::*;
//! # trait Repository { }
//! # #[derive(Default)]
//! # struct DataSet {}
//! # impl Repository for DataSet { }
//! mod empty_cases {
//! # use rstest::*;
//! # trait Repository { }
//! # #[derive(Default)]
//! # struct DataSet {}
//! # impl Repository for DataSet { }
//!     use super::*;
//!
//!     #[fixture]
//!     fn repository() -> impl Repository {
//!         DataSet::default()
//!     }
//!
//!     #[rstest]
//!     fn should_do_nothing(repository: impl Repository) {
//!         //.. test impl ..
//!     }
//! }
//!
//! mod non_trivial_case {
//! # use rstest::*;
//! # trait Repository { }
//! # #[derive(Default)]
//! # struct DataSet {}
//! # impl Repository for DataSet { }
//!     use super::*;
//!
//!     #[fixture]
//!     fn repository() -> impl Repository {
//!         let mut ds = DataSet::default();
//!         // Fill your dataset with interesting case
//!         ds
//!     }
//!
//!     #[rstest]
//!     fn should_notify_all_entries(repository: impl Repository) {
//!         //.. test impl ..
//!     }
//! }
//!
//! ```
//!
//! Last but not least, fixtures can be injected like we saw in `alice_and_bob` example.
//!
//! ## Creating parametrized tests
//!
//! You can use use [`[rstest_parametrize]`](attr.rstest_parametrize.html) to create simple
//! table-based tests. Let's see the classic Fibonacci exmple:
//!
//! ```
//! use rstest::rstest_parametrize;
//!
//! #[rstest_parametrize(input, expected,
//!     case(0, 0),
//!     case(1, 1),
//!     case(2, 1),
//!     case(3, 2),
//!     case(4, 3),
//!     case(5, 5),
//!     case(6, 8)
//! )]
//! fn fibonacci_test(input: u32, expected: u32) {
//!     assert_eq!(expected, fibonacci(input))
//! }
//!
//! fn fibonacci(input: u32) -> u32 {
//!     match input {
//!         0 => 0,
//!         1 => 1,
//!         n => fibonacci(n - 2) + fibonacci(n - 1)
//!     }
//! }
//! ```
//! This will generate a bunch of tests, one for every `case()`.


#![cfg_attr(use_proc_macro_diagnostic, feature(proc_macro_diagnostic))]
extern crate proc_macro;

use std::collections::HashMap;

use itertools::Itertools;
use proc_macro2::{Span, TokenStream};
use syn::{FnArg, Ident, ItemFn, parse_macro_input, parse_quote, Stmt, ReturnType, Generics};
use syn::spanned::Spanned;

use quote::quote;

use crate::parse::{
    fixture::FixtureInfo,
    testcase::TestCase,
    rstest::{RsTestAttributes, RsTestInfo, RsTestData},
};
use crate::refident::MaybeIdent;
use crate::resolver::{arg_2_fixture, Resolver};

// Test utility module
#[cfg(test)]
#[macro_use]
pub(crate) mod test;

mod parse;
mod resolver;
mod refident;

fn fn_args(item_fn: &ItemFn) -> impl Iterator<Item=&FnArg> {
    item_fn.sig.inputs.iter()
}

trait IntoOption: Sized {
    fn into_option(self, predicate: fn(&mut Self) -> bool) -> Option<Self>;
}

impl<T: Sized> IntoOption for T {
    fn into_option(mut self, predicate: fn(&mut Self) -> bool) -> Option<Self> {
        match predicate(&mut self) {
            true => Some(self),
            false => None
        }
    }
}

fn trace_arguments<'a>(args: impl Iterator<Item=&'a Ident>, attributes: &RsTestAttributes) -> Option<proc_macro2::TokenStream> {
    args.filter(|&arg| attributes.trace_me(arg))
        .map(|arg|
            parse_quote! {
                println!("{} = {:?}", stringify!(#arg), #arg);
            }
        )
        .map(|stmt: Stmt| stmt)
        .peekable()
        .into_option(|it| it.peek().is_some())
        .map(
            |it|
                quote! {
                    println!("{:-^40}", " TEST ARGUMENTS ");
                    #(#it)*
                }
        )
}

fn resolve_args<'a>(args: impl Iterator<Item=&'a Ident>, resolver: &impl Resolver) -> TokenStream {
    let define_vars = args
        .map(|arg|
            arg_2_fixture(arg, resolver)
        );
    quote! {
        #(#define_vars)*
    }
}

fn resolve_fn_args<'a>(args: impl Iterator<Item=&'a FnArg>, resolver: &impl Resolver) -> TokenStream {
    resolve_args(args.filter_map(MaybeIdent::maybe_ident), resolver)
}

fn render_fn_test<'a>(name: Ident, testfn: &ItemFn, test_impl: Option<&ItemFn>,
                      resolver: impl Resolver, attributes: &'a RsTestAttributes)
                      -> TokenStream {
    let testfn_name = &testfn.sig.ident;
    let args = fn_args_idents(&testfn).cloned().collect::<Vec<_>>();
    let attrs = &testfn.attrs;
    let output = &testfn.sig.output;
    let inject = resolve_fn_args(fn_args(&testfn), &resolver);
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

fn where_predicate_bounded_type(wp: &syn::WherePredicate) -> Option<&syn::Type> {
    match wp {
        syn::WherePredicate::Type(pt) => {
            Some(&pt.bounded_ty)
        }
        _ => None
    }
}

//noinspection RsTypeCheck
fn generics_clean_up<'a>(original: &Generics, inputs: impl Iterator<Item=&'a FnArg>, output: &ReturnType) -> syn::Generics {
    use syn::visit::Visit;
    #[derive(Default, Debug)]
    struct Used(std::collections::HashSet<proc_macro2::Ident>);
    impl<'ast> syn::visit::Visit<'ast> for Used {
        fn visit_type_path(&mut self, i: &'ast syn::TypePath) {
            if i.qself.is_none() && i.path.leading_colon.is_none() && i.path.segments.len() == 1 {
                self.0.insert(i.path.segments.first().unwrap().ident.clone());
            }
        }
    }
    let mut outs = Used::default();
    outs.visit_return_type(output);
    inputs.for_each(|fn_arg| outs.visit_fn_arg(fn_arg));
    let mut result: Generics = original.clone();
    result.params = result.params.into_iter().filter(|p|
        match p {
            syn::GenericParam::Type(tp) if !outs.0.contains(&tp.ident) => false,
            _ => true,
        }
    ).collect();
    result.where_clause.as_mut().map(
        |mut w| w.predicates = w.predicates.clone()
            .into_iter()
            .filter(|wp| where_predicate_bounded_type(wp)
                .and_then(MaybeIdent::maybe_ident)
                .map(|t| outs.0.contains(t))
                .unwrap_or(true)
            ).collect()
    );
    result
}

fn render_partial_impl(fixture: &ItemFn, n: usize, resolver: &impl Resolver, info: &FixtureInfo) -> TokenStream {
    let fixture_inputs = &fixture.sig.inputs;

    let output = info.attributes.extract_partial_type(n).unwrap_or(fixture.sig.output.clone());

    let generics = generics_clean_up(&fixture.sig.generics, fixture_inputs.iter().take(n), &output);
    let where_clause = &generics.where_clause;

    let to_resolve_args = fixture_inputs.iter().skip(n).filter_map(MaybeIdent::maybe_ident);
    let inject = resolve_args(to_resolve_args, resolver);

    let sign_args = fixture_inputs.iter().take(n);
    let fixture_args = fixture_inputs.iter().filter_map(MaybeIdent::maybe_ident);
    let name = Ident::new(&format!("partial_{}", n), Span::call_site());

    quote! {
                pub fn #name #generics (#(#sign_args),*) #output #where_clause {
                    #inject
                    Self::get(#(#fixture_args),*)
                }
            }
}

fn render_fixture<'a>(fixture: ItemFn, info: FixtureInfo)
                      -> TokenStream {
    let name = &fixture.sig.ident;
    let vargs = fn_args_idents(&fixture).cloned().collect::<Vec<_>>();
    let args = &vargs;
    let orig_args = &fixture.sig.inputs;
    let orig_attrs = &fixture.attrs;
    let generics = &fixture.sig.generics;
    let default_output = info.attributes.extract_default_type().unwrap_or(fixture.sig.output.clone());
    let default_generics = generics_clean_up(&fixture.sig.generics, std::iter::empty(), &default_output);
    let default_where_clause = &default_generics.where_clause;
    let body = &fixture.block;
    let where_clause = &fixture.sig.generics.where_clause;
    let output = &fixture.sig.output;
    let visibility = &fixture.vis;
    let resolver = resolver::fixture_resolver(info.data.fixtures());
    let inject = resolve_fn_args(fn_args(&fixture), &resolver);
    let partials = (1..=orig_args.len())
        .map(|n| render_partial_impl(&fixture, n, &resolver, &info)
        );
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

fn fn_args_idents(test: &ItemFn) -> impl Iterator<Item=&Ident> {
    fn_args(&test)
        .filter_map(MaybeIdent::maybe_ident)
}

/// Define a fixture that you can use in all `rstest`'s test arguments. You should just mark your
/// function as `[fixture]` and then use it as a test's argument. Fixture functions can also
/// use other fixtures.
///
/// Let's see a trivial example:
///
/// ```
/// use rstest::*;
///
/// #[fixture]
/// fn twenty_one() -> i32 { 21 }
///
/// #[fixture]
/// fn two() -> i32 { 2 }
///
/// #[fixture]
/// fn injected(twenty_one: i32, two: i32) -> i32 { twenty_one * two }
///
/// #[rstest]
/// fn the_test(injected: i32) {
///     assert_eq!(42, injected)
/// }
/// ```
///
/// You can also partialy inject fixture dependency simply indicate dependency value as fixture
/// argument:
///
/// ```
/// use rstest::*;
///
/// #[fixture]
/// fn base() -> i32 { 1 }
///
/// #[fixture]
/// fn first(base: i32) -> i32 { 1 * base }
///
/// #[fixture]
/// fn second(base: i32) -> i32 { 2 * base }
///
/// #[fixture(second(-3))]
/// fn injected(first: i32, second: i32) -> i32 { first * second }
///
/// #[rstest]
/// fn the_test(injected: i32) {
///     assert_eq!(-6, injected)
/// }
/// ```
/// Note that injected value can be an arbitrary rust expression and not just a literal.
///
/// Sometimes the return type cannot be infered so you must define it: For the few times you may
/// need to do it, you can use the `default<type>`, `partial_n<type>` attribute syntax to define it:
///
/// ```
/// use rstest::*;
/// # use std::fmt::Debug;
///
/// #[fixture]
/// pub fn i() -> u32 {
///     42
/// }
///
/// #[fixture]
/// pub fn j() -> i32 {
///     -42
/// }
///
/// #[fixture(::default<impl Iterator<Item=(u32, i32)>>::partial_1<impl Iterator<Item=(I,i32)>>)]
/// pub fn fx<I, J>(i: I, j: J) -> impl Iterator<Item=(I, J)> {
///     std::iter::once((i, j))
/// }
///
/// #[rstest]
/// fn resolve_by_default<I: Debug + PartialEq>(mut fx: impl Iterator<Item=I>) {
///     assert_eq!((42, -42), fx.next().unwrap())
/// }
///
/// #[rstest(fx(42.0))]
/// fn resolve_partial<I: Debug + PartialEq>(mut fx: impl Iterator<Item=I>) {
///     assert_eq!((42.0, -42), fx.next().unwrap())
/// }
/// ```
/// `partial_i` is the fixture used when you inject the first `i` arguments in test call.
#[proc_macro_attribute]
pub fn fixture(args: proc_macro::TokenStream,
               input: proc_macro::TokenStream)
               -> proc_macro::TokenStream {
    let info: FixtureInfo = parse_macro_input!(args as FixtureInfo);
    let fixture = parse_macro_input!(input as ItemFn);

    let errors = errors_in_fixture(&fixture, &info);
    if errors.is_empty() {
        render_fixture(fixture, info).into()
    } else {
        errors
    }.into()
}

/// Write a test that can be injected with [`[fixture]`](attr.fixture.html)s. You can declare all used fixtures
/// by passing them as a function's arguments.
/// ```
/// use rstest::*;
///
/// #[fixture]
/// fn injected() -> i32 { 42 }
///
/// #[rstest]
/// fn the_test(injected: i32) {
///     assert_eq!(42, injected)
/// }
/// ```
///
/// [`[rstest]`](attr.rstest.html) macro will desugar it to something that is not so far from
///
/// ```
/// #[test]
/// fn the_test() {
///     let injected=injected();
///     assert_eq!(42, injected)
/// }
/// ```
///
/// You can dump all input arguments of your test by using the `trace` parameter for the `[rstest]`
/// attribute.
///
/// ```
/// use rstest::*;
///
/// #[fixture]
/// fn injected() -> i32 { 42 }
///
/// #[rstest(::trace)]
/// fn the_test(injected: i32) {
///     assert_eq!(42, injected)
/// }
/// ```
///
/// Will print an output like
///
/// ```bash
/// Testing started at 14.12 ...
/// ------------ TEST ARGUMENTS ------------
/// injected = 42
/// -------------- TEST START --------------
///
///
/// Expected :42
/// Actual   :43
/// ```
/// If you want to trace input arguments but skip some of them that do not implement the `Debug`
/// trait, you can also use the `notrace(list_of_inputs)` attribute:
///
/// ```
/// # use rstest::*;
/// # struct Xyz;
/// # struct NoSense;
/// #[rstest(::trace::notrace(xzy, have_no_sense))]
/// fn the_test(injected: i32, xyz: Xyz, have_no_sense: NoSense) {
///     assert_eq!(42, injected)
/// }
/// ```
#[proc_macro_attribute]
pub fn rstest(args: proc_macro::TokenStream,
              input: proc_macro::TokenStream)
              -> proc_macro::TokenStream {
    let test = parse_macro_input!(input as ItemFn);
    let info = parse_macro_input!(args as RsTestInfo);

    let errors = errors_in_rstest(&test, &info);

    if errors.is_empty() {
        if info.data.has_list_values() {
            render_matrix_cases(test, info)
        } else if info.data.has_list_values() {
            render_parametrize_cases(test, info)
        } else {
            render_single_case(test, info)
        }
    } else {
        errors
    }.into()
}

fn render_single_case(test: ItemFn, info: RsTestInfo) -> TokenStream {
    let name = &test.sig.ident;
    let resolver = resolver::fixture_resolver(info.data.fixtures());

    render_fn_test(name.clone(), &test, Some(&test), resolver, &info.attributes)
}

fn fn_args_has_ident(fn_decl: &ItemFn, ident: &Ident) -> bool {
    fn_args(fn_decl)
        .filter_map(MaybeIdent::maybe_ident)
        .find(|&id| id == ident)
        .is_some()
}

type Errors<'a> = Box<dyn Iterator<Item=syn::Error> + 'a>;

fn missed_arguments_errors<'a, I: MaybeIdent + Spanned + 'a>(test: &'a ItemFn, args: impl Iterator<Item=&'a I> + 'a) -> Errors<'a> {
    Box::new(
        args
            .filter_map(|it| it.maybe_ident().map(|ident| (it, ident)))
            .filter(move |(_, ident)| !fn_args_has_ident(test, ident))
            .map(|(missed, ident)| syn::Error::new(missed.span(),
                                                   &format!("Missed argument: '{}' should be a test function argument.", ident),
            ))
    )
}

fn duplicate_arguments_errors<'a, I: MaybeIdent + Spanned + 'a>(args: impl Iterator<Item=&'a I> + 'a) -> Errors<'a> {
    let mut used = HashMap::new();
    Box::new(
        args
            .filter_map(|it| it.maybe_ident().map(|ident| (it, ident)))
            .filter_map(move |(it, ident)|
                {
                    let name = ident.to_string();
                    let is_duplicate = used.contains_key(&name);
                    used.insert(name, it);
                    match is_duplicate {
                        true => Some((it, ident)),
                        false => None
                    }
                })
            .map(|(duplicate, ident)|
                syn::Error::new(duplicate.span(),
                                &format!("Duplicate argument: '{}' is already defined.", ident),
                )
            )
    )
}

fn invalid_case_errors(params: &RsTestData) -> Errors {
    let n_args = params.case_args().count();
    Box::new(
        params.cases()
            .filter(move |case| case.args.len() != n_args)
            .map(|case|
                syn::Error::new_spanned(&case, "Wrong case signature: should match the given parameters list.")
            )
    )
}

fn case_args_without_cases(params: &RsTestData) -> Errors {
    if !params.has_cases() {
        return Box::new(params.case_args().map(|a|
            syn::Error::new(a.span(), "No cases for this argument."))
        );
    }
    Box::new(
        std::iter::empty()
    )
}

fn errors_in_rstest(test: &ItemFn, info: &parse::rstest::RsTestInfo) -> TokenStream{
    missed_arguments_errors(test, info.data.items.iter())
        .chain(duplicate_arguments_errors(info.data.items.iter()))
        .chain(invalid_case_errors(&info.data))
        .chain(case_args_without_cases(&info.data))
        .map(|e| e.to_compile_error())
        .collect()
}

fn errors_in_fixture(test: &ItemFn, info: &parse::fixture::FixtureInfo) -> TokenStream {
    missed_arguments_errors(test, info.data.items.iter())
        .chain(duplicate_arguments_errors(info.data.items.iter()))
        .map(|e| e.to_compile_error())
        .collect()
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

fn render_cases<R: Resolver>(test: ItemFn, cases: impl Iterator<Item=TestCaseRender<R>>, attributes: RsTestAttributes) -> TokenStream {
    let fname = &test.sig.ident;
    let cases = cases.map(|case| case.render(&test, &attributes));

    quote! {
        #[cfg(test)]
        #test

        #[cfg(test)]
        mod #fname {
            use super::*;

            #(#cases)*
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
        .description.as_ref()
        .map(|d| format!("_{}", d))
        .unwrap_or_default();
    format!("case_{:0len$}{d}", index, len = display_len, d = description)
}

fn render_parametrize_cases(test: ItemFn, info: RsTestInfo) -> TokenStream {
    let RsTestInfo { data, attributes } = info;
    let display_len = data.cases().count().display_len();

    let cases = data.cases()
        .enumerate()
        .map({
            let span = test.sig.ident.span();
            let data = &data;
            move |(n, case)|
                {
                    let resolver_fixtures = resolver::fixture_resolver(data.fixtures());
                    let resolver_case = data.case_args()
                        .map(|a| a.to_string())
                        .zip(case.args.iter())
                        .collect::<HashMap<_, _>>();
                    TestCaseRender::new(Ident::new(&format_case_name(case, n + 1, display_len), span),
                                        (resolver_case, resolver_fixtures))
                }
        }
        );

    render_cases(test, cases, attributes.into())
}

fn render_matrix_cases(test: ItemFn, info: parse::rstest::RsTestInfo) -> TokenStream {
    let parse::rstest::RsTestInfo { data, attributes, .. } = info;
    let span = test.sig.ident.span();
    let fname = &test.sig.ident;

    let display_len = data.cases().count().display_len();
    let cases: Vec<(_, _)> = data.cases()
        .enumerate()
        .map({
            let data = &data;
            move |(n, case)|
                {
                    let resolver_case = data.case_args()
                        .map(|a| a.to_string())
                        .zip(case.args.iter())
                        .collect::<HashMap<_, _>>();
                    (resolver_case, Ident::new(&format_case_name(case, n + 1, display_len), span))
                }
        }
        ).collect();

    let base_resolver = resolver::fixture_resolver(data.fixtures());
    if cases.is_empty() {
        // Steps:
        // 1. pack data P=(ident, expr, (pos, max_len)) in one iterator for each variable
        // 2. do a cartesian product of iterators to build all cases (every case is a vector of P)
        // 3. format case by packed data vector
        let test_cases = data.list_values()
            .map(|group|
                group.values.iter()
                    .enumerate()
                    .map(move |(pos, expr)| (&group.arg, expr, (pos, group.values.len())))
            )
            .multi_cartesian_product()
            .map(|c| {
                let args_indexes = c.iter()
                    .map(|(_, _, (index, max))|
                        format!("{:0len$}", index + 1, len = max.display_len())
                    )
                    .collect::<Vec<_>>()
                    .join("_");
                let name = format!("case_{}", args_indexes);
                let resolver_case = c.into_iter()
                    .map(|(a, e, _)| (a.to_string(), e))
                    .collect::<HashMap<_, _>>();
                TestCaseRender::new(Ident::new(&name, span),
                                    (resolver_case, &base_resolver))
            }
            );

        let test_cases = test_cases.map(|test_case| test_case.render(&test, &attributes));

        quote! {
        #[cfg(test)]
        #test

        #[cfg(test)]
        mod #fname {
                use super::*;

                #(#test_cases)*
            }
        }
    } else {

        let  test_cases = cases.into_iter().map(
            |(resolver, case_name)| {
                let resolver = (resolver, &base_resolver);
                let test_cases = data.list_values()
                    .map(|group|
                        group.values.iter()
                            .enumerate()
                            .map(move |(pos, expr)| (&group.arg, expr, (pos, group.values.len())))
                    )
                    .multi_cartesian_product()
                    .map(|c| {
                        let args_indexes = c.iter()
                            .map(|(_, _, (index, max))|
                                format!("{:0len$}", index + 1, len = max.display_len())
                            )
                            .collect::<Vec<_>>()
                            .join("_");
                        let name = format!("case_{}", args_indexes);
                        let resolver_case = c.into_iter()
                            .map(|(a, e, _)| (a.to_string(), e))
                            .collect::<HashMap<_, _>>();
                        TestCaseRender::new(Ident::new(&name, span),
                                            (resolver_case, &resolver)
                        )
                    }
                    )
                    .map(|test_case| test_case.render(&test, &attributes));
                quote! {
                    mod #case_name {
                        use super::*;

                        #(#test_cases)*
                    }
                }
            }
        );

        quote! {
        #[cfg(test)]
        #test

        #[cfg(test)]
        mod #fname {
                use super::*;

                #(#test_cases)*
            }
        }
    }
}

/// Write table-based tests: you must indicate the arguments that you want use in your cases
/// and provide them for each case you want to test.
///
/// `rstest_parametrize` generates an independent test for each case.
///
/// ```
/// # use rstest::rstest_parametrize;
/// #[rstest_parametrize(input, expected,
///     case(0, 0),
///     case(1, 1),
///     case(2, 1),
///     case(3, 2),
///     case(4, 3)
/// )]
/// fn fibonacci_test(input: u32, expected: u32) {
///     assert_eq!(expected, fibonacci(input))
/// }
/// ```
///
/// Running `cargo test` in this case executes five tests:
///
/// ```bash
/// running 5 tests
/// test fibonacci_test::case_1 ... ok
/// test fibonacci_test::case_2 ... ok
/// test fibonacci_test::case_3 ... ok
/// test fibonacci_test::case_4 ... ok
/// test fibonacci_test::case_5 ... ok
///
/// test result: ok. 5 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
/// ```
///
/// Like in [`[rstest]`](attr.rstest.html) you can inject fixture values and every parameter that
/// isn't mapped in `case()`s will be resolved as default `fixture`.
///
/// In general `rstest_parametrize`'s syntax is:
///
/// ```norun
/// rstest_parametrize(ident_1,..., ident_n,
///     case[::description_1](val_1_1, ..., val_n_1),
///     ...,
///     case[::description_m](val_1_m, ..., val_n_m)[,]
///     [fixture_1(...]
///     [...,]
///     [fixture_k(...)]
///     [::attribute_1[:: ... [::attribute_k]]]
/// )
/// ```
/// * `ident_x` should be a valid function argument name
/// * `val_x_y` should be a valid rust expression that can be assigned to `ident_x` function argument
/// * `description_z` when present should be a valid Rust identity
/// * `fixture_v(...)` should be a valid function argument and a [`[fixture]`](attr.fixture.html) fixture function
/// * attributes now can be just `trace` or `notrace(args..)` (see [`[rstest]`](attr.rstest.html)
///
/// Function's arguments can be present just once as identity or fixture.
///
/// Functions marked by `rstest_parametrize` can use generics, `impl` and `dyn` without any
/// restriction.
///
/// ```
/// # use rstest::rstest_parametrize;
/// #[rstest_parametrize(input, expected,
///     case("foo", 3),
///     case(String::from("bar"), 3),
/// )]
/// fn len<S: AsRef<str>>(input: S, expected: usize) {
///     assert_eq!(expected, input.as_ref().len())
/// }
///
/// #[rstest_parametrize(input, expected,
///     case("foo", 3),
///     case(String::from("bar"), 3),
/// )]
/// fn len_by_impl(input: impl AsRef<str>, expected: usize) {
///     assert_eq!(expected, input.as_ref().len())
/// }
/// ```
#[proc_macro_attribute]
pub fn rstest_parametrize(args: proc_macro::TokenStream, input: proc_macro::TokenStream)
                          -> proc_macro::TokenStream
{
    rstest(args, input)
}

/// Write matrix-based tests: you must indicate arguments and values list that you want to test and
/// `rstest_matrix` generate an indipendent test for each argument combination (the carthesian
/// product of values lists).
///
/// ```
/// # use rstest::rstest_matrix;
/// #[rstest_matrix(
///     foo => [42, 24],
///     bar => ["foo", "bar"]
/// )]
/// fn matrix_test(foo: u32, bar: &str) {
///     //... test body
/// }
/// ```
///
/// Running `cargo test` in this case executes four tests:
///
/// ```bash
/// running 4 tests
/// test matrix_test::case_1_1 ... ok
/// test matrix_test::case_1_2 ... ok
/// test matrix_test::case_2_1 ... ok
/// test matrix_test::case_2_2 ... ok
///
/// test result: ok. 4 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
///
/// ```
///
/// Like in [`[rstest]`](attr.rstest.html) you can inject fixture values and every parameter that
/// isn't mapped in `case()`s will be resolved as default `fixture`.
///
/// In general `rstest_matrix`'s syntax is:
///
/// ```norun
/// rstest_matrix(
///     ident_1 => [val_1_1, ..., val_1_m1],
///     ....
///     ident_n => [val_n_1, ..., val_n_mn][,]
///     [fixture_1(...]
///     [...,]
///     [fixture_k(...)]
///     [::attribute_1[:: ... [::attribute_k]]]
/// )
/// ```
/// * `ident_x` should be a valid function argument name
/// * `val_x_y` should be a valid rust expression that can be assigned to `ident_x` function argument
/// * `fixture_v(...)` should be a valid function argument and a [`[fixture]`](attr.fixture.html) fixture function
/// * attributes now can be just `trace` or `notrace(args..)` (see [`[rstest]`](attr.rstest.html)
///
/// Function's arguments can be present just once as identity or fixture.
///
/// Functions marked by `rstest_matrix` can use generics, `impl` and `dyn` without any
/// restriction.
///
/// ```
/// # use rstest::rstest_matrix;
/// #[rstest_matrix(
///     foo => ["foo", String::from("foo")]
/// )]
/// fn len<S: AsRef<str>>(foo: S) {
///     assert_eq!(3, input.as_ref().len())
/// }
///
/// #[rstest_matrix(
///     foo => ["foo", String::from("foo")]
/// )]
/// fn len_by_impl(foo: impl AsRef<str>) {
///     assert_eq!(3, input.as_ref().len())
/// }
/// ```
#[proc_macro_attribute]
pub fn rstest_matrix(args: proc_macro::TokenStream, input: proc_macro::TokenStream)
                     -> proc_macro::TokenStream
{
    rstest(args, input)
}

#[cfg(test)]
mod render {
    use syn::{
        ItemFn, ItemMod, parse::{Parse, ParseStream, Result}, parse2,
        parse_str, punctuated, visit::Visit,
    };

    use crate::resolver::*;
    use crate::test::{*, fixture, assert_eq};

    use super::*;

    fn fn_args(item: &ItemFn) -> punctuated::Iter<'_, FnArg> {
        item.sig.inputs.iter()
    }

    fn first_arg_ident(ast: &ItemFn) -> &Ident {
        fn_args(&ast)
            .next().unwrap()
            .maybe_ident().unwrap()
    }

    #[test]
    fn extract_fixture_call_arg() {
        let ast = "fn foo(fix: String) {}".ast();
        let arg = first_arg_ident(&ast);

        let line = arg_2_fixture(arg, &EmptyResolver);

        assert_eq!(line, "let fix = fix::default();".ast());
    }

    #[test]
    fn extract_fixture_should_not_add_mut() {
        let ast = "fn foo(mut fix: String) {}".ast();
        let arg = first_arg_ident(&ast);

        let line = arg_2_fixture(arg, &EmptyResolver);

        assert_eq!(line, "let fix = fix::default();".ast());
    }

    #[test]
    fn arg_2_fixture_str_should_use_passed_fixture_if_any() {
        let ast = "fn foo(mut fix: String) {}".ast();
        let arg = first_arg_ident(&ast);
        let call = expr("bar()");
        let mut resolver = HashMap::new();
        resolver.insert("fix".to_string(), &call);

        let line = arg_2_fixture(arg, &resolver);

        assert_eq!(line, "let fix = bar();".ast());
    }

    #[test]
    fn resolver_should_return_the_given_expression() {
        let ast = parse_str("fn function(mut foo: String) {}").unwrap();
        let arg = first_arg_ident(&ast);
        let expected = expr("bar()");
        let mut resolver = HashMap::new();

        resolver.insert("foo".to_string(), &expected);

        assert_eq!(expected, (&resolver).resolve(&arg).unwrap().into_owned())
    }

    #[test]
    fn resolver_should_return_none_for_unknown_argument() {
        let ast = "fn function(mut fix: String) {}".ast();
        let arg = first_arg_ident(&ast);

        assert!(EmptyResolver.resolve(&arg).is_none())
    }

    mod fn_test_should {
        use pretty_assertions::assert_eq;
        use proc_macro2::Span;

        use super::*;

        #[test]
        fn add_return_type_if_any() {
            let ast: ItemFn = "fn function(mut fix: String) -> Result<i32, String> { Ok(42) }".ast();

            let tokens = render_fn_test(Ident::new("new_name", Span::call_site()),
                                        &ast, None, EmptyResolver, &Default::default());

            let result: ItemFn = parse2(tokens).unwrap();

            assert_eq!(result.sig.ident.to_string(), "new_name");
            assert_eq!(result.sig.output, ast.sig.output);
        }

        #[test]
        fn include_given_function() {
            let input_fn: ItemFn = r#"
                pub fn test<R: AsRef<str>, B>(mut s: String, v: &u32, a: &mut [i32], r: R) -> (u32, B, String, &str)
                        where B: Borrow<u32>
                {
                    let some = 42;
                    assert_eq!(42, some);
                }
                "#.ast();

            let tokens = render_fn_test(Ident::new("new_name", Span::call_site()),
                                        &input_fn, Some(&input_fn), EmptyResolver, &Default::default());

            let result: ItemFn = parse2(tokens).unwrap();
            let first_stmt = result.block.stmts.get(0).unwrap();

            let inner_fn: ItemFn = parse_quote! {
                #first_stmt
            };

            assert_eq!(inner_fn, inner_fn);
        }
    }

    struct TestsGroup {
        requested_test: ItemFn,
        module: ItemMod,
    }

    impl Parse for TestsGroup {
        fn parse(input: ParseStream) -> Result<Self> {
            Ok(Self {
                requested_test: input.parse()?,
                module: input.parse()?,
            })
        }
    }

    /// To extract all test functions
    struct TestFunctions(Vec<ItemFn>);

    impl TestFunctions {
        fn is_test_fn(item_fn: &ItemFn) -> bool {
            item_fn.attrs.iter().filter(|&a|
                a.path == parse_str::<syn::Path>("test").unwrap())
                .next().is_some()
        }
    }

    impl<'ast> Visit<'ast> for TestFunctions {
        //noinspection RsTypeCheck
        fn visit_item_fn(&mut self, item_fn: &'ast ItemFn) {
            if Self::is_test_fn(item_fn) {
                self.0.push(item_fn.clone())
            }
        }
    }

    /// To extract all submodules
    struct SubModules(Vec<ItemMod>);

    impl<'ast> Visit<'ast> for SubModules {
        //noinspection RsTypeCheck
        fn visit_item_mod(&mut self, item_mod: &'ast ItemMod) {
            self.0.push(item_mod.clone())
        }
    }

    impl TestsGroup {
        pub fn get_test_functions(&self) -> Vec<ItemFn> {
            let mut f = TestFunctions(vec![]);

            f.visit_item_mod(&self.module);
            f.0
        }

        pub fn get_submodules(&self) -> Vec<ItemMod> {
            let mut f = SubModules(vec![]);

            self.module.content.as_ref().map(|(_, items)| items.iter().for_each(|it| f.visit_item(it)));
            f.0
        }
    }

    impl From<TokenStream> for TestsGroup {
        fn from(tokens: TokenStream) -> Self {
            syn::parse2::<TestsGroup>(tokens).unwrap()
        }
    }

    mod cases {
        use crate::parse::{
            rstest::{RsTestInfo, RsTestData, RsTestItem},
            testcase::TestCase,
        };

        use super::{*, assert_eq};

        fn into_rstest_data(item_fn: &ItemFn) -> RsTestData {
            RsTestData {
                items: fn_args_idents(item_fn)
                    .cloned()
                    .map(RsTestItem::CaseArgName)
                    .collect(),
            }
        }

        #[test]
        fn should_create_a_module_named_as_test_function() {
            let item_fn = parse_str::<ItemFn>("fn should_be_the_module_name(mut fix: String) {}").unwrap();
            let data = into_rstest_data(&item_fn);
            let tokens = render_parametrize_cases(item_fn.clone(), data.into());

            let output = TestsGroup::from(tokens);

            assert_eq!(output.module.ident, "should_be_the_module_name");
        }

        #[test]
        fn should_copy_user_function() {
            let item_fn = parse_str::<ItemFn>(
                r#"fn should_be_the_module_name(mut fix: String) { println!("user code") }"#
            ).unwrap();
            let data = into_rstest_data(&item_fn);
            let tokens = render_parametrize_cases(item_fn.clone(), data.into());

            let mut output = TestsGroup::from(tokens);

            output.requested_test.attrs = vec![];
            assert_eq!(output.requested_test, item_fn);
        }

        #[test]
        fn should_mark_user_function_as_test() {
            let item_fn = parse_str::<ItemFn>(
                r#"fn should_be_the_module_name(mut fix: String) { println!("user code") }"#
            ).unwrap();
            let data = into_rstest_data(&item_fn);
            let tokens = render_parametrize_cases(item_fn.clone(), data.into());

            let output = TestsGroup::from(tokens);

            let expected = parse2::<ItemFn>(quote! {
                #[cfg(test)]
                fn some() {}
            }).unwrap().attrs;

            assert_eq!(expected, output.requested_test.attrs);
        }

        #[test]
        fn should_mark_module_as_test() {
            let item_fn = parse_str::<ItemFn>(
                r#"fn should_be_the_module_name(mut fix: String) { println!("user code") }"#
            ).unwrap();
            let data = into_rstest_data(&item_fn);
            let tokens = render_parametrize_cases(item_fn.clone(), data.into());

            let output = TestsGroup::from(tokens);

            let expected = parse2::<ItemMod>(quote! {
                #[cfg(test)]
                mod some {}
            }).unwrap().attrs;

            assert_eq!(expected, output.module.attrs);
        }


        fn one_simple_case() -> (ItemFn, RsTestInfo) {
            let item_fn = parse_str::<ItemFn>(
                r#"fn test(mut fix: String) { println!("user code") }"#
            ).unwrap();
            let mut info: RsTestInfo = into_rstest_data(&item_fn).into();
            info.push_case(TestCase::from(r#"String::from("3")"#));
            (item_fn, info)
        }

        fn some_simple_cases(cases: i32) -> (ItemFn, RsTestInfo) {
            let item_fn = parse_str::<ItemFn>(
                r#"fn test(mut fix: String) { println!("user code") }"#
            ).unwrap();
            let mut info: RsTestInfo = into_rstest_data(&item_fn).into();
            info.extend((0..cases).map(|_| TestCase::from(r#"String::from("3")"#)));
            (item_fn, info)
        }

        #[test]
        fn should_add_a_test_case() {
            let (item_fn, info) = one_simple_case();

            let tokens = render_parametrize_cases(item_fn.clone(), info);

            let tests = TestsGroup::from(tokens).get_test_functions();

            assert_eq!(1, tests.len());
            assert!(&tests[0].sig.ident.to_string().starts_with("case_"))
        }

        #[test]
        fn case_number_should_starts_from_1() {
            let (item_fn, info) = one_simple_case();

            let tokens = render_parametrize_cases(item_fn.clone(), info);

            let tests = TestsGroup::from(tokens).get_test_functions();

            assert!(&tests[0].sig.ident.to_string().starts_with("case_1"), "Should starts with case_1 but is {}", tests[0].sig.ident.to_string())
        }

        #[test]
        fn should_add_all_test_cases() {
            let (item_fn, info) = some_simple_cases(5);

            let tokens = render_parametrize_cases(item_fn.clone(), info);

            let tests = TestsGroup::from(tokens).get_test_functions();

            let valid_names = tests.iter()
                .filter(|it| it.sig.ident.to_string().starts_with("case_"));
            assert_eq!(5, valid_names.count())
        }

        #[test]
        fn should_left_pad_case_number_by_zeros() {
            let (item_fn, info) = some_simple_cases(1000);

            let tokens = render_parametrize_cases(item_fn.clone(), info);

            let tests = TestsGroup::from(tokens).get_test_functions();

            let first_name = tests[0].sig.ident.to_string();
            let last_name = tests[999].sig.ident.to_string();

            assert!(first_name.ends_with("_0001"), "Should ends by _0001 but is {}", first_name);
            assert!(last_name.ends_with("_1000"), "Should ends by _1000 but is {}", last_name);

            let valid_names = tests.iter()
                .filter(|it| it.sig.ident.to_string().len() == first_name.len());
            assert_eq!(1000, valid_names.count())
        }

        #[test]
        fn should_use_description_if_any() {
            let (item_fn, mut info) = one_simple_case();
            let description = "show_this_description";

            if let &mut RsTestItem::TestCase(ref mut case) = &mut info.data.items[1] {
                case.description = Some(parse_str::<Ident>(description).unwrap());
            } else {
                panic!("Test case should be the second one");
            }

            let tokens = render_parametrize_cases(item_fn.clone(), info);

            let tests = TestsGroup::from(tokens).get_test_functions();

            assert!(tests[0].sig.ident.to_string().ends_with(&format!("_{}", description)));
        }
    }

    mod matrix_cases {
        use unindent::Unindent;

        /// Should test matrix tests render without take in account MatrixInfo to RsTestInfo
        /// transformation

        use super::{*, assert_eq};
        use crate::parse::vlist::ValueList;

        fn into_rstest_data(item_fn: &ItemFn) -> RsTestData {
            RsTestData {
                items: fn_args_idents(item_fn)
                    .cloned()
                    .map(|it|
                        ValueList { arg: it, values: vec![] }.into()
                    )
                    .collect(),
            }
        }

        #[test]
        fn should_create_a_module_named_as_test_function() {
            let item_fn = parse_str::<ItemFn>("fn should_be_the_module_name(mut fix: String) {}").unwrap();
            let data = into_rstest_data(&item_fn);
            let tokens = render_matrix_cases(item_fn.clone(), data.into());

            let output = TestsGroup::from(tokens);

            assert_eq!(output.module.ident, "should_be_the_module_name");
        }

        #[test]
        fn should_copy_user_function() {
            let item_fn = parse_str::<ItemFn>(
                r#"fn should_be_the_module_name(mut fix: String) { println!("user code") }"#
            ).unwrap();
            let data = into_rstest_data(&item_fn);
            let tokens = render_matrix_cases(item_fn.clone(), data.into());

            let mut output = TestsGroup::from(tokens);

            output.requested_test.attrs = vec![];
            assert_eq!(output.requested_test, item_fn);
        }

        #[test]
        fn should_mark_user_function_as_test() {
            let item_fn = parse_str::<ItemFn>(
                r#"fn should_be_the_module_name(mut fix: String) { println!("user code") }"#
            ).unwrap();
            let data = into_rstest_data(&item_fn);
            let tokens = render_matrix_cases(item_fn.clone(), data.into());

            let output = TestsGroup::from(tokens);

            let expected = parse2::<ItemFn>(quote! {
                #[cfg(test)]
                fn some() {}
            }).unwrap().attrs;

            assert_eq!(expected, output.requested_test.attrs);
        }

        #[test]
        fn should_mark_module_as_test() {
            let item_fn = parse_str::<ItemFn>(
                r#"fn should_be_the_module_name(mut fix: String) { println!("user code") }"#
            ).unwrap();
            let data = into_rstest_data(&item_fn);
            let tokens = render_matrix_cases(item_fn.clone(), data.into());

            let output = TestsGroup::from(tokens);

            let expected = parse2::<ItemMod>(quote! {
                #[cfg(test)]
                mod some {}
            }).unwrap().attrs;

            assert_eq!(expected, output.module.attrs);
        }

        fn one_simple_case() -> (ItemFn, RsTestInfo) {
            let item_fn = parse_str::<ItemFn>(
                r#"fn test(mut fix: String) { println!("user code") }"#
            ).unwrap();
            let info = RsTestInfo {
                data: RsTestData {
                    items: vec![
                        ValueList { arg: ident("fix"), values: vec![expr(r#""value""#)] }.into()
                    ]
                },
                ..Default::default()
            };
            (item_fn, info)
        }

        #[test]
        fn should_add_a_test_case() {
            let (item_fn, info) = one_simple_case();

            let tokens = render_matrix_cases(item_fn.clone(), info);

            let tests = TestsGroup::from(tokens).get_test_functions();

            assert_eq!(1, tests.len());
            assert!(&tests[0].sig.ident.to_string().starts_with("case_"))
        }

        #[test]
        fn should_add_a_test_cases_from_all_combinations() {
            let item_fn = parse_str::<ItemFn>(
                r#"fn test(first: u32, second: u32, third: u32) { println!("user code") }"#
            ).unwrap();
            let info = RsTestInfo {
                data: RsTestData {
                    items: vec![
                        values_list("first", ["1", "2"].as_ref()).into(),
                        values_list("second", ["3", "4"].as_ref()).into(),
                        values_list("third", ["5", "6"].as_ref()).into(),
                    ]
                },
                ..Default::default()
            };

            let tokens = render_matrix_cases(item_fn.clone(), info);

            let tests = TestsGroup::from(tokens).get_test_functions();

            let tests = tests.into_iter()
                .map(|t| t.sig.ident.to_string())
                .collect::<Vec<_>>()
                .join("\n");

            assert_eq!(tests, "
                    case_1_1_1
                    case_1_1_2
                    case_1_2_1
                    case_1_2_2
                    case_2_1_1
                    case_2_1_2
                    case_2_2_1
                    case_2_2_2".unindent()
            )
        }

        #[test]
        fn should_pad_case_index() {
            let item_fn = parse_str::<ItemFn>(
                r#"fn test(first: u32, second: u32, third: u32) { println!("user code") }"#
            ).unwrap();
            let values = (1..=100).map(|i| i.to_string()).collect::<Vec<_>>();
            let info = RsTestInfo {
                data: RsTestData {
                    items: vec![
                        values_list("first", values.as_ref()).into(),
                        values_list("second", values[..10].as_ref()).into(),
                        values_list("third", values[..2].as_ref()).into(),
                    ]
                },
                ..Default::default()
            };

            let tokens = render_matrix_cases(item_fn.clone(), info);

            let tests = TestsGroup::from(tokens).get_test_functions();

            assert_eq!(tests[0].sig.ident.to_string(), "case_001_01_1");
            assert_eq!(tests.last().unwrap().sig.ident.to_string(), "case_100_10_2");
        }
    }

    mod cases_and_values_lists {
        use super::{*, assert_eq};

        #[test]
        fn should_module_for_each_cases() {
            let item_fn = parse_str::<ItemFn>(r#"
                fn should_be_the_outer_module_name(
                    fix: u32,
                    a: f64, b: f32,
                    x: i32, y: i32) {}"#).unwrap();
            let data = RsTestData {
                items: vec![fixture("fix", vec!["2"]).into(),
                            ident("a").into(), ident("b").into(),
                            vec!["1f64", "2f32"].into_iter().collect::<TestCase>().into(),
                            vec!["3f64", "4f32"].into_iter().collect::<TestCase>().into(),
                            values_list("x", &["12", "-2"]).into(),
                            values_list("y", &["-3", "42"]).into(),
                ],
            };
            let tokens = render_matrix_cases(item_fn.clone(), data.into());

            let output = TestsGroup::from(tokens);

            assert_eq!(output.module.ident, "should_be_the_outer_module_name");
            assert_eq!(2, output.get_submodules().len());
        }
    }

    mod generics_clean_up {
        use super::{*, assert_eq};

        #[test]
        fn should_remove_generics_not_in_output() {
            // Should remove all generics parameters that are not present in output
            let item_fn = parse_str::<ItemFn>(
                r#"
                    pub fn test<R: AsRef<str>, B, F, H: Iterator<Item=u32>>() -> (H, B, String, &str)
                            where F: ToString,
                            B: Borrow<u32>

                    { }
                "#
            ).unwrap();

            let expected = parse_str::<ItemFn>(
                r#"
                    pub fn test<B, H: Iterator<Item=u32>>() -> (H, B, String, &str)
                            where B: Borrow<u32>
                    { }
                "#
            ).unwrap();

            let cleaned = generics_clean_up(&item_fn.sig.generics, item_fn.sig.inputs.iter(), &item_fn.sig.output);

            assert_eq!(expected.sig.generics, cleaned);
        }

        #[test]
        fn should_not_remove_generics_used_in_arguments() {
            // Should remove all generics parameters that are not present in output
            let item_fn = parse_str::<ItemFn>(
                r#"
                    pub fn test<R: AsRef<str>, B, F, H: Iterator<Item=u32>>
                        (h: H, it: impl Iterator<Item=R>, j: &[B])
                        where F: ToString,
                        B: Borrow<u32>
                    { }
                "#
            ).unwrap();

            let expected = parse_str::<ItemFn>(
                r#"
                    pub fn test<R: AsRef<str>, B, H: Iterator<Item=u32>>
                        (h: H, it: impl Iterator<Item=R>, j: &[B])
                        where
                        B: Borrow<u32>
                    { }
                "#
            ).unwrap();

            let cleaned = generics_clean_up(&item_fn.sig.generics, item_fn.sig.inputs.iter(), &item_fn.sig.output);

            dbg!(item_fn.sig.inputs);

            assert_eq!(expected.sig.generics, cleaned);
        }
    }

    mod fixture {
        use syn::{ItemImpl, ItemStruct};

        use crate::parse::{Attribute, Attributes};

        use super::{*, assert_eq};

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
            let item_fn = parse_str::<ItemFn>(
                code.as_ref()
            ).unwrap();

            let tokens = render_fixture(item_fn.clone(), Default::default());
            (item_fn, parse2(tokens).unwrap())
        }

        fn test_maintains_function_visibility(code: &str) {
            let (item_fn, out) = parse_fixture(code);

            assert_eq!(item_fn.vis, out.fixture.vis);
            assert_eq!(item_fn.vis, out.orig.vis);
        }

        #[test]
        fn should_maintains_pub_visibility() {
            test_maintains_function_visibility(
                r#"pub fn test() { }"#
            );
        }

        #[test]
        fn should_maintains_no_pub_visibility() {
            test_maintains_function_visibility(
                r#"fn test() { }"#
            );
        }

        fn select_method<S: AsRef<str>>(impl_code: ItemImpl, name: S) -> Option<syn::ImplItemMethod> {
            impl_code.items.into_iter()
                .filter_map(|ii| match ii {
                    syn::ImplItem::Method(f) => Some(f),
                    _ => None
                })
                .find(|f| f.sig.ident == name.as_ref())
        }

        #[test]
        fn should_implement_a_get_method_with_input_fixture_signature() {
            let (item_fn, out) = parse_fixture(
                r#"
                pub fn test<R: AsRef<str>, B>(mut s: String, v: &u32, a: &mut [i32], r: R) -> (u32, B, String, &str)
                        where B: Borrow<u32>
                { }
                "#);


            let mut signature = select_method(out.core_impl, "get")
                .unwrap()
                .sig;

            signature.ident = item_fn.sig.ident.clone();

            assert_eq!(item_fn.sig, signature);
        }

        #[test]
        fn should_implement_a_default_method_with_input_cleaned_fixture_signature_and_no_args() {
            let (item_fn, out) = parse_fixture(
                r#"
                pub fn test<R: AsRef<str>, B, F, H: Iterator<Item=u32>>(mut s: String, v: &u32, a: &mut [i32], r: R) -> (H, B, String, &str)
                        where F: ToString,
                        B: Borrow<u32>

                { }
                "#);

            let default_decl = select_method(out.core_impl, "default")
                .unwrap()
                .sig;

            let expected = parse_str::<ItemFn>(
                r#"
                pub fn default<B, H: Iterator<Item=u32>>() -> (H, B, String, &str)
                        where B: Borrow<u32>
                { }
                "#
            ).unwrap();


            assert_eq!(expected.sig.generics, default_decl.generics);
            assert_eq!(item_fn.sig.output, default_decl.output);
            assert!(default_decl.inputs.is_empty());
        }

        #[test]
        fn should_use_default_return_type_if_any() {
            let item_fn = parse_str::<ItemFn>(
                r#"
                pub fn test<R: AsRef<str>, B, F, H: Iterator<Item=u32>>() -> (H, B)
                        where F: ToString,
                        B: Borrow<u32>
                { }
                "#
            ).unwrap();

            let tokens = render_fixture(item_fn.clone(),
                                        FixtureInfo {
                                            attributes: Attributes {
                                                attributes: vec![
                                                    Attribute::Type(
                                                        parse_str("default").unwrap(),
                                                        parse_str("(impl Iterator<Item=u32>, B)").unwrap(),
                                                    )
                                                ]
                                            }.into(),
                                            ..Default::default()
                                        });
            let out: FixtureOutput = parse2(tokens).unwrap();

            let expected = parse_str::<syn::ItemFn>(
                r#"
                pub fn default<B>() -> (impl Iterator<Item=u32>, B)
                        where B: Borrow<u32>
                { }
                "#
            ).unwrap();

            let default_decl = select_method(out.core_impl, "default")
                .unwrap()
                .sig;

            assert_eq!(expected.sig, default_decl);
        }

        #[test]
        fn should_implement_partial_methods() {
            let (item_fn, out) = parse_fixture(
                r#"
                pub fn test(mut s: String, v: &u32, a: &mut [i32]) -> usize
                { }
                "#);

            let partials = (1..=3).map(|n|
                select_method(out.core_impl.clone(), format!("partial_{}", n))
                    .unwrap()
                    .sig)
                .collect::<Vec<_>>();

            // All 3 methods found

            assert!(select_method(out.core_impl, "partial_4").is_none());

            let expected_1 = parse_str::<ItemFn>(
                r#"
                pub fn partial_1(mut s: String) -> usize
                { }
                "#
            ).unwrap();


            assert_eq!(expected_1.sig, partials[0]);
            for p in partials {
                assert_eq!(item_fn.sig.output, p.output);
            }
        }

        #[test]
        fn should_clean_generics_in_partial_methods() {
            let (_, out) = parse_fixture(
                r#"
                pub fn test<S: AsRef<str>, U: AsRef<u32>, F: ToString>(mut s: S, v: U) -> F
                { }
                "#);

            let partials = (1..=2).map(|n|
                select_method(out.core_impl.clone(), format!("partial_{}", n))
                    .unwrap()
                    .sig)
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
        fn should_use_partial_return_type_if_any() {
            let item_fn = parse_str::<ItemFn>(
                r#"
                pub fn test<R: AsRef<str>, B, F, H: Iterator<Item=u32>>(h: H, b: B) -> (H, B)
                        where F: ToString,
                        B: Borrow<u32>
                { }
                "#
            ).unwrap();

            let tokens = render_fixture(item_fn.clone(),
                                        FixtureInfo {
                                            attributes: Attributes {
                                                attributes: vec![
                                                    Attribute::Type(
                                                        parse_str("partial_1").unwrap(),
                                                        parse_str("(H, impl Iterator<Item=u32>)").unwrap(),
                                                    )
                                                ]
                                            }.into(),
                                            ..Default::default()
                                        });
            let out: FixtureOutput = parse2(tokens).unwrap();

            let expected = parse_str::<syn::ItemFn>(
                r#"
                pub fn partial_1<H: Iterator<Item=u32>>(h: H) -> (H, impl Iterator<Item=u32>)
                { }
                "#
            ).unwrap();

            let partial = select_method(out.core_impl, "partial_1")
                .unwrap();

            assert_eq!(expected.sig, partial.sig);
        }
    }
}
