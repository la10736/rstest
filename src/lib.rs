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
//! ## Injecting fixtures as funtion arguments
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

use proc_macro2::TokenStream;
use syn::{ArgCaptured, FnArg, Generics, Ident, ItemFn,
          parse_macro_input, Pat, ReturnType, Stmt};

use error::error_statement;
use parse::{Modifiers, RsTestAttribute};
use quote::{quote, TokenStreamExt, ToTokens};

mod parse;
mod error;


trait Tokenize {
    fn into_tokens(self) -> TokenStream;
}

impl<T: ToTokens> Tokenize for T {
    fn into_tokens(self) -> TokenStream {
        quote! { #self }
    }
}

fn default_fixture_resolve(ident: &Ident) -> parse::CaseArg {
    syn::parse2(
        quote! {#ident::default()}
    ).unwrap()
}

fn fn_arg_ident(arg: &FnArg) -> Option<&Ident> {
    match arg {
        FnArg::Captured(ArgCaptured { pat: Pat::Ident(ident), .. }) => Some(&ident.ident),
        _ => None
    }
}

fn arg_2_fixture(ident: &Ident, resolver: &Resolver) -> TokenStream {
    let fixture = resolver
        .resolve(ident)
        .map(|e| e.clone())
        .unwrap_or_else(|| default_fixture_resolve(ident));
    quote! {
        let #ident = #fixture;
    }
}

fn arg_2_fixture_dump(ident: &Ident, modifiers: &RsTestModifiers) -> Option<Stmt> {
    if modifiers.trace_me(ident) {
        syn::parse2(quote! {
            println!("{} = {:?}", stringify!(#ident), #ident);
        }).ok()
    } else {
        None
    }
}

#[derive(Default)]
/// `Resolver` can `resolve` an ident to a `CaseArg`. Pass it to `render_fn_test`
/// function to inject the case arguments resolution.
struct Resolver<'a> (std::collections::HashMap<String, &'a parse::CaseArg>);

impl<'a> Resolver<'a> {
    fn new(args: &Vec<Ident>, case: &'a parse::TestCase) -> Self {
        Resolver(
            args.iter()
                .zip(case.args.iter())
                .map(|(ref name, case_arg)| (name.to_string(), case_arg))
                .collect()
        )
    }

    fn resolve(&self, ident: &Ident) -> Option<&parse::CaseArg> {
        self.0.get(&ident.to_string()).map(|&a| a)
    }
}

fn fn_args(item_fn: &ItemFn) -> impl Iterator<Item=&FnArg> {
    item_fn.decl.inputs.iter()
}

macro_rules! wrap_modifiers {
    ($ident:ident) => {
        #[derive(Default)]
        struct $ident {
            inner: Modifiers,
        }

        impl From<Modifiers> for $ident {
            fn from(inner: Modifiers) -> Self {
                $ident { inner }
            }
        }

        impl $ident {
            fn iter(&self) -> impl Iterator<Item=&RsTestAttribute> {
                self.inner.modifiers.iter()
            }
        }
    };
}

wrap_modifiers!(RsTestModifiers);

impl RsTestModifiers {
    const TRACE_VARIABLE_ATTR: &'static str = "trace";
    const NOTRACE_VARIABLE_ATTR: &'static str = "notrace";

    fn trace_me(&self, ident: &Ident) -> bool {
        if self.should_trace() {
            self.iter()
                .filter(|&m|
                    Self::is_notrace(ident, m)
                ).next().is_none()
        } else { false }
    }

    fn is_notrace(ident: &Ident, m: &RsTestAttribute) -> bool {
        match m {
            RsTestAttribute::Tagged(i, args) if i == Self::NOTRACE_VARIABLE_ATTR =>
                args.iter().find(|&a| a == ident).is_some(),
            _ => false
        }
    }

    fn should_trace(&self) -> bool {
        self.iter()
            .filter(|&m|
                Self::is_trace(m)
            ).next().is_some()
    }

    fn is_trace(m: &RsTestAttribute) -> bool {
        match m {
            RsTestAttribute::Attr(i) if i == Self::TRACE_VARIABLE_ATTR => true,
            _ => false
        }
    }
}

wrap_modifiers!(FixtureModifiers);

impl FixtureModifiers {
    const DEFAULT_RET_ATTR: &'static str = "default";

    fn extract_default_type(self) -> Option<syn::ReturnType> {
        self.iter()
            .filter_map(|m|
                match m {
                    RsTestAttribute::Type(name, t) if name == Self::DEFAULT_RET_ATTR =>
                        Some(syn::parse2(quote!( -> #t)).unwrap()),
                    _ => None
                })
            .next()
    }
}

trait Iterable<I, IT: Iterator<Item=I>, OUT: Iterator<Item=I>> {
    fn iterable(self) -> Option<OUT>;
}

impl<I, IT: Iterator<Item=I>> Iterable<I, IT, std::iter::Peekable<IT>> for IT {
    fn iterable(self) -> Option<std::iter::Peekable<IT>> {
        let mut peekable = self.peekable();
        if peekable.peek().is_some() {
            Some(peekable)
        } else {
            None
        }
    }
}

fn trace_arguments(args: &Vec<Ident>, modifiers: &RsTestModifiers) -> Option<proc_macro2::TokenStream> {
    args.iter()
        .filter_map(move |arg| arg_2_fixture_dump(arg, modifiers))
        .iterable()
        .map(
            |it|
                quote! {
                    println!("{:-^40}", " TEST ARGUMENTS ");
                    #(#it)*
                }
        )
}

trait ArgsResolver {
    fn resolve_args(&self, resolver: &Resolver) -> TokenStream;
}

impl ArgsResolver for ItemFn {
    fn resolve_args(&self, resolver: &Resolver) -> TokenStream {
        let define_vars = fn_args(self)
            .filter_map(fn_arg_ident)
            .map(|arg|
                arg_2_fixture(arg, resolver)
            );
        quote! {
            #(#define_vars)*
        }
    }
}

fn render_fn_test<'a>(name: Ident, testfn: &ItemFn,
                      resolver: &'a Resolver, modifiers: &'a RsTestModifiers, inline_impl: bool)
                      -> TokenStream {
    let testfn_name = &testfn.ident;
    let args = fn_args_idents(&testfn);
    let attrs = &testfn.attrs;
    let output = &testfn.decl.output;
    let test_impl = if inline_impl { Some(testfn) } else { None };
    let inject = testfn.resolve_args(resolver);
    let trace_args = trace_arguments(&args, modifiers);
    quote! {
        #[test]
        #(#attrs)*
        fn #name() #output {
            #test_impl
            #(#inject)*
            #trace_args
            println!("{:-^40}", " TEST START ");
            #testfn_name(#(#args),*)
        }
    }
}


fn type_ident(t: &syn::Type) -> Option<&syn::Ident> {
    match t {
        syn::Type::Path(tp) if tp.qself.is_none() && tp.path.segments.len() == 1 => {
            tp.path.segments.first()
                .map(|pair| &pair.value().ident)
        }
        _ => None
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

fn generics_clean_up(original: Generics, output: &ReturnType) -> syn::Generics {
    use syn::visit::Visit;
    #[derive(Default, Debug)]
    struct Used(std::collections::HashSet<proc_macro2::Ident>);
    impl<'ast> syn::visit::Visit<'ast> for Used {
        fn visit_type_path(&mut self, i: &'ast syn::TypePath) {
            if i.qself.is_none() && i.path.leading_colon.is_none() && i.path.segments.len() == 1 {
                self.0.insert(i.path.segments.first().unwrap().value().ident.clone());
            }
        }
    }
    let mut outs = Used::default();
    outs.visit_return_type(&output);
    let mut result = original;
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
                .and_then(type_ident)
                .map(|t| outs.0.contains(t))
                .unwrap_or(true)
            ).collect()
    );
    result
}

fn render_fixture<'a>(fixture: ItemFn, resolver: Resolver,
                      modifiers: FixtureModifiers)
                      -> TokenStream {
    let name = &fixture.ident;
    let vargs = fn_args_idents(&fixture);
    let args = &vargs;
    let orig_args = &fixture.decl.inputs;
    let orig_attrs = &fixture.attrs;
    let generics = &fixture.decl.generics;
    let default_output = modifiers.extract_default_type().unwrap_or(fixture.decl.output.clone());
    let default_generics = generics_clean_up(fixture.decl.generics.clone(), &default_output);
    let default_where_clause = &default_generics.where_clause;
    let body = &fixture.block;
    let where_clause = &fixture.decl.generics.where_clause;
    let output = &fixture.decl.output;
    let visibility = &fixture.vis;
    let inject = fixture.resolve_args(&resolver);
    quote! {
        #[allow(non_camel_case_types)]
        #visibility struct #name {}

        impl #name {
            #(#orig_attrs)*
            pub fn get #generics (#orig_args) #output #where_clause {
                #body
            }

            pub fn default #default_generics () #default_output #default_where_clause {
                #(#inject)*
                Self::get(#(#args),*)
            }
        }

        #[allow(dead_code)]
        #fixture
    }
}

fn fn_args_idents(test: &ItemFn) -> Vec<Ident> {
    fn_args(&test)
        .filter_map(fn_arg_ident)
        .cloned()
        .collect::<Vec<_>>()
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
/// Sometimes the return type cannot be infered so you must define it: For the few times you may
/// need to do it, you can use the `default<type>` syntax to define it:
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
/// #[fixture(default<impl Iterator<Item=u32>>)]
/// pub fn fx<I>(i: I) -> impl Iterator<Item=I> {
///     std::iter::once(i)
/// }
///
/// #[rstest]
/// fn resolve<I: Debug + PartialEq>(mut fx: impl Iterator<Item=I>) {
///     assert_eq!(42, fx.next().unwrap())
/// }
/// ```
///
#[proc_macro_attribute]
pub fn fixture(args: proc_macro::TokenStream,
               input: proc_macro::TokenStream)
               -> proc_macro::TokenStream {
    let fixture = parse_macro_input!(input as ItemFn);
    let modifiers = parse_macro_input!(args as Modifiers).into();
    render_fixture(fixture, Resolver::default(), modifiers).into()
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
/// #[rstest(trace)]
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
/// trait, you can also use the `notrace(list_of_inputs)` modifier:
///
/// ```
/// # use rstest::*;
/// # struct Xyz;
/// # struct NoSense;
/// #[rstest(trace::notrace(xzy, have_no_sense))]
/// fn the_test(injected: i32, xyz: Xyz, have_no_sense: NoSense) {
///     assert_eq!(42, injected)
/// }
/// ```
#[proc_macro_attribute]
pub fn rstest(args: proc_macro::TokenStream,
              input: proc_macro::TokenStream)
              -> proc_macro::TokenStream {
    let test = parse_macro_input!(input as ItemFn);
    let modifiers = parse_macro_input!(args as Modifiers).into();
    let name = &test.ident;
    let resolver = Resolver::default();
    render_fn_test(name.clone(), &test, &resolver, &modifiers, true)
        .into()
}

fn fn_args_has_ident(fn_decl: &ItemFn, ident: &Ident) -> bool {
    fn_args(fn_decl)
        .filter_map(fn_arg_ident)
        .find(|&id| id == ident)
        .is_some()
}

fn errors_in_parametrize(test: &ItemFn, params: &parse::ParametrizeData) -> Option<TokenStream> {
    let invalid_args = params.args.iter()
        .filter(|&p| !fn_args_has_ident(test, p));

    let mut tokens = TokenStream::default();
    for missed in invalid_args {
        let span = missed.span().into();
        let message = format!("Missed argument: '{}' should be a test function argument.", missed);
        tokens.extend(error_statement(&message, span, span));
    }

    if !tokens.is_empty() {
        Some(tokens)
    } else {
        None
    }
}

fn add_parametrize_cases(test: ItemFn, params: parse::ParametrizeInfo) -> TokenStream {
    let fname = &test.ident;
    let parse::ParametrizeInfo { data: params, modifiers } = params;
    let modifiers = modifiers.into();

    let mut cases = TokenStream::new();
    for (n, case) in params.cases.iter().enumerate() {
        cases.append_all(
            if case.args.len() != params.args.len() {
                error_statement("Wrong case signature: should match the given parameters list.",
                                case.span_start(), case.span_end())
            } else {
                let resolver = Resolver::new(&params.args, &case);
                let name = Ident::new(&format_case_name(&params, n), fname.span());
                render_fn_test(name, &test, &resolver, &modifiers, false)
            }
        )
    };
    quote! {
        #[cfg(test)]
        #test

        #[cfg(test)]
        mod #fname {
            use super::*;

            #cases
        }
    }
}

fn format_case_name(params: &parse::ParametrizeData, index: usize) -> String {
    let len_max = format!("{}", params.cases.len()).len();
    let description = params.cases[index]
        .description.as_ref()
        .map(|d| format!("_{}", d))
        .unwrap_or_default();
    format!("case_{:0len$}{d}", index + 1, len = len_max as usize, d = description)
}

/// Write table-based tests: you must indicate the arguments tha you want use in your cases
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
/// Every parameter that isn't mapped in `case()`s will be resolved as `fixture` like
/// [`[rstest]`](attr.rstest.html)'s function arguments.
///
/// In general `rstest_parametrize`'s syntax is:
///
/// ```norun
/// rstest_parametrize(ident_1,..., ident_n,
///     case[::description_1](val_1_1, ..., val_n_1),
///     ...,
///     case[::description_m](val_1_m, ..., val_n_m)[,]
///     [::modifier_1[:: ... [::modifir_k]]]
/// )
/// ```
/// * `ident_x` should be a valid function argument name
/// * `val_x_y` should be a valid rust expression that can be assigned to `ident_x` function argument
/// * `description_l` when present should be a valid Rust identity
/// * modifiers now can be just `trace` or `notrace(args..)` (see [`[rstest]`](attr.rstest.html)
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
    let params = parse_macro_input!(args as parse::ParametrizeInfo);
    let test = parse_macro_input!(input as ItemFn);

    if let Some(tokens) = errors_in_parametrize(&test, &params.data) {
        tokens
    } else {
        add_parametrize_cases(test, params)
    }.into()
}

#[proc_macro_attribute]
pub fn rstest_matrix(args: proc_macro::TokenStream, input: proc_macro::TokenStream)
                     -> proc_macro::TokenStream
{
    let info = parse_macro_input!(args as parse::MatrixInfo);
    let test = parse_macro_input!(input as ItemFn);

    add_parametrize_cases(test, info.into()).into()
}

#[cfg(test)]
mod render {
    use pretty_assertions::assert_eq;
    use syn::{Expr, ItemFn, parse_str, punctuated};
    use syn::export::Debug;
    use syn::parse2;

    use crate::parse::*;

    use super::*;

    fn fn_args(item: &ItemFn) -> punctuated::Iter<'_, FnArg> {
        item.decl.inputs.iter()
    }

    fn first_arg_ident(ast: &ItemFn) -> &Ident {
        let arg = fn_args(&ast).next().unwrap();
        fn_arg_ident(arg).unwrap()
    }

    fn assert_syn_eq<P, S>(expected: S, ast: P) where
        S: AsRef<str>,
        P: syn::parse::Parse + Debug + Eq
    {
        assert_eq!(
            parse_str::<P>(expected.as_ref()).unwrap(),
            ast
        )
    }

    fn assert_statement_eq<T, S>(expected: S, tokens: T) where
        T: Into<TokenStream>,
        S: AsRef<str>
    {
        assert_syn_eq::<Stmt, _>(expected, parse2::<Stmt>(tokens.into()).unwrap())
    }

    #[test]
    fn extract_fixture_call_arg() {
        let ast = parse_str("fn foo(mut fix: String) {}").unwrap();
        let arg = first_arg_ident(&ast);
        let resolver = Resolver::default();

        let line = arg_2_fixture(arg, &resolver);

        assert_statement_eq("let fix = fix::default();", line);
    }

    #[test]
    fn extract_fixture_should_not_add_mut() {
        let ast = parse_str("fn foo(mut fix: String) {}").unwrap();
        let arg = first_arg_ident(&ast);
        let resolver = Resolver::default();

        let line = arg_2_fixture(arg, &resolver);

        assert_statement_eq("let fix = fix::default();", line);
    }

    fn case_arg<S: AsRef<str>>(s: S) -> CaseArg {
        parse_str::<Expr>(s.as_ref()).unwrap().into()
    }

    #[test]
    fn arg_2_fixture_str_should_use_passed_fixture_if_any() {
        let ast = parse_str("fn foo(mut fix: String) {}").unwrap();
        let arg = first_arg_ident(&ast);
        let call = case_arg("bar()");
        let mut resolver = Resolver::default();
        resolver.add("fix", &call);

        let line = arg_2_fixture(arg, &resolver);

        assert_statement_eq("let fix = bar();", line);
    }

    impl<'a> Resolver<'a> {
        fn add<S: AsRef<str>>(&mut self, ident: S, expr: &'a CaseArg) {
            self.0.insert(ident.as_ref().to_string(), expr);
        }
    }

    #[test]
    fn resolver_should_return_the_given_expression() {
        let ast = parse_str("fn function(mut foo: String) {}").unwrap();
        let arg = first_arg_ident(&ast);
        let expected = case_arg("bar()");
        let mut resolver = Resolver::default();

        resolver.add("foo", &expected);

        assert_eq!(&expected, resolver.resolve(&arg).unwrap())
    }

    #[test]
    fn resolver_should_return_none_for_unknown_argument() {
        let ast = parse_str("fn function(mut fix: String) {}").unwrap();
        let arg = first_arg_ident(&ast);
        let resolver = Resolver::default();

        assert!(resolver.resolve(&arg).is_none())
    }

    mod render_fn_test_should {
        use pretty_assertions::assert_eq;
        use proc_macro2::Span;

        use super::*;

        #[test]
        fn add_return_type_if_any() {
            let ast: ItemFn = parse_str("fn function(mut fix: String) -> Result<i32, String> { Ok(42) }").unwrap();

            let tokens = render_fn_test(Ident::new("new_name", Span::call_site()),
                                        &ast, &Default::default(), &Default::default(), false);

            let result: ItemFn = parse2(tokens).unwrap();

            assert_eq!(result.ident.to_string(), "new_name");
            assert_eq!(result.decl.output, ast.decl.output);
        }

        #[test]
        fn should_include_given_function() {
            let input_fn: ItemFn = parse_str(
                r#"
                pub fn test<R: AsRef<str>, B>(mut s: String, v: &u32, a: &mut [i32], r: R) -> (u32, B, String, &str)
                        where B: Borrow<u32>
                {
                    let some = 42;
                    assert_eq!(42, some);
                }
                "#
            ).unwrap();

            let tokens = render_fn_test(Ident::new("new_name", Span::call_site()),
                                        &input_fn, &Default::default(), &Default::default(), true);

            let result: ItemFn = parse2(tokens).unwrap();

            let inner_fn: ItemFn = parse2(result.block.stmts.get(0).into_tokens()).unwrap();

            assert_eq!(inner_fn, inner_fn);
        }
    }

    mod add_parametrize_cases {
        use std::borrow::Cow;

        use syn::{ItemFn, ItemMod, parse::{Parse, ParseStream, Result}};
        use syn::visit::Visit;

        use super::{*, assert_eq};

        struct ParametrizeOutput {
            orig: ItemFn,
            module: ItemMod,
        }

        impl Parse for ParametrizeOutput {
            fn parse(input: ParseStream) -> Result<Self> {
                Ok(Self {
                    orig: input.parse()?,
                    module: input.parse()?,
                })
            }
        }

        impl ParametrizeOutput {
            pub fn get_test_functions(&self) -> Vec<ItemFn> {
                let mut f = TestFunctions(vec![]);

                f.visit_item_mod(&self.module);
                f.0
            }
        }

        impl From<TokenStream> for ParametrizeOutput {
            fn from(tokens: TokenStream) -> Self {
                syn::parse2::<ParametrizeOutput>(tokens).unwrap()
            }
        }

        impl<'a> From<&'a ItemFn> for parse::ParametrizeData {
            fn from(item_fn: &'a ItemFn) -> Self {
                parse::ParametrizeData {
                    args: fn_args_idents(item_fn),
                    cases: vec![],
                }
            }
        }

        impl<'a> From<&'a ItemFn> for parse::ParametrizeInfo {
            fn from(item_fn: &'a ItemFn) -> Self {
                parse::ParametrizeInfo {
                    data: item_fn.into(),
                    modifiers: Default::default(),
                }
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
            fn visit_item_fn(&mut self, item_fn: &'ast ItemFn) {
                if Self::is_test_fn(item_fn) {
                    self.0.push(item_fn.clone())
                }
            }
        }


        #[test]
        fn should_create_a_module_named_as_test_function() {
            let item_fn = parse_str::<ItemFn>("fn should_be_the_module_name(mut fix: String) {}").unwrap();
            let info = (&item_fn).into();
            let tokens = add_parametrize_cases(item_fn.clone(), info);

            let output = ParametrizeOutput::from(tokens);

            assert_eq!(output.module.ident, "should_be_the_module_name");
        }

        #[test]
        fn should_copy_user_function() {
            let item_fn = parse_str::<ItemFn>(
                r#"fn should_be_the_module_name(mut fix: String) { println!("user code") }"#
            ).unwrap();
            let info = (&item_fn).into();
            let tokens = add_parametrize_cases(item_fn.clone(), info);

            let mut output = ParametrizeOutput::from(tokens);

            output.orig.attrs = vec![];
            assert_eq!(output.orig, item_fn);
        }

        #[test]
        fn should_mark_user_function_as_test() {
            let item_fn = parse_str::<ItemFn>(
                r#"fn should_be_the_module_name(mut fix: String) { println!("user code") }"#
            ).unwrap();
            let info = (&item_fn).into();
            let tokens = add_parametrize_cases(item_fn.clone(), info);

            let output = ParametrizeOutput::from(tokens);

            let expected = parse2::<ItemFn>(quote! {
                #[cfg(test)]
                fn some() {}
            }).unwrap().attrs;

            assert_eq!(expected, output.orig.attrs);
        }

        #[test]
        fn should_mark_module_as_test() {
            let item_fn = parse_str::<ItemFn>(
                r#"fn should_be_the_module_name(mut fix: String) { println!("user code") }"#
            ).unwrap();
            let info = (&item_fn).into();
            let tokens = add_parametrize_cases(item_fn.clone(), info);

            let output = ParametrizeOutput::from(tokens);

            let expected = parse2::<ItemMod>(quote! {
                #[cfg(test)]
                mod some {}
            }).unwrap().attrs;

            assert_eq!(expected, output.module.attrs);
        }

        impl ParametrizeInfo {
            fn push_case(&mut self, case: TestCase) {
                self.data.cases.push(case);
            }

            fn extend(&mut self, cases: impl Iterator<Item=TestCase>) {
                self.data.cases.extend(cases);
            }
        }

        impl<'a> From<Vec<Cow<'a, str>>> for TestCase {
            fn from(arguments: Vec<Cow<'a, str>>) -> Self {
                TestCase {
                    args: arguments
                        .iter().map(|a| CaseArg::new(parse_str(a.as_ref().into()).unwrap())).collect(),
                    description: None,
                }
            }
        }

        impl<'a> From<Cow<'a, str>> for TestCase {
            fn from(argument: Cow<'a, str>) -> Self {
                vec![argument].into()
            }
        }

        impl<'a> From<&'a str> for TestCase {
            fn from(argument: &'a str) -> Self {
                argument.split(",\n")
                    .map(|s| Cow::from(s))
                    .collect::<Vec<_>>().into()
            }
        }

        fn one_simple_case() -> (ItemFn, ParametrizeInfo) {
            let item_fn = parse_str::<ItemFn>(
                r#"fn test(mut fix: String) { println!("user code") }"#
            ).unwrap();
            let mut info: ParametrizeInfo = (&item_fn).into();
            info.push_case(TestCase::from(r#"String::from("3")"#));
            (item_fn, info)
        }

        fn some_simple_cases(cases: i32) -> (ItemFn, ParametrizeInfo) {
            let item_fn = parse_str::<ItemFn>(
                r#"fn test(mut fix: String) { println!("user code") }"#
            ).unwrap();
            let mut info: ParametrizeInfo = (&item_fn).into();
            info.extend((0..cases).map(|_| TestCase::from(r#"String::from("3")"#)));
            (item_fn, info)
        }

        #[test]
        fn should_add_a_test_case() {
            let (item_fn, info) = one_simple_case();

            let tokens = add_parametrize_cases(item_fn.clone(), info);

            let tests = ParametrizeOutput::from(tokens).get_test_functions();

            assert_eq!(1, tests.len());
            assert!(&tests[0].ident.to_string().starts_with("case_"))
        }

        #[test]
        fn case_number_should_starts_from_1() {
            let (item_fn, info) = one_simple_case();

            let tokens = add_parametrize_cases(item_fn.clone(), info);

            let tests = ParametrizeOutput::from(tokens).get_test_functions();

            assert!(&tests[0].ident.to_string().starts_with("case_1"))
        }

        #[test]
        fn should_add_all_test_cases() {
            let (item_fn, info) = some_simple_cases(5);

            let tokens = add_parametrize_cases(item_fn.clone(), info);

            let tests = ParametrizeOutput::from(tokens).get_test_functions();

            let valid_names = tests.iter()
                .filter(|it| it.ident.to_string().starts_with("case_"));
            assert_eq!(5, valid_names.count())
        }

        #[test]
        fn should_left_pad_case_number_by_zeros() {
            let (item_fn, info) = some_simple_cases(1000);

            let tokens = add_parametrize_cases(item_fn.clone(), info);

            let tests = ParametrizeOutput::from(tokens).get_test_functions();

            let first_name = tests[0].ident.to_string();
            let last_name = tests[999].ident.to_string();

            assert!(first_name.ends_with("_0001"));
            assert!(last_name.ends_with("_1000"));

            let valid_names = tests.iter()
                .filter(|it| it.ident.to_string().len() == first_name.len());
            assert_eq!(1000, valid_names.count())
        }

        #[test]
        fn should_use_description_if_any() {
            let (item_fn, mut info) = one_simple_case();
            let description = "show_this_description";
            info.data.cases[0].description = Some(parse_str::<Ident>(description).unwrap());

            let tokens = add_parametrize_cases(item_fn.clone(), info);

            let tests = ParametrizeOutput::from(tokens).get_test_functions();

            assert!(tests[0].ident.to_string().ends_with(&format!("_{}", description)));
        }
    }

    mod fixture {
        use syn::{ItemFn, ItemImpl, ItemStruct, parse2, parse_str};
        use syn::parse::{Parse, ParseBuffer, Result};

        use crate::{generics_clean_up, render_fixture};
        use crate::parse::{Modifiers, RsTestAttribute};

        use super::assert_eq;

        struct FixtureOutput {
            orig: ItemFn,
            fixture: ItemStruct,
            core_impl: ItemImpl,
        }

        impl Parse for FixtureOutput {
            fn parse(input: &ParseBuffer) -> Result<Self> {
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

            let tokens = render_fixture(item_fn.clone(),
                                        Default::default(), Default::default());
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
        fn fixture_impl_should_implement_a_get_method_with_input_fixture_signature() {
            let (item_fn, out) = parse_fixture(
                r#"
                pub fn test<R: AsRef<str>, B>(mut s: String, v: &u32, a: &mut [i32], r: R) -> (u32, B, String, &str)
                        where B: Borrow<u32>
                { }
                "#);


            let get_decl = select_method(out.core_impl, "get")
                .unwrap()
                .sig
                .decl;

            assert_eq!(*item_fn.decl, get_decl);
        }

        #[test]
        fn fixture_impl_should_implement_a_default_method_with_input_cleaned_fixture_signature_and_no_args() {
            let (item_fn, out) = parse_fixture(
                r#"
                pub fn test<R: AsRef<str>, B, F, H: Iterator<Item=u32>>(mut s: String, v: &u32, a: &mut [i32], r: R) -> (H, B, String, &str)
                        where F: ToString,
                        B: Borrow<u32>

                { }
                "#);

            let default_decl = select_method(out.core_impl, "default")
                .unwrap()
                .sig
                .decl;

            let expected = parse_str::<ItemFn>(
                r#"
                pub fn default<B, H: Iterator<Item=u32>>() -> (H, B, String, &str)
                        where B: Borrow<u32>
                { }
                "#
            ).unwrap();


            assert_eq!(expected.decl.generics, default_decl.generics);
            assert_eq!(item_fn.decl.output, default_decl.output);
            assert!(default_decl.inputs.is_empty());
        }

        #[test]
        fn clean_up_default_generics_no_output() {
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

            let cleaned = generics_clean_up(item_fn.decl.generics, &item_fn.decl.output);

            assert_eq!(expected.decl.generics, cleaned);
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
                                        Default::default(),
                                        Modifiers {
                                            modifiers: vec![
                                                RsTestAttribute::Type(
                                                    parse_str("default").unwrap(),
                                                    parse_str("(impl Iterator<Item=u32>, B)").unwrap(),
                                                )
                                            ]
                                        }.into());
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
                .sig
                .decl;

            assert_eq!(*expected.decl, default_decl);
        }
    }
}

