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
//! - [`[rstest]`](attr.rstest.html): Declare that a test or a group of tests that may take fixtures,
//! input table or list of values.
//! - [`[fixture]`](attr.fixture.html): To mark a function as a fixture.
//! - ~[`[rstest_parametrize]`](attr.rstest_parametrize.html): Like `[rstest]` above but with the
//! added ability to also generate new test cases based on input tables~. Now the `rstest`'s syntax
//! include these features too.
//! - ~[`[rstest_matrix]`](attr.rstest_matrix.html): Like `[rstest]` above but with the
//! added ability to also generate new test cases for every combination of given values~. Now the
//! `rstest`'s syntax include these features too.
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
//! You can use also [`[rstest]`](attr.rstest.html) to create simple table-based tests. Let's see
//! the classic Fibonacci exmple:
//!
//! ```
//! use rstest::rstest;
//!
//! #[rstest(input, expected,
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
//!
//! ## Creating a test for each combinations of given values
//!
//! In some cases you need to test your code for each cominations of some input values. In this
//! cases [`[rstest]`](attr.rstest.html) give you the ability to define a list
//! of values (rust expressions) to use for an arguments.
//!
//! ```
//! # use rstest::rstest;
//! # #[derive(PartialEq, Debug)]
//! # enum State { Init, Start, Processing, Terminated }
//! # #[derive(PartialEq, Debug)]
//! # enum Event { Error, Fatal }
//! # impl State { fn process(self, event: Event) -> Self { self } }
//!
//! #[rstest(
//!     state => [State::Iint, State::Start, State::Processing],
//!     event => [Event::Error, Event::Fatal]
//! )]
//! fn should_terminate(state: State, event: Event) {
//!     assert_eq!(State::Terminated, state.process(event))
//! }
//! ```
//!
//! This will generate a test for each combination of `state` and `event`.
//!
//! ## Putting all togheder
//!
//! All these features can be used togheder: take some fixtures, define some fixed cases and, for
//! each case, tests all combinations of gine values.
//!


#![cfg_attr(use_proc_macro_diagnostic, feature(proc_macro_diagnostic))]
extern crate proc_macro;

use std::collections::HashMap;

use proc_macro2::TokenStream;
use syn::{FnArg, Ident, ItemFn, parse_macro_input};
use syn::spanned::Spanned;

use crate::parse::{
    fixture::FixtureInfo,
    rstest::{RsTestData, RsTestInfo},
};
use crate::render::{
    render_fixture, render_single_case, render_matrix_cases, render_parametrize_cases
};
use crate::refident::MaybeIdent;

// Test utility module
#[cfg(test)]
#[macro_use]
pub(crate) mod test;

mod parse;
mod render;
mod resolver;
mod refident;

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
        } else if info.data.has_cases() {
            render_parametrize_cases(test, info)
        } else {
            render_single_case(test, info)
        }
    } else {
        errors
    }.into()
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

// TODO: move in utils module (with other stuffs)
fn fn_args(item_fn: &ItemFn) -> impl Iterator<Item=&FnArg> {
    item_fn.sig.inputs.iter()
}

fn fn_args_has_ident(fn_decl: &ItemFn, ident: &Ident) -> bool {
    fn_args(fn_decl)
        .filter_map(MaybeIdent::maybe_ident)
        .find(|&id| id == ident)
        .is_some()
}


//TODO: Move to error module
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

fn errors_in_rstest(test: &ItemFn, info: &parse::rstest::RsTestInfo) -> TokenStream {
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
