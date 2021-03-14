//! # Reuse `rstest`'s parametrized cases
//!
//! This crate give a way to define a tests set and apply them to every case you need to
//! test.
//!
//! With `rstest` crate you can define a tests list but if you want to apply the same tests
//! to another test function you must rewrite all cases or write some macros that do the job.
//! Both solutions have some drawbreak:
//!
//! - rewrite create duplication
//! - macros makes code harder to read and shift out the focus from tests core
//!
//! The aim of this crate is solve this problem. `rstest_resuse` expose two attributes:
//!
//! - `#[template]`: to define a template
//! - `#[apply]`: to apply a defined template to create tests
//!
//! Here is a simple example:
//!
//! ```
//! use rstest::rstest;
//! use rstest_reuse::{self, *};
//!
//! // Here we define the template. This define
//! // * The test list name to `two_simple_cases`
//! // * cases: here two cases that feed the `a`, `b` values
//! #[template]
//! #[rstest(a,  b,
//!     case(2, 2),
//!     case(4/2, 2),
//!     )
//! ]
//! fn two_simple_cases(a: u32, b: u32) {}
//!
//! // Here we apply the `two_simple_cases` template: That is expanded in
//! // #[rstest(a,  b,
//! //     case(2, 2),
//! //     case(4/2, 2),
//! //     )
//! // ]
//! // fn it_works(a: u32, b: u32) {
//! //     assert!(a == b);
//! // }
//! #[apply(two_simple_cases)]
//! fn it_works(a: u32, b: u32) {
//!     assert!(a == b);
//! }
//!
//!
//! // Here we reuse the `two_simple_cases` template to create two
//! // other tests
//! #[apply(two_simple_cases)]
//! fn it_fail(a: u32, b: u32) {
//!     assert!(a != b);
//! }
//! ```
//! If we run `cargo test` we have:
//!
//! ```text
//!     Finished test [unoptimized + debuginfo] target(s) in 0.05s
//!      Running target/debug/deps/playground-8a1212f8b5eb00ce
//!
//! running 4 tests
//! test it_fail::case_1 ... FAILED
//! test it_works::case_1 ... ok
//! test it_works::case_2 ... ok
//! test it_fail::case_2 ... FAILED
//!
//! failures:
//!
//! ---- it_fail::case_1 stdout ----
//! -------------- TEST START --------------
//! thread 'it_fail::case_1' panicked at 'assertion failed: a != b', src/main.rs:34:5
//! note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace
//!
//! ---- it_fail::case_2 stdout ----
//! -------------- TEST START --------------
//! thread 'it_fail::case_2' panicked at 'assertion failed: a != b', src/main.rs:34:5
//!
//!
//! failures:
//!     it_fail::case_1
//!     it_fail::case_2
//!
//! test result: FAILED. 2 passed; 2 failed; 0 ignored; 0 measured; 0 filtered out
//!
//! error: test failed, to rerun pass '--bin playground'
//! ```
//!
//! Simple and neat!
//!
//! ## Cavelets
//!
//! ### `use rstest_resuse` at the top of your crate
//!
//! You **should** add `use rstest_resuse` at the top of your crate:
//!
//! ```
//! #[cfg(test)]
//! use rstest_reuse;
//! ```
//!
//! This is due `rstest_reuse::template` define a macro that need to call a `rstest_resuse`'s macro.
//! I hope to remove this in the future but for now we should live with it.
//!
//! Note that
//! ```
//! use rstest_reuse::*;
//! ```
//! is not enougth: this statment doesn't include `rstest_reuse` but just its public items.
//!
//!
//! ### Define `template` before `apply` it
//!
//! `template` attribute define a macro that `apply` will use. Macro in rust are expanded in
//! a single depth-first, lexical-order traversal of a crate’s source, that means the template
//! definition should be allways before the `apply`.
//!
//! ### Tag modules with `#[macro_use]`
//!
//! If you define a `template` in a module and you want to use it outside the module you should
//! _lift_ it by mark the module with the `#[macro_use]` attribute. This attribute make your
//! `template` visibe outside this module but not at the upper level. When a `template` is
//! defined you can use it in all submodules that follow the definition.
//!
//! If you plan to spread your templates in some modules and you use a unique name for each template
//! consider to add the global attribute `!#[macro_use]` at crate level: this put all your templates
//! available everywhere: you should
//! just take care that a `template` should be defined before the `apply` call.
//!
//!
//! ## Disclamer
//!
//! This crate is in developer stage. I don't know if I'll include it in `rstest` or changing some syntax in
//! the future.
//!
//! I did't test it in a lot of cases: if you have some cases where it doesn't works file a ticket on
//! [`rstest`](https://github.com/la10736/rstest)

extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::{self, parse, parse::Parse, parse_macro_input, Ident, ItemFn, Token};

struct MergeAttrs {
    template: ItemFn,
    function: ItemFn,
}

impl Parse for MergeAttrs {
    fn parse(input: parse::ParseStream) -> syn::Result<Self> {
        let template = input.parse()?;
        let _comma: Token![,] = input.parse()?;
        let function = input.parse()?;
        Ok(Self { template, function })
    }
}

#[cfg(sanitize_multiple_should_panic_compiler_bug)]
fn is_should_panic(attr: &syn::Attribute) -> bool {
    let should_panic: Ident = syn::parse_str("should_panic").unwrap();
    attr.path.is_ident(&should_panic)
}

#[cfg(sanitize_multiple_should_panic_compiler_bug)]
fn sanitize_should_panic_duplication_bug(
    mut attributes: Vec<syn::Attribute>,
) -> Vec<syn::Attribute> {
    if attributes.len() != 2 || attributes[0] != attributes[1] || !is_should_panic(&attributes[0]) {
        // Nothing to do
        return attributes;
    }
    attributes.pop();
    attributes
}

#[doc(hidden)]
#[proc_macro]
pub fn merge_attrs(item: TokenStream) -> TokenStream {
    let MergeAttrs {
        template,
        mut function,
    } = parse_macro_input!(item as MergeAttrs);
    let mut attrs = template.attrs;

    #[cfg(sanitize_multiple_should_panic_compiler_bug)]
    {
        function.attrs = sanitize_should_panic_duplication_bug(function.attrs);
    }
    attrs.append(&mut function.attrs);
    function.attrs = attrs;

    let tokens = quote! {
        #function
    };
    tokens.into()
}

/// Define a template where the name is given from the function name. This attribute register all
/// attributes. The function signature don't really mater but to make it clear is better that you
/// use a signature like if you're wrinting a standard `rstest`.
#[proc_macro_attribute]
pub fn template(_args: proc_macro::TokenStream, input: proc_macro::TokenStream) -> TokenStream {
    let template: ItemFn = parse(input).unwrap();
    let macro_name = template.sig.ident.clone();
    let tokens = quote! {
        #[macro_export]
        /// Apply #macro_name telmplate to given body
        macro_rules! #macro_name {
            ( $test:item ) => {
                        $crate::rstest_reuse::merge_attrs! {
                            #template,
                            $test
                        }
                    }
        }
    };
    tokens.into()
}

/// Apply a defined template. The function signature should satisfy the template attributes
/// but can also add some other fixtures.
/// Example:
///
/// ```
/// use rstest::{rstest, fixture};
/// use rstest_reuse::{self, *};
///
/// #[fixture]
/// fn empty () -> Vec<u32> {
///     Vec::new()    
/// }
///
/// #[template]
/// #[rstest(a,  b,
///     case(2, 2),
///     case(4/2, 2),
///     )
/// ]
/// fn two_simple_cases(a: u32, b: u32) {}
///
/// #[apply(two_simple_cases)]
/// fn it_works(mut empty: Vec<u32>, a: u32, b: u32) {
///     empty.append(a);
///     assert!(empty.last() == b);
/// }
/// ```

#[proc_macro_attribute]
pub fn apply(args: proc_macro::TokenStream, input: proc_macro::TokenStream) -> TokenStream {
    let template: Ident = parse(args).unwrap();
    let test: ItemFn = parse(input).unwrap();
    let tokens = quote! {
        #template! {
            #test
        }
    };
    tokens.into()
}
