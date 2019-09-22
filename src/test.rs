pub use pretty_assertions::assert_eq;
use proc_macro2::TokenTree;
use syn::{ItemFn, parse2, parse_str};

use quote::quote;

use super::*;
use crate::parse::fixture::{FixtureItem, FixtureData};
use crate::parse::rstest::{RsTestItem, RsTestData};
use crate::parse::{Fixture, CaseArg, ValueList, Attribute};

macro_rules! to_args {
    ($e:expr) => {
                   {
                   use itertools::Itertools;
                   $e.iter()
                   .map(|s| s as & dyn AsRef<str>)
                   .map(case_arg)
                   .collect_vec()
                   }
                 };
}

macro_rules! to_exprs {
    ($e:expr) => {$e.iter().map(|s| expr(s)).collect::<Vec<_>>()};
}

macro_rules! to_strs {
    ($e:expr) => {$e.iter().map(ToString::to_string).collect::<Vec<_>>()};
}

pub(crate) fn parse_meta<T: syn::parse::Parse, S: AsRef<str>>(test_case: S) -> T {
    let to_parse = format!(r#"
        #[{}]
        fn to_parse() {{}}
        "#, test_case.as_ref());

    let item_fn = parse_str::<ItemFn>(&to_parse).unwrap();

    let tokens = quote!(
            #item_fn
        );

    let tt = tokens.into_iter().skip(1).next().unwrap();

    if let TokenTree::Group(g) = tt {
        let ts = g.stream();
        parse2::<T>(ts).unwrap()
    } else {
        panic!("Cannot find group in {:#?}", tt)
    }
}

pub(crate) fn ident<S: AsRef<str>>(s: S) -> syn::Ident {
    parse_str::<syn::Ident>(s.as_ref()).unwrap()
}

pub(crate) fn expr<S: AsRef<str>>(s: S) -> syn::Expr {
    parse_str::<syn::Expr>(s.as_ref()).unwrap()
}

pub fn fixture(name: impl AsRef<str>, args: Vec<&str>) -> Fixture {
    Fixture::new(ident(name), to_exprs!(args))
}

pub(crate) fn case_arg<S: AsRef<str>>(s: S) -> CaseArg {
    CaseArg::new(expr(s))
}

pub(crate) fn values_list<S: AsRef<str>>(arg: &str, values: &[S]) -> ValueList {
    ValueList {
        arg: ident(arg),
        values: values.into_iter().map(|s| case_arg(s)).collect(),
    }
}

pub(crate) fn literal_expressions_str() -> Vec<&'static str> {
    vec!["42", "42isize", "1.0", "-1", "-1.0", "true", "1_000_000u64", "0b10100101u8",
         r#""42""#, "b'H'"]
}

pub(crate) trait ExtractArgs {
    fn args(&self) -> Vec<CaseArg>;
}

impl ExtractArgs for TestCase {
    fn args(&self) -> Vec<CaseArg> {
        self.args.iter().cloned().collect()
    }
}

impl ExtractArgs for ValueList {
    fn args(&self) -> Vec<CaseArg> {
        self.values.iter().cloned().collect()
    }
}

impl Attribute {
    pub fn attr<S: AsRef<str>>(s: S) -> Self {
        Attribute::Attr(ident(s))
    }

    pub fn tagged<SI: AsRef<str>, SA: AsRef<str>>(tag: SI, attrs: Vec<SA>) -> Self {
        Attribute::Tagged(
            ident(tag),
            attrs.into_iter()
                .map(|a| ident(a))
                .collect(),
        )
    }

    pub fn typed<S: AsRef<str>, T: AsRef<str>>(tag: S, inner: T) -> Self {
        Attribute::Type(
            ident(tag),
            parse_str(inner.as_ref()).unwrap(),
        )
    }
}

impl From<Vec<RsTestItem>> for RsTestData {
    fn from(fixtures: Vec<RsTestItem>) -> Self {
        Self { items: fixtures }
    }
}

impl From<Vec<FixtureItem>> for FixtureData {
    fn from(fixtures: Vec<FixtureItem>) -> Self {
        Self { items: fixtures }
    }
}
