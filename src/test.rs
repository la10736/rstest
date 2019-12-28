#![macro_use]

/// Unit testing utility module. Collect a bunch of functions&macro and impls to simplify unit
/// testing bolilerplate.
///
use std::borrow::Cow;
use std::iter::FromIterator;

pub(crate) use pretty_assertions::assert_eq;
use proc_macro2::TokenTree;
use quote::quote;
use syn::{parse::Parse, parse2, parse_str, Expr, Ident, ItemFn};

use super::*;
use crate::parse::{
    fixture::{FixtureData, FixtureItem},
    rstest::{RsTestData, RsTestItem},
    testcase::TestCase,
    vlist::ValueList,
    Attribute, Fixture,
};
use crate::resolver::Resolver;

macro_rules! to_args {
    ($e:expr) => {{
        $e.iter()
            .map(|s| s as &dyn AsRef<str>)
            .map(expr)
            .collect::<Vec<_>>()
    }};
}

macro_rules! to_exprs {
    ($e:expr) => {
        $e.iter().map(|s| expr(s)).collect::<Vec<_>>()
    };
}

macro_rules! to_strs {
    ($e:expr) => {
        $e.iter().map(ToString::to_string).collect::<Vec<_>>()
    };
}

pub(crate) fn parse_meta<T: syn::parse::Parse, S: AsRef<str>>(test_case: S) -> T {
    let to_parse = format!(
        r#"
        #[{}]
        fn to_parse() {{}}
        "#,
        test_case.as_ref()
    );

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

pub(crate) trait ToAst {
    fn ast<T: Parse>(&self) -> T;
}

impl<S: AsRef<str>> ToAst for S {
    fn ast<T: Parse>(&self) -> T {
        parse_str(self.as_ref()).unwrap()
    }
}

pub(crate) fn ident(s: impl AsRef<str>) -> syn::Ident {
    s.as_ref().ast()
}

pub(crate) fn expr(s: impl AsRef<str>) -> syn::Expr {
    s.as_ref().ast()
}

pub(crate) fn fixture(name: impl AsRef<str>, args: Vec<&str>) -> Fixture {
    Fixture::new(ident(name), to_exprs!(args))
}

pub(crate) fn values_list<S: AsRef<str>>(arg: &str, values: &[S]) -> ValueList {
    ValueList {
        arg: ident(arg),
        values: values.into_iter().map(|s| expr(s)).collect(),
    }
}

pub(crate) fn literal_expressions_str() -> Vec<&'static str> {
    vec![
        "42",
        "42isize",
        "1.0",
        "-1",
        "-1.0",
        "true",
        "1_000_000u64",
        "0b10100101u8",
        r#""42""#,
        "b'H'",
    ]
}

pub(crate) trait ExtractArgs {
    fn args(&self) -> Vec<Expr>;
}

impl ExtractArgs for TestCase {
    fn args(&self) -> Vec<Expr> {
        self.args.iter().cloned().collect()
    }
}

impl ExtractArgs for ValueList {
    fn args(&self) -> Vec<Expr> {
        self.values.iter().cloned().collect()
    }
}

impl Attribute {
    pub fn attr<S: AsRef<str>>(s: S) -> Self {
        Attribute::Attr(ident(s))
    }

    pub fn tagged<SI: AsRef<str>, SA: AsRef<str>>(tag: SI, attrs: Vec<SA>) -> Self {
        Attribute::Tagged(ident(tag), attrs.into_iter().map(|a| ident(a)).collect())
    }

    pub fn typed<S: AsRef<str>, T: AsRef<str>>(tag: S, inner: T) -> Self {
        Attribute::Type(ident(tag), parse_str(inner.as_ref()).unwrap())
    }
}

impl RsTestInfo {
    pub fn push_case(&mut self, case: TestCase) {
        self.data.items.push(RsTestItem::TestCase(case));
    }

    pub fn extend(&mut self, cases: impl Iterator<Item = TestCase>) {
        self.data.items.extend(cases.map(RsTestItem::TestCase));
    }
}

impl<'a> FromIterator<&'a str> for TestCase {
    fn from_iter<T: IntoIterator<Item = &'a str>>(iter: T) -> Self {
        TestCase {
            args: iter.into_iter().map(expr).collect(),
            description: None,
        }
    }
}

impl<'a> From<&'a str> for TestCase {
    fn from(argument: &'a str) -> Self {
        std::iter::once(argument).collect()
    }
}

impl From<Vec<RsTestItem>> for RsTestData {
    fn from(items: Vec<RsTestItem>) -> Self {
        Self { items }
    }
}

impl From<RsTestData> for RsTestInfo {
    fn from(data: RsTestData) -> Self {
        Self {
            data,
            attributes: Default::default(),
        }
    }
}

impl From<Vec<FixtureItem>> for FixtureData {
    fn from(fixtures: Vec<FixtureItem>) -> Self {
        Self { items: fixtures }
    }
}

pub(crate) struct EmptyResolver;

impl<'a> Resolver for EmptyResolver {
    fn resolve(&self, _ident: &Ident) -> Option<Cow<Expr>> {
        None
    }
}
