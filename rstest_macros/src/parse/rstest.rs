use syn::{
    parse::{Parse, ParseStream},
    visit_mut::VisitMut,
    Ident, ItemFn, Token,
};

use self::{
    files_args::{extract_files_args, ValueListFromFiles},
    json::{FilesExtractor, JsonFiles},
};

use super::{
    arguments::ArgumentsInfo,
    check_timeout_attrs, extract_case_args, extract_cases, extract_excluded_trace,
    extract_fixtures, extract_value_list,
    future::{extract_futures, extract_global_awt},
    parse_vector_trailing_till_double_comma,
    sys::{DefaultSysEngine, SysEngine},
    testcase::TestCase,
    Attribute, Attributes, ExtendWithFunctionAttrs, Fixture,
};
use crate::{error::attribute_used_more_than_once, parse::vlist::ValueList};
use crate::{
    error::ErrorsVec,
    refident::{MaybeIdent, RefIdent},
    utils::attr_is,
};
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, ToTokens};

pub(crate) mod file;
pub(crate) mod files_args;
mod json;

#[cfg(test)]
mod test;

#[derive(PartialEq, Debug, Default)]
pub(crate) struct RsTestInfo {
    pub(crate) data: RsTestData,
    pub(crate) attributes: RsTestAttributes,
    pub(crate) arguments: ArgumentsInfo,
}

impl Parse for RsTestInfo {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(if input.is_empty() {
            Default::default()
        } else {
            Self {
                data: input.parse()?,
                attributes: input
                    .parse::<Token![::]>()
                    .or_else(|_| Ok(Default::default()))
                    .and_then(|_| input.parse())?,
                arguments: Default::default(),
            }
        })
    }
}

impl ExtendWithFunctionAttrs for RsTestInfo {
    fn extend_with_function_attrs<S: SysEngine>(
        &mut self,
        item_fn: &mut ItemFn,
    ) -> Result<(), ErrorsVec> {
        let composed_tuple!(_inner, excluded, _timeout, futures, global_awt) = merge_errors!(
            self.data.extend_with_function_attrs::<S>(item_fn),
            extract_excluded_trace(item_fn),
            check_timeout_attrs(item_fn),
            extract_futures(item_fn),
            extract_global_awt(item_fn)
        )?;
        self.attributes.add_notraces(excluded);
        self.arguments.set_global_await(global_awt);
        self.arguments.set_futures(futures.into_iter());
        Ok(())
    }
}

#[derive(PartialEq, Debug, Default)]
pub(crate) struct RsTestData {
    pub(crate) items: Vec<RsTestItem>,
}

impl RsTestData {
    pub(crate) fn case_args(&self) -> impl Iterator<Item = &Ident> {
        self.items.iter().filter_map(|it| match it {
            RsTestItem::CaseArgName(ref arg) => Some(arg),
            _ => None,
        })
    }

    #[allow(dead_code)]
    pub(crate) fn has_case_args(&self) -> bool {
        self.case_args().next().is_some()
    }

    pub(crate) fn cases(&self) -> impl Iterator<Item = &TestCase> {
        self.items.iter().filter_map(|it| match it {
            RsTestItem::TestCase(ref case) => Some(case),
            _ => None,
        })
    }

    pub(crate) fn has_cases(&self) -> bool {
        self.cases().next().is_some()
    }

    pub(crate) fn fixtures(&self) -> impl Iterator<Item = &Fixture> {
        self.items.iter().filter_map(|it| match it {
            RsTestItem::Fixture(ref fixture) => Some(fixture),
            _ => None,
        })
    }

    #[allow(dead_code)]
    pub(crate) fn has_fixtures(&self) -> bool {
        self.fixtures().next().is_some()
    }

    pub(crate) fn list_values(&self) -> impl Iterator<Item = &ValueList> {
        self.items.iter().filter_map(|mv| match mv {
            RsTestItem::ValueList(ref value_list) => Some(value_list),
            _ => None,
        })
    }

    pub(crate) fn has_list_values(&self) -> bool {
        self.list_values().next().is_some()
    }

    fn files(&self) -> Option<&JsonFiles> {
        self.items.iter().find_map(|it| match it {
            RsTestItem::Files(ref files) => Some(files),
            _ => None,
        })
    }
}

impl Parse for RsTestData {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![::]) {
            Ok(Default::default())
        } else {
            Ok(Self {
                items: parse_vector_trailing_till_double_comma::<_, Token![,]>(input)?,
            })
        }
    }
}

fn maybe_parse_attribute_args_just_once<T: Parse>(
    node: &syn::PatType,
    name: &str,
) -> (Option<Option<T>>, Vec<syn::Error>) {
    let mut errors = Vec::new();
    let val = node
        .attrs
        .iter()
        .filter(|&a| attr_is(a, name))
        .map(|a| {
            (
                a,
                match &a.meta {
                    syn::Meta::Path(_path) => None,
                    _ => Some(a.parse_args::<T>()),
                },
            )
        })
        .fold(None, |first, (a, res)| match (first, res) {
            (None, None) => Some(None),
            (None, Some(Ok(parsed))) => Some(Some(parsed)),
            (first, Some(Err(err))) => {
                errors.push(err);
                first
            }
            (first, _) => {
                errors.push(attribute_used_more_than_once(a, name));
                first
            }
        });
    (val, errors)
}

fn attribute_args_once<'a>(
    node: &'a syn::PatType,
    name: &str,
) -> (Option<&'a syn::Attribute>, Vec<syn::Error>) {
    let mut errors = Vec::new();
    let mut attributes = node
        .attrs
        .iter()
        .filter(|&a| attr_is(a, name))
        .map(|a| match a.meta.require_path_only() {
            Ok(_) => a,
            Err(err) => {
                errors.push(err);
                a
            }
        })
        .collect::<Vec<_>>()
        .into_iter();
    let val = attributes.next();
    while let Some(attr) = attributes.next() {
        errors.push(attribute_used_more_than_once(attr, name));
    }
    (val, errors)
}

pub(crate) fn extract_files<S: SysEngine>(
    item_fn: &mut ItemFn,
) -> Result<Option<JsonFiles>, ErrorsVec> {
    let mut extractor = FilesExtractor::<S>::default();
    extractor.visit_item_fn_mut(item_fn);

    if extractor.errors.is_empty() {
        Ok(extractor.hierarchy)
    } else {
        Err(extractor.errors.into())
    }
}

impl ExtendWithFunctionAttrs for RsTestData {
    fn extend_with_function_attrs<S: SysEngine>(
        &mut self,
        item_fn: &mut ItemFn,
    ) -> Result<(), ErrorsVec> {
        let composed_tuple!(fixtures, case_args, cases, value_list, files, files_args) = merge_errors!(
            extract_fixtures(item_fn),
            extract_case_args(item_fn),
            extract_cases(item_fn),
            extract_value_list(item_fn),
            extract_files::<S>(item_fn),
            extract_files_args(item_fn)
        )?;

        self.items.extend(fixtures.into_iter().map(|f| f.into()));
        self.items.extend(case_args.into_iter().map(|f| f.into()));
        self.items.extend(cases.into_iter().map(|f| f.into()));
        self.items.extend(value_list.into_iter().map(|f| f.into()));
        self.items.extend(files.into_iter().map(|f| f.into()));
        self.items.extend(
            ValueListFromFiles::<DefaultSysEngine>::default()
                .to_value_list(files_args)?
                .into_iter()
                .map(|f| f.into()),
        );
        Ok(())
    }
}

#[derive(PartialEq, Debug)]
pub(crate) enum RsTestItem {
    Fixture(Fixture),
    CaseArgName(Ident),
    TestCase(TestCase),
    ValueList(ValueList),
    Files(JsonFiles),
}

impl From<Fixture> for RsTestItem {
    fn from(f: Fixture) -> Self {
        RsTestItem::Fixture(f)
    }
}

impl From<Ident> for RsTestItem {
    fn from(ident: Ident) -> Self {
        RsTestItem::CaseArgName(ident)
    }
}

impl From<TestCase> for RsTestItem {
    fn from(case: TestCase) -> Self {
        RsTestItem::TestCase(case)
    }
}

impl From<ValueList> for RsTestItem {
    fn from(value_list: ValueList) -> Self {
        RsTestItem::ValueList(value_list)
    }
}

impl From<JsonFiles> for RsTestItem {
    fn from(value: JsonFiles) -> Self {
        RsTestItem::Files(value)
    }
}

impl Parse for RsTestItem {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.fork().parse::<TestCase>().is_ok() {
            input.parse::<TestCase>().map(RsTestItem::TestCase)
        } else if input.peek2(Token![=>]) {
            input.parse::<ValueList>().map(RsTestItem::ValueList)
        } else if input.fork().parse::<Fixture>().is_ok() {
            input.parse::<Fixture>().map(RsTestItem::Fixture)
        } else if input.fork().parse::<Ident>().is_ok() {
            input.parse::<Ident>().map(RsTestItem::CaseArgName)
        } else {
            Err(syn::Error::new(Span::call_site(), "Cannot parse it"))
        }
    }
}

impl MaybeIdent for RsTestItem {
    fn maybe_ident(&self) -> Option<&Ident> {
        use RsTestItem::*;
        match self {
            Fixture(ref fixture) => Some(fixture.ident()),
            CaseArgName(ref case_arg) => Some(case_arg),
            ValueList(ref value_list) => Some(value_list.ident()),
            TestCase(_) => None,
            Files(_) => None,
        }
    }
}

impl ToTokens for RsTestItem {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        use RsTestItem::*;
        match self {
            Fixture(ref fixture) => fixture.to_tokens(tokens),
            CaseArgName(ref case_arg) => case_arg.to_tokens(tokens),
            TestCase(ref case) => case.to_tokens(tokens),
            ValueList(ref list) => list.to_tokens(tokens),
            Files(files) => files.to_tokens(tokens),
        }
    }
}

wrap_attributes!(RsTestAttributes);

impl RsTestAttributes {
    const TRACE_VARIABLE_ATTR: &'static str = "trace";
    const NOTRACE_VARIABLE_ATTR: &'static str = "notrace";

    pub(crate) fn trace_me(&self, ident: &Ident) -> bool {
        if self.should_trace() {
            !self.iter().any(|m| Self::is_notrace(ident, m))
        } else {
            false
        }
    }

    fn is_notrace(ident: &Ident, m: &Attribute) -> bool {
        match m {
            Attribute::Tagged(i, args) if i == Self::NOTRACE_VARIABLE_ATTR => {
                args.iter().any(|a| a == ident)
            }
            _ => false,
        }
    }

    pub(crate) fn should_trace(&self) -> bool {
        self.iter().any(Self::is_trace)
    }

    pub(crate) fn add_trace(&mut self, trace: Ident) {
        self.inner.attributes.push(Attribute::Attr(trace));
    }

    pub(crate) fn add_notraces(&mut self, notraces: Vec<Ident>) {
        if notraces.is_empty() {
            return;
        }
        self.inner.attributes.push(Attribute::Tagged(
            format_ident!("{}", Self::NOTRACE_VARIABLE_ATTR),
            notraces,
        ));
    }

    fn is_trace(m: &Attribute) -> bool {
        matches!(m, Attribute::Attr(i) if i == Self::TRACE_VARIABLE_ATTR)
    }
}

impl Parse for RsTestAttributes {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(input.parse::<Attributes>()?.into())
    }
}
