use proc_macro2::TokenStream;
use syn::{
    parse::{Parse, ParseStream},
    parse_quote,
    punctuated::Punctuated,
    token::{self, Async, Paren},
    visit_mut::VisitMut,
    FnArg, Ident, ItemFn, Token,
};

use crate::{
    error::ErrorsVec,
    refident::{MaybeIdent, RefIdent},
    utils::{attr_is, attr_starts_with},
};
use fixture::{
    ArgumentValue, DefaultsFunctionExtractor, FixtureModifiers, FixturesFunctionExtractor,
};
use quote::ToTokens;
use testcase::TestCase;

use self::{expressions::Expressions, vlist::ValueList};

// To use the macros this should be the first one module
#[macro_use]
pub(crate) mod macros;

pub(crate) mod expressions;
pub(crate) mod fixture;
pub(crate) mod future;
pub(crate) mod rstest;
pub(crate) mod testcase;
pub(crate) mod vlist;

pub(crate) trait ExtendWithFunctionAttrs {
    fn extend_with_function_attrs(
        &mut self,
        item_fn: &mut ItemFn,
    ) -> std::result::Result<(), ErrorsVec>;
}

#[derive(Default, Debug, PartialEq, Clone)]
pub(crate) struct Attributes {
    pub(crate) attributes: Vec<Attribute>,
}

impl Parse for Attributes {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let vars = Punctuated::<Attribute, Token![::]>::parse_terminated(input)?;
        Ok(Attributes {
            attributes: vars.into_iter().collect(),
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Attribute {
    Attr(Ident),
    Tagged(Ident, Vec<Ident>),
    Type(Ident, Box<syn::Type>),
}

impl Parse for Attribute {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek2(Token![<]) {
            let tag = input.parse()?;
            let _open = input.parse::<Token![<]>()?;
            let inner = input.parse()?;
            let _close = input.parse::<Token![>]>()?;
            Ok(Attribute::Type(tag, inner))
        } else if input.peek2(Token![::]) {
            let inner = input.parse()?;
            Ok(Attribute::Attr(inner))
        } else if input.peek2(token::Paren) {
            let tag = input.parse()?;
            let content;
            let _ = syn::parenthesized!(content in input);
            let args = Punctuated::<Ident, Token![,]>::parse_terminated(&content)?
                .into_iter()
                .collect();

            Ok(Attribute::Tagged(tag, args))
        } else {
            Ok(Attribute::Attr(input.parse()?))
        }
    }
}

fn parse_vector_trailing_till_double_comma<T, P>(input: ParseStream) -> syn::Result<Vec<T>>
where
    T: Parse,
    P: syn::token::Token + Parse,
{
    Ok(
        Punctuated::<Option<T>, P>::parse_separated_nonempty_with(input, |input_tokens| {
            if input_tokens.is_empty() || input_tokens.peek(Token![::]) {
                Ok(None)
            } else {
                T::parse(input_tokens).map(Some)
            }
        })?
        .into_iter()
        .flatten()
        .collect(),
    )
}

#[allow(dead_code)]
pub(crate) fn drain_stream(input: ParseStream) {
    // JUST TO SKIP ALL
    let _ = input.step(|cursor| {
        let mut rest = *cursor;
        while let Some((_, next)) = rest.token_tree() {
            rest = next
        }
        Ok(((), rest))
    });
}

#[derive(PartialEq, Debug, Clone, Default)]
pub(crate) struct Positional(pub(crate) Vec<syn::Expr>);

impl Parse for Positional {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self(
            Punctuated::<syn::Expr, Token![,]>::parse_terminated(input)?
                .into_iter()
                .collect(),
        ))
    }
}

#[derive(PartialEq, Debug, Clone)]
pub(crate) struct Fixture {
    pub(crate) name: Ident,
    pub(crate) resolve: Option<Ident>,
    pub(crate) positional: Positional,
}

impl Fixture {
    pub(crate) fn new(name: Ident, resolve: Option<Ident>, positional: Positional) -> Self {
        Self {
            name,
            resolve,
            positional,
        }
    }
}

impl Parse for Fixture {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let resolve = input.parse()?;
        if input.peek(Paren) || input.peek(Token![as]) {
            let positional = if input.peek(Paren) {
                let content;
                let _ = syn::parenthesized!(content in input);
                content.parse()?
            } else {
                Default::default()
            };

            if input.peek(Token![as]) {
                let _: Token![as] = input.parse()?;
                Ok(Self::new(input.parse()?, Some(resolve), positional))
            } else {
                Ok(Self::new(resolve, None, positional))
            }
        } else {
            Err(syn::Error::new(
                input.span(),
                "fixture need arguments or 'as new_name' format",
            ))
        }
    }
}

impl RefIdent for Fixture {
    fn ident(&self) -> &Ident {
        &self.name
    }
}

impl ToTokens for Fixture {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.name.to_tokens(tokens)
    }
}

pub(crate) fn extract_fixtures(item_fn: &mut ItemFn) -> Result<Vec<Fixture>, ErrorsVec> {
    let mut fixtures_extractor = FixturesFunctionExtractor::default();
    fixtures_extractor.visit_item_fn_mut(item_fn);

    if fixtures_extractor.1.is_empty() {
        Ok(fixtures_extractor.0)
    } else {
        Err(fixtures_extractor.1.into())
    }
}

pub(crate) fn extract_defaults(item_fn: &mut ItemFn) -> Result<Vec<ArgumentValue>, ErrorsVec> {
    let mut defaults_extractor = DefaultsFunctionExtractor::default();
    defaults_extractor.visit_item_fn_mut(item_fn);

    if defaults_extractor.1.is_empty() {
        Ok(defaults_extractor.0)
    } else {
        Err(defaults_extractor.1.into())
    }
}

pub(crate) fn extract_default_return_type(
    item_fn: &mut ItemFn,
) -> Result<Option<syn::Type>, ErrorsVec> {
    let mut default_type_extractor = DefaultTypeFunctionExtractor::default();
    default_type_extractor.visit_item_fn_mut(item_fn);
    default_type_extractor.take()
}

pub(crate) fn extract_partials_return_type(
    item_fn: &mut ItemFn,
) -> Result<Vec<(usize, syn::Type)>, ErrorsVec> {
    let mut partials_type_extractor = PartialsTypeFunctionExtractor::default();
    partials_type_extractor.visit_item_fn_mut(item_fn);
    partials_type_extractor.take()
}

pub(crate) fn extract_once(item_fn: &mut ItemFn) -> Result<Option<Ident>, ErrorsVec> {
    let mut extractor = IsOnceAttributeFunctionExtractor::default();
    extractor.visit_item_fn_mut(item_fn);
    extractor.take()
}

pub(crate) fn extract_argument_attrs<'a, B: 'a + std::fmt::Debug>(
    node: &mut FnArg,
    is_valid_attr: fn(&syn::Attribute) -> bool,
    build: fn(syn::Attribute, &Ident) -> syn::Result<B>,
) -> Box<dyn Iterator<Item = syn::Result<B>> + 'a> {
    let name = node.maybe_ident().cloned();
    if name.is_none() {
        return Box::new(std::iter::empty());
    }

    let name = name.unwrap();
    if let FnArg::Typed(ref mut arg) = node {
        // Extract interesting attributes
        let attrs = std::mem::take(&mut arg.attrs);
        let (extracted, remain): (Vec<_>, Vec<_>) = attrs.into_iter().partition(is_valid_attr);

        arg.attrs = remain;

        // Parse attrs
        Box::new(extracted.into_iter().map(move |attr| build(attr, &name)))
    } else {
        Box::new(std::iter::empty())
    }
}

/// Simple struct used to visit function attributes and extract default return
/// type
struct DefaultTypeFunctionExtractor(Result<Option<syn::Type>, ErrorsVec>);

impl DefaultTypeFunctionExtractor {
    fn take(self) -> Result<Option<syn::Type>, ErrorsVec> {
        self.0
    }
}

impl Default for DefaultTypeFunctionExtractor {
    fn default() -> Self {
        Self(Ok(None))
    }
}

impl VisitMut for DefaultTypeFunctionExtractor {
    fn visit_item_fn_mut(&mut self, node: &mut ItemFn) {
        let attrs = std::mem::take(&mut node.attrs);
        let (defaults, remain): (Vec<_>, Vec<_>) = attrs
            .into_iter()
            .partition(|attr| attr_is(attr, FixtureModifiers::DEFAULT_RET_ATTR));

        node.attrs = remain;
        let mut defaults = defaults.into_iter();
        let mut data = None;
        let mut errors = ErrorsVec::default();
        match defaults.next().map(|def| def.parse_args::<syn::Type>()) {
            Some(Ok(t)) => data = Some(t),
            Some(Err(e)) => errors.push(e),
            None => {}
        };
        errors.extend(
            defaults.map(|a| syn::Error::new_spanned(a, "You cannot use default more than once")),
        );
        self.0 = if errors.len() > 0 {
            Err(errors)
        } else {
            Ok(data)
        };
    }
}

/// Simple struct used to visit function attributes and extract default return
/// type
struct PartialsTypeFunctionExtractor(Result<Vec<(usize, syn::Type)>, ErrorsVec>);

impl PartialsTypeFunctionExtractor {
    fn take(self) -> Result<Vec<(usize, syn::Type)>, ErrorsVec> {
        self.0
    }
}

impl Default for PartialsTypeFunctionExtractor {
    fn default() -> Self {
        Self(Ok(Vec::default()))
    }
}

impl VisitMut for PartialsTypeFunctionExtractor {
    fn visit_item_fn_mut(&mut self, node: &mut ItemFn) {
        let attrs = std::mem::take(&mut node.attrs);
        let (partials, remain): (Vec<_>, Vec<_>) =
            attrs
                .into_iter()
                .partition(|attr| match attr.path().get_ident() {
                    Some(name) => name
                        .to_string()
                        .starts_with(FixtureModifiers::PARTIAL_RET_ATTR),
                    None => false,
                });

        node.attrs = remain;
        let mut errors = ErrorsVec::default();
        let mut data: Vec<(usize, syn::Type)> = Vec::default();
        for attr in partials {
            match attr.parse_args::<syn::Type>() {
                Ok(t) => {
                    match attr.path().get_ident().unwrap().to_string()
                        [FixtureModifiers::PARTIAL_RET_ATTR.len()..]
                        .parse()
                    {
                        Ok(id) => data.push((id, t)),
                        Err(_) => errors.push(syn::Error::new_spanned(
                            attr,
                            "Invalid partial syntax: should be partial_<n_arguments>",
                        )),
                    }
                }
                Err(e) => errors.push(e),
            }
        }
        self.0 = if errors.len() > 0 {
            Err(errors)
        } else {
            Ok(data)
        };
    }
}

/// Simple struct used to visit function attributes and extract once
/// type
struct IsOnceAttributeFunctionExtractor(Result<Option<Ident>, ErrorsVec>);

impl IsOnceAttributeFunctionExtractor {
    fn take(self) -> Result<Option<Ident>, ErrorsVec> {
        self.0
    }
}

impl Default for IsOnceAttributeFunctionExtractor {
    fn default() -> Self {
        Self(Ok(None))
    }
}

impl VisitMut for IsOnceAttributeFunctionExtractor {
    fn visit_item_fn_mut(&mut self, node: &mut ItemFn) {
        let attrs = std::mem::take(&mut node.attrs);
        let (onces, remain): (Vec<_>, Vec<_>) =
            attrs.into_iter().partition(|attr| attr_is(attr, "once"));

        node.attrs = remain;
        self.0 = match onces.len() {
            1 => Ok(onces[0].path().get_ident().cloned()),
            0 => Ok(None),
            _ => Err(onces
                .into_iter()
                .skip(1)
                .map(|attr| syn::Error::new_spanned(attr, "You cannot use #[once] more than once"))
                .collect::<Vec<_>>()
                .into()),
        };
    }
}

/// Simple struct used to visit function attributes and extract case arguments and
/// eventualy parsing errors
#[derive(Default)]
struct CaseArgsFunctionExtractor(Vec<Ident>, Vec<syn::Error>);

impl VisitMut for CaseArgsFunctionExtractor {
    fn visit_fn_arg_mut(&mut self, node: &mut FnArg) {
        for r in extract_argument_attrs(node, |a| attr_is(a, "case"), |_a, name| Ok(name.clone())) {
            match r {
                Ok(value) => self.0.push(value),
                Err(err) => self.1.push(err),
            }
        }

        syn::visit_mut::visit_fn_arg_mut(self, node);
    }
}

pub(crate) fn extract_case_args(item_fn: &mut ItemFn) -> Result<Vec<Ident>, ErrorsVec> {
    let mut case_args_extractor = CaseArgsFunctionExtractor::default();
    case_args_extractor.visit_item_fn_mut(item_fn);

    if case_args_extractor.1.is_empty() {
        Ok(case_args_extractor.0)
    } else {
        Err(case_args_extractor.1.into())
    }
}

/// Simple struct used to visit function attributes and extract cases and
/// eventualy parsing errors
#[derive(Default)]
struct CasesFunctionExtractor(Vec<TestCase>, Vec<syn::Error>);

impl VisitMut for CasesFunctionExtractor {
    fn visit_item_fn_mut(&mut self, node: &mut ItemFn) {
        let attrs = std::mem::take(&mut node.attrs);
        let mut attrs_buffer = Default::default();
        let case: syn::PathSegment = parse_quote! { case };
        for attr in attrs.into_iter() {
            if attr_starts_with(&attr, &case) {
                match attr.parse_args::<Expressions>() {
                    Ok(expressions) => {
                        let description =
                            attr.path().segments.iter().nth(1).map(|p| p.ident.clone());
                        self.0.push(TestCase {
                            args: expressions.into(),
                            attrs: std::mem::take(&mut attrs_buffer),
                            description,
                        });
                    }
                    Err(err) => self.1.push(err),
                };
            } else {
                attrs_buffer.push(attr)
            }
        }
        node.attrs = std::mem::take(&mut attrs_buffer);
    }
}

pub(crate) fn extract_cases(item_fn: &mut ItemFn) -> Result<Vec<TestCase>, ErrorsVec> {
    let mut cases_extractor = CasesFunctionExtractor::default();
    cases_extractor.visit_item_fn_mut(item_fn);

    if cases_extractor.1.is_empty() {
        Ok(cases_extractor.0)
    } else {
        Err(cases_extractor.1.into())
    }
}

/// Simple struct used to visit function attributes and extract value list and
/// eventualy parsing errors
#[derive(Default)]
struct ValueListFunctionExtractor(Vec<ValueList>, Vec<syn::Error>);

impl VisitMut for ValueListFunctionExtractor {
    fn visit_fn_arg_mut(&mut self, node: &mut FnArg) {
        for r in extract_argument_attrs(
            node,
            |a| attr_is(a, "values"),
            |a, name| {
                a.parse_args::<Expressions>().map(|v| ValueList {
                    arg: name.clone(),
                    values: v.take(),
                })
            },
        ) {
            match r {
                Ok(vlist) => self.0.push(vlist),
                Err(err) => self.1.push(err),
            }
        }

        syn::visit_mut::visit_fn_arg_mut(self, node);
    }
}

pub(crate) fn extract_value_list(item_fn: &mut ItemFn) -> Result<Vec<ValueList>, ErrorsVec> {
    let mut vlist_extractor = ValueListFunctionExtractor::default();
    vlist_extractor.visit_item_fn_mut(item_fn);

    if vlist_extractor.1.is_empty() {
        Ok(vlist_extractor.0)
    } else {
        Err(vlist_extractor.1.into())
    }
}

/// Simple struct used to visit function args attributes to extract the
/// excluded ones and eventualy parsing errors
struct ExcludedTraceAttributesFunctionExtractor(Result<Vec<Ident>, ErrorsVec>);
impl From<Result<Vec<Ident>, ErrorsVec>> for ExcludedTraceAttributesFunctionExtractor {
    fn from(inner: Result<Vec<Ident>, ErrorsVec>) -> Self {
        Self(inner)
    }
}

impl ExcludedTraceAttributesFunctionExtractor {
    pub(crate) fn take(self) -> Result<Vec<Ident>, ErrorsVec> {
        self.0
    }

    fn update_error(&mut self, mut errors: ErrorsVec) {
        match &mut self.0 {
            Ok(_) => self.0 = Err(errors),
            Err(err) => err.append(&mut errors),
        }
    }

    fn update_excluded(&mut self, value: Ident) {
        if let Some(inner) = self.0.iter_mut().next() {
            inner.push(value);
        }
    }
}

impl Default for ExcludedTraceAttributesFunctionExtractor {
    fn default() -> Self {
        Self(Ok(Default::default()))
    }
}

impl VisitMut for ExcludedTraceAttributesFunctionExtractor {
    fn visit_fn_arg_mut(&mut self, node: &mut FnArg) {
        for r in
            extract_argument_attrs(node, |a| attr_is(a, "notrace"), |_a, name| Ok(name.clone()))
        {
            match r {
                Ok(value) => self.update_excluded(value),
                Err(err) => self.update_error(err.into()),
            }
        }

        syn::visit_mut::visit_fn_arg_mut(self, node);
    }
}

pub(crate) fn extract_excluded_trace(item_fn: &mut ItemFn) -> Result<Vec<Ident>, ErrorsVec> {
    let mut excluded_trace_extractor = ExcludedTraceAttributesFunctionExtractor::default();
    excluded_trace_extractor.visit_item_fn_mut(item_fn);
    excluded_trace_extractor.take()
}

/// Simple struct used to visit function args attributes to check timeout syntax
struct CheckTimeoutAttributesFunction(Result<(), ErrorsVec>);
impl From<ErrorsVec> for CheckTimeoutAttributesFunction {
    fn from(errors: ErrorsVec) -> Self {
        Self(Err(errors))
    }
}

impl CheckTimeoutAttributesFunction {
    pub(crate) fn take(self) -> Result<(), ErrorsVec> {
        self.0
    }

    fn check_if_can_implement_timeous(
        &self,
        timeouts: &[&syn::Attribute],
        asyncness: Option<&Async>,
    ) -> Option<syn::Error> {
        if cfg!(feature = "async-timeout") || timeouts.is_empty() {
            None
        } else {
            asyncness.map(|a| {
                syn::Error::new(
                    a.span,
                    "Enable async-timeout feature to use timeout in async tests",
                )
            })
        }
    }
}

impl Default for CheckTimeoutAttributesFunction {
    fn default() -> Self {
        Self(Ok(()))
    }
}

impl VisitMut for CheckTimeoutAttributesFunction {
    fn visit_item_fn_mut(&mut self, node: &mut ItemFn) {
        let timeouts = node
            .attrs
            .iter()
            .filter(|&a| attr_is(a, "timeout"))
            .collect::<Vec<_>>();
        let mut errors = timeouts
            .iter()
            .map(|&attr| attr.parse_args::<syn::Expr>())
            .filter_map(Result::err)
            .collect::<Vec<_>>();

        if let Some(e) =
            self.check_if_can_implement_timeous(timeouts.as_slice(), node.sig.asyncness.as_ref())
        {
            errors.push(e);
        }
        if !errors.is_empty() {
            *self = Self(Err(errors.into()));
        }
    }
}

pub(crate) fn check_timeout_attrs(item_fn: &mut ItemFn) -> Result<(), ErrorsVec> {
    let mut checker = CheckTimeoutAttributesFunction::default();
    checker.visit_item_fn_mut(item_fn);
    checker.take()
}

pub(crate) mod arguments {
    use std::collections::HashMap;

    use syn::Ident;

    #[derive(PartialEq, Debug, Clone, Copy)]
    #[allow(dead_code)]
    pub(crate) enum FutureArg {
        None,
        Define,
        Await,
    }

    impl Default for FutureArg {
        fn default() -> Self {
            FutureArg::None
        }
    }

    #[derive(PartialEq, Default, Debug)]
    pub(crate) struct ArgumentInfo {
        future: FutureArg,
    }

    impl ArgumentInfo {
        fn future(future: FutureArg) -> Self {
            Self { future }
        }

        fn is_future(&self) -> bool {
            use FutureArg::*;

            matches!(self.future, Define | Await)
        }

        fn is_future_await(&self) -> bool {
            use FutureArg::*;

            matches!(self.future, Await)
        }
    }

    #[derive(PartialEq, Default, Debug)]
    pub(crate) struct ArgumentsInfo {
        args: HashMap<Ident, ArgumentInfo>,
        is_global_await: bool,
        once: Option<Ident>,
    }

    impl ArgumentsInfo {
        pub(crate) fn set_future(&mut self, ident: Ident, kind: FutureArg) {
            self.args
                .entry(ident)
                .and_modify(|v| v.future = kind)
                .or_insert_with(|| ArgumentInfo::future(kind));
        }

        pub(crate) fn set_futures(&mut self, futures: impl Iterator<Item = (Ident, FutureArg)>) {
            futures.for_each(|(ident, k)| self.set_future(ident, k));
        }

        pub(crate) fn set_global_await(&mut self, is_global_await: bool) {
            self.is_global_await = is_global_await;
        }

        #[allow(dead_code)]
        pub(crate) fn add_future(&mut self, ident: Ident) {
            self.set_future(ident, FutureArg::Define);
        }

        pub(crate) fn is_future(&self, id: &Ident) -> bool {
            self.args
                .get(id)
                .map(|arg| arg.is_future())
                .unwrap_or_default()
        }

        pub(crate) fn is_future_await(&self, ident: &Ident) -> bool {
            match self.args.get(ident) {
                Some(arg) => arg.is_future_await() || (arg.is_future() && self.is_global_await()),
                None => false,
            }
        }

        pub(crate) fn is_global_await(&self) -> bool {
            self.is_global_await
        }

        pub(crate) fn set_once(&mut self, once: Option<Ident>) {
            self.once = once
        }

        pub(crate) fn get_once(&self) -> Option<&Ident> {
            self.once.as_ref()
        }

        pub(crate) fn is_once(&self) -> bool {
            self.get_once().is_some()
        }
    }

    #[cfg(test)]
    mod should_implement_is_future_await_logic {
        use super::*;
        use crate::test::*;

        #[fixture]
        fn info() -> ArgumentsInfo {
            let mut a = ArgumentsInfo::default();
            a.set_future(ident("simple"), FutureArg::Define);
            a.set_future(ident("other_simple"), FutureArg::Define);
            a.set_future(ident("awaited"), FutureArg::Await);
            a.set_future(ident("other_awaited"), FutureArg::Await);
            a.set_future(ident("none"), FutureArg::None);
            a
        }

        #[rstest]
        fn no_matching_ident(info: ArgumentsInfo) {
            assert!(!info.is_future_await(&ident("some")));
            assert!(!info.is_future_await(&ident("simple")));
            assert!(!info.is_future_await(&ident("none")));
        }

        #[rstest]
        fn matching_ident(info: ArgumentsInfo) {
            assert!(info.is_future_await(&ident("awaited")));
            assert!(info.is_future_await(&ident("other_awaited")));
        }

        #[rstest]
        fn global_matching_future_ident(mut info: ArgumentsInfo) {
            info.set_global_await(true);
            assert!(info.is_future_await(&ident("simple")));
            assert!(info.is_future_await(&ident("other_simple")));
            assert!(info.is_future_await(&ident("awaited")));

            assert!(!info.is_future_await(&ident("some")));
            assert!(!info.is_future_await(&ident("none")));
        }
    }
}

#[cfg(test)]
mod should {
    use super::*;
    use crate::test::*;

    mod parse_attributes {
        use super::assert_eq;
        use super::*;

        fn parse_attributes<S: AsRef<str>>(attributes: S) -> Attributes {
            parse_meta(attributes)
        }

        #[test]
        fn one_simple_ident() {
            let attributes = parse_attributes("my_ident");

            let expected = Attributes {
                attributes: vec![Attribute::attr("my_ident")],
            };

            assert_eq!(expected, attributes);
        }

        #[test]
        fn one_simple_group() {
            let attributes = parse_attributes("group_tag(first, second)");

            let expected = Attributes {
                attributes: vec![Attribute::tagged("group_tag", vec!["first", "second"])],
            };

            assert_eq!(expected, attributes);
        }

        #[test]
        fn one_simple_type() {
            let attributes = parse_attributes("type_tag<(u32, T, (String, i32))>");

            let expected = Attributes {
                attributes: vec![Attribute::typed("type_tag", "(u32, T, (String, i32))")],
            };

            assert_eq!(expected, attributes);
        }

        #[test]
        fn integrated() {
            let attributes = parse_attributes(
                r#"
            simple :: tagged(first, second) :: type_tag<(u32, T, (std::string::String, i32))> :: more_tagged(a,b)"#,
            );

            let expected = Attributes {
                attributes: vec![
                    Attribute::attr("simple"),
                    Attribute::tagged("tagged", vec!["first", "second"]),
                    Attribute::typed("type_tag", "(u32, T, (std::string::String, i32))"),
                    Attribute::tagged("more_tagged", vec!["a", "b"]),
                ],
            };

            assert_eq!(expected, attributes);
        }
    }
}
