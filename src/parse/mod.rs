use proc_macro2::TokenStream;
use syn::{FnArg, Ident, ItemFn, Token, parse::{Parse, ParseStream}, parse_quote, punctuated::Punctuated, spanned::Spanned, token, visit_mut::VisitMut};

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

use self::vlist::{Expressions, ValueList};

// To use the macros this should be the first one module
#[macro_use]
pub(crate) mod macros;

pub(crate) mod fixture;
pub(crate) mod rstest;
pub(crate) mod testcase;
pub(crate) mod vlist;

pub(crate) trait ExtendWithFunctionAttrs {
    fn extend_with_function_attrs(
        &mut self,
        item_fn: &mut ItemFn,
    ) -> std::result::Result<(), ErrorsVec>;
}

#[derive(Default, Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub(crate) enum Attribute {
    Attr(Ident),
    Tagged(Ident, Vec<Ident>),
    Type(Ident, syn::Type),
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
                T::parse(input_tokens).map(|inner| Some(inner))
            }
        })?
        .into_iter()
        .filter_map(|it| it)
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

#[derive(PartialEq, Debug, Clone)]
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
    pub(crate) positional: Positional,
}

impl Fixture {
    pub(crate) fn new(name: Ident, positional: Positional) -> Self {
        Self { name, positional }
    }
}

impl Parse for Fixture {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name = input.parse()?;
        let content;
        let _ = syn::parenthesized!(content in input);
        let positional = content.parse()?;
        Ok(Self::new(name, positional))
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

    if fixtures_extractor.1.len() > 0 {
        Err(fixtures_extractor.1.into())
    } else {
        Ok(fixtures_extractor.0)
    }
}

pub(crate) fn extract_defaults(item_fn: &mut ItemFn) -> Result<Vec<ArgumentValue>, ErrorsVec> {
    let mut defaults_extractor = DefaultsFunctionExtractor::default();
    defaults_extractor.visit_item_fn_mut(item_fn);

    if defaults_extractor.1.len() > 0 {
        Err(defaults_extractor.1.into())
    } else {
        Ok(defaults_extractor.0)
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

fn extract_argument_attrs<'a, B: 'a + std::fmt::Debug>(
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
        let (extracted, remain): (Vec<_>, Vec<_>) =
            attrs.into_iter().partition(|attr| is_valid_attr(attr));

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
        let (mut defaults, remain): (Vec<_>, Vec<_>) = attrs
            .into_iter()
            .partition(|attr| attr_is(&attr, FixtureModifiers::DEFAULT_RET_ATTR));

        node.attrs = remain;
        self.0 = match defaults.pop().map(|def| def.parse_args::<syn::Type>()) {
            Some(Ok(t)) => Ok(Some(t)),
            Some(Err(e)) => Err(e.into()),
            None => Ok(None),
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
                .partition(|attr| match attr.path.get_ident() {
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
                    match attr.path.get_ident().unwrap().to_string()
                        [FixtureModifiers::PARTIAL_RET_ATTR.len()..]
                        .parse()
                    {
                        Ok(id) => data.push((id, t)),
                        Err(_) => errors.push(syn::Error::new(attr.span(), "Invalid patial syntax: should be partial_<n_arguments>")),
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
    }
}

pub(crate) fn extract_case_args(item_fn: &mut ItemFn) -> Result<Vec<Ident>, ErrorsVec> {
    let mut case_args_extractor = CaseArgsFunctionExtractor::default();
    case_args_extractor.visit_item_fn_mut(item_fn);

    if case_args_extractor.1.len() > 0 {
        Err(case_args_extractor.1.into())
    } else {
        Ok(case_args_extractor.0)
    }
}

/// Simple struct used to visit function attributes and extract cases and
/// eventualy parsing errors
#[derive(Default)]
struct CasesFunctionExtractor(Vec<TestCase>, Vec<syn::Error>);

impl VisitMut for CasesFunctionExtractor {
    fn visit_item_fn_mut(&mut self, node: &mut ItemFn) {
        let attrs = std::mem::take(&mut node.attrs);
        let mut tc: Option<TestCase> = None;
        let case: syn::PathSegment = parse_quote! { case };
        for attr in attrs.into_iter() {
            if attr_starts_with(&attr, &case) {
                match attr
                    .parse_meta()
                    .map(|m| m.into_token_stream())
                    .and_then(|tokens| syn::parse2::<TestCase>(tokens))
                {
                    Ok(t) => {
                        match tc {
                            Some(test_case) => self.0.push(test_case),
                            None => {}
                        }
                        tc = Some(t);
                    }
                    Err(err) => self.1.push(err),
                };
            } else {
                let dest = tc.as_mut().map(|t| &mut t.attrs).unwrap_or(&mut node.attrs);
                dest.push(attr);
            }
        }
        match tc {
            Some(test_case) => self.0.push(test_case),
            None => {}
        }
    }
}

pub(crate) fn extract_cases(item_fn: &mut ItemFn) -> Result<Vec<TestCase>, ErrorsVec> {
    let mut cases_extractor = CasesFunctionExtractor::default();
    cases_extractor.visit_item_fn_mut(item_fn);

    if cases_extractor.1.len() > 0 {
        Err(cases_extractor.1.into())
    } else {
        Ok(cases_extractor.0)
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
    }
}

pub(crate) fn extract_value_list(item_fn: &mut ItemFn) -> Result<Vec<ValueList>, ErrorsVec> {
    let mut vlist_extractor = ValueListFunctionExtractor::default();
    vlist_extractor.visit_item_fn_mut(item_fn);

    if vlist_extractor.1.len() > 0 {
        Err(vlist_extractor.1.into())
    } else {
        Ok(vlist_extractor.0)
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
