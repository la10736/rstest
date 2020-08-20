use proc_macro2::TokenStream;
use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token,
    visit_mut::VisitMut,
    Ident, ItemFn, Token,
};

use crate::{error::ErrorsVec, refident::RefIdent};
use fixture::{DefaultsFunctionExtractor, FixturesFunctionExtractor, ArgumentValue};
use quote::ToTokens;

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
