use proc_macro2::TokenStream;
use syn::{Expr, Ident, Lit, LitStr, Meta, MetaList, NestedMeta, Token,
          parse::{Error, Parse, ParseStream, Result},
          punctuated::Punctuated,
          spanned::Spanned};

use cfg_if::cfg_if;
use quote::ToTokens;
use crate::refident::RefIdent;

// To use the macros this should be the first one module
#[macro_use]
pub(crate) mod macros;

pub(crate) mod fixture;
pub(crate) mod rstest;
pub(crate) mod parametrize;
pub(crate) mod matrix;

#[derive(Debug, Clone)]
/// A test case's argument as an expression that can be assigned.
pub(crate) struct CaseArg {
    expr: Expr,
}

impl CaseArg {
    pub(crate) fn new(expr: Expr) -> Self {
        Self { expr }
    }
}

#[cfg(test)]
impl PartialEq for CaseArg {
    fn eq(&self, other: &Self) -> bool {
        self.expr == other.expr
    }
}

// To Enable Spanned trait
impl ToTokens for CaseArg {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.expr.to_tokens(tokens);
    }
}

impl From<Expr> for CaseArg {
    fn from(expr: Expr) -> Self {
        CaseArg::new(expr)
    }
}

impl Parse for CaseArg {
    fn parse(input: ParseStream) -> Result<Self> {
        if UnwrapRustCode::peek(input) {
            Ok(CaseArg::new(input.parse::<UnwrapRustCode>()?.0))
        } else {
            input.parse()
                .map(CaseArg::new)
                .map_err(|e| Error::new(
                    e.span(),
                    format!("Cannot parse due {}", e),
                )
                )
        }
    }
}

struct UnwrapRustCode(Expr);

impl Parse for UnwrapRustCode {
    fn parse(input: ParseStream) -> Result<Self> {
        let nested: NestedMeta = input.parse()?;
        Self::report_deprecated(&nested);
        let arg = Self::get_unwrap(&nested)?;
        arg.nested.first()
            .and_then(Self::nested_meta_literal_str)
            .ok_or(syn::Error::new_spanned(&nested,
                                           &format!("Invalid {} argument", UnwrapRustCode::UNWRAP_NAME)))
            .and_then(|lit| lit.parse())
            .map(UnwrapRustCode)
    }
}

impl UnwrapRustCode {
    const UNWRAP_NAME: &'static str = "Unwrap";

    fn peek(input: ParseStream) -> bool {
        input.fork().parse::<NestedMeta>().map(|nested|
            Self::get_unwrap(&nested).is_ok()
        ).unwrap_or(false)
    }

    fn get_unwrap(nested: &NestedMeta) -> Result<&MetaList> {
        match nested {
            NestedMeta::Meta(Meta::List(ref arg)) if
                arg.path.get_ident().map(|id| id.to_string()) == Some(Self::UNWRAP_NAME.to_string()) => Ok(arg),
            _ => return Err(Error::new(nested.span(), "Not a valid string rust code"))
        }
    }

    fn report_deprecated(nested: &NestedMeta) {
        cfg_if! {
        if #[cfg(use_proc_macro_diagnostic)] {
            fn inner(nested: &NestedMeta) {
                nested.span()
                    .unwrap()
                    .warning("Deprecated: Case argument accepts arbitrary rust code now.")
                    .emit();
            }
        } else {
            fn inner(nested: &NestedMeta) {
                match nested {
                    NestedMeta::Meta(Meta::List(arg)) => {
                        arg.nested.first()
                            .and_then(UnwrapRustCode::nested_meta_literal_str)
                            .map(|content| {
                                eprintln!(r#"{}("<code>") is deprecated. Case argument accepts arbitrary rust code now."#,
                                          UnwrapRustCode::UNWRAP_NAME);
                                content
                            })
                            .unwrap();
                    }
                    _ => { unreachable!() }
                }
            }
        }
        }
        inner(nested);
    }

    fn nested_meta_literal_str(nested: &NestedMeta) -> Option<&LitStr> {
        match nested {
            NestedMeta::Lit(Lit::Str(lit)) => Some(lit),
            _ => None
        }
    }
}

#[derive(Default, Debug, PartialEq)]
pub(crate) struct Attributes {
    pub(crate) attributes: Vec<Attribute>
}

impl Parse for Attributes {
    fn parse(input: ParseStream) -> Result<Self> {
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

fn no_literal_nested(nested: NestedMeta) -> Result<Meta> {
    match nested {
        NestedMeta::Meta(m) => Ok(m),
        NestedMeta::Lit(l) => Err(Error::new(l.span(), "Unexpected literal"))
    }
}

fn just_word_meta(meta: Meta) -> Result<Ident> {
    let span = meta.span();
    match meta {
        Meta::Path(path)  => path.get_ident().map(|id| id.clone()),
        _ => None
    }.ok_or(Error::new(span, "Should be an ident"))
}

impl Parse for Attribute {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek2(Token![<]) {
            let tag = input.parse()?;
            let _open = input.parse::<Token![<]>()?;
            let inner = input.parse()?;
            let _close = input.parse::<Token![>]>()?;
            Ok(Attribute::Type(tag, inner))
        } else {
            use Meta::*;
            match no_literal_nested(NestedMeta::parse(input)?)? {
                Path(path) => path.get_ident().map(|id| id.clone())
                    .map(Attribute::Attr)
                    .ok_or(Error::new(path.span(), "Invalid attribute")),
                List(l) =>
                    {
                        let ident = l.path.get_ident().map(|id| id.clone());
                        let span = l.span();
                        let tag = l.nested.into_iter()
                            .map(no_literal_nested)
                            .collect::<Result<Vec<Meta>>>()?
                            .into_iter().map(just_word_meta)
                            .collect::<Result<Vec<Ident>>>()?;
                        ident.map(|id| Attribute::Tagged(id, tag))
                        .ok_or(Error::new(span, "Invalid attribute"))
                    },
                NameValue(nv) => Err(Error::new(nv.span(), "Invalid attribute"))
            }
        }
    }
}

fn parse_vector_trailing<T, P>(input: ParseStream) -> Result<Vec<T>>
    where
        T: Parse,
        P: syn::token::Token + Parse
{
    Ok(
        Punctuated::<Option<T>, P>::parse_separated_nonempty_with(
            input, |input_tokens|
                if input_tokens.is_empty() {
                    Ok(None)
                } else {
                    T::parse(input_tokens).map(|inner| Some(inner))
                },
        )?.into_iter()
            .filter_map(|it| it)
            .collect()
    )
}

#[allow(dead_code)]
pub(crate) fn drain_stream(input: ParseStream) {
    // JUST TO SKIP ALL
    let _ = input.step(|cursor| {
        let mut rest = *cursor;
        while let Some((_, next)) = rest.token_tree() {
            rest = next
        };
        Ok(((), rest))
    });
}

#[derive(PartialEq, Debug, Clone)]
pub(crate) struct Fixture {
    pub(crate) name: Ident,
    pub(crate) positional: Vec<syn::Expr>,
}

impl Fixture {
    pub(crate) fn new(name: Ident, positional: Vec<syn::Expr>) -> Self {
        Self { name, positional }
    }
}

impl Parse for Fixture {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.parse()?;
        let content;
        let _ = syn::parenthesized!(content in input);
        let positional = Punctuated::<syn::Expr, Token![,]>::parse_terminated(&content)?
            .into_iter()
            .collect();
        Ok(
            Self::new(name, positional)
        )
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

#[cfg(test)]
mod should {
    use super::*;
    use crate::test::*;

    mod parse_attributes {
        use super::*;
        use super::assert_eq;

        fn parse_attributes<S: AsRef<str>>(attributes: S) -> Attributes {
            parse_meta(attributes)
        }

        #[test]
        fn one_simple_ident() {
            let attributes = parse_attributes("my_ident");

            let expected = Attributes {
                attributes: vec![
                    Attribute::attr("my_ident")
                ]
            };

            assert_eq!(expected, attributes);
        }

        #[test]
        fn one_simple_group() {
            let attributes = parse_attributes("group_tag(first, second)");

            let expected = Attributes {
                attributes: vec![
                    Attribute::tagged("group_tag", vec!["first", "second"])
                ]
            };

            assert_eq!(expected, attributes);
        }

        #[test]
        fn one_simple_type() {
            let attributes = parse_attributes("type_tag<(u32, T, (String, i32))>");

            let expected = Attributes {
                attributes: vec![
                    Attribute::typed("type_tag", "(u32, T, (String, i32))")
                ]
            };

            assert_eq!(expected, attributes);
        }

        #[test]
        fn integrated() {
            let attributes = parse_attributes(r#"
            simple :: tagged(first, second) :: type_tag<(u32, T, (std::string::String, i32))> :: more_tagged(a,b)"#);

            let expected = Attributes {
                attributes: vec![
                    Attribute::attr("simple"),
                    Attribute::tagged("tagged", vec!["first", "second"]),
                    Attribute::typed("type_tag", "(u32, T, (std::string::String, i32))"),
                    Attribute::tagged("more_tagged", vec!["a", "b"]),
                ]
            };

            assert_eq!(expected, attributes);
        }
    }
}

