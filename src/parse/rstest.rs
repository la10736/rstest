use syn::{Ident, Token,
          parse::{Parse, ParseStream, Result},
          };

use super::{Fixture, Attribute, Attributes, parse_vector_trailing_till_double_comma};
use crate::refident::RefIdent;
use quote::ToTokens;
use proc_macro2::TokenStream;

#[derive(PartialEq, Debug, Default)]
pub(crate) struct RsTestInfo {
    pub(crate) data: RsTestData,
    pub(crate) attributes: RsTestAttributes,
}

impl Parse for RsTestInfo {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(
            if input.is_empty() {
                Default::default()
            } else {
                Self {
                    data: input.parse()?,
                    attributes: input.parse::<Token![::]>()
                        .or_else(|_| Ok(Default::default()))
                        .and_then(|_| input.parse())?,
                }
            }
        )
    }
}

#[derive(PartialEq, Debug, Default)]
pub(crate) struct RsTestData {
    pub(crate) items: Vec<RsTestItem>
}

impl RsTestData {
    pub(crate) fn fixtures(&self) -> impl Iterator<Item=&Fixture> {
        self.items.iter().filter_map(|it|
            match it {
                RsTestItem::Fixture(ref fixture) => Some(fixture),
            }
        )
    }
}

impl Parse for RsTestData {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(Token![::]) {
            Ok(Default::default())
        } else {
            Ok(Self { items: parse_vector_trailing_till_double_comma::<_, Token![,]>(input)? })
        }
    }
}

#[derive(PartialEq, Debug)]
pub(crate) enum RsTestItem {
    Fixture(Fixture)
}

impl From<Fixture> for RsTestItem {
    fn from(f: Fixture) -> Self {
        RsTestItem::Fixture(f)
    }
}

impl Parse for RsTestItem {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse().map(RsTestItem::Fixture)
    }
}

impl RefIdent for RsTestItem {
    fn ident(&self) -> &Ident {
        match self {
            RsTestItem::Fixture(ref fixture) => fixture.ident()
        }
    }
}

impl ToTokens for RsTestItem {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.ident().to_tokens(tokens)
    }
}

wrap_attributes!(RsTestAttributes);

impl RsTestAttributes {
    const TRACE_VARIABLE_ATTR: &'static str = "trace";
    const NOTRACE_VARIABLE_ATTR: &'static str = "notrace";

    pub(crate) fn trace_me(&self, ident: &Ident) -> bool {
        if self.should_trace() {
            self.iter()
                .filter(|&m|
                    Self::is_notrace(ident, m)
                ).next().is_none()
        } else { false }
    }

    fn is_notrace(ident: &Ident, m: &Attribute) -> bool {
        match m {
            Attribute::Tagged(i, args) if i == Self::NOTRACE_VARIABLE_ATTR =>
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

    fn is_trace(m: &Attribute) -> bool {
        match m {
            Attribute::Attr(i) if i == Self::TRACE_VARIABLE_ATTR => true,
            _ => false
        }
    }
}

impl Parse for RsTestAttributes {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(input.parse::<Attributes>()?.into())
    }
}

#[cfg(test)]
mod should {
    use crate::test::*;
    use super::*;

    mod parse_rstest_data {
        use super::*;
        use super::assert_eq;

        fn parse_rstest_data<S: AsRef<str>>(fixtures: S) -> RsTestData {
            parse_meta(fixtures)
        }

        #[test]
        fn one_arg() {
            let fixtures = parse_rstest_data("my_fixture(42)");

            let expected = RsTestData {
                items: vec![
                    Fixture::new(ident("my_fixture"), vec![expr("42")]).into()
                ]
            };

            assert_eq!(expected, fixtures);
        }
    }

    mod parse_rstest {
        use super::{*, assert_eq};
        use crate::parse::{Attributes, Attribute};

        fn parse_rstest<S: AsRef<str>>(rstest_data: S) -> RsTestInfo {
            parse_meta(rstest_data)
        }

        #[test]
        fn happy_path() {
            let data = parse_rstest(r#"my_fixture(42, "other"), other(vec![42])
                :: trace :: no_trace(some)"#);

            let expected = RsTestInfo {
                data: vec![
                    fixture("my_fixture", vec!["42", r#""other""#]).into(),
                    fixture("other", vec!["vec![42]"]).into(),
                ].into(),
                attributes: Attributes {
                    attributes: vec![
                        Attribute::attr("trace"),
                        Attribute::tagged("no_trace", vec!["some"])
                    ]
                }.into(),
            };

            assert_eq!(expected, data);
        }

        #[test]
        fn empty_fixtures() {
            let data = parse_rstest(r#"::trace::no_trace(some)"#);

            let expected = RsTestInfo {
                attributes: Attributes {
                    attributes: vec![
                        Attribute::attr("trace"),
                        Attribute::tagged("no_trace", vec!["some"])
                    ]
                }.into(),
                ..Default::default()
            };

            assert_eq!(expected, data);
        }

        #[test]
        fn empty_attributes() {
            let data = parse_rstest(r#"my_fixture(42, "other")"#);

            let expected = RsTestInfo {
                data: vec![
                    fixture("my_fixture", vec!["42", r#""other""#]).into(),
                ].into(),
                ..Default::default()
            };

            assert_eq!(expected, data);
        }
    }
}
