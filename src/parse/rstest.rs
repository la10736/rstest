use syn::{Ident, Token,
          parse::{Parse, ParseStream, Result},
          };

use super::{Fixture, parse_vector_trailing};
use crate::parse::RsTestAttributes;

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
                #[allow(unreachable_patterns)]
                _ => None
            }
        )
    }
}

impl Parse for RsTestData {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(Token![::]) {
            Ok(Default::default())
        } else {
            Ok(Self { items: parse_vector_trailing::<_, Token![,]>(input)? })
        }
    }
}

#[derive(PartialEq, Debug)]
pub(crate) enum RsTestItem {
    Fixture(Fixture)
}

impl RsTestItem {
    pub(crate) fn name(&self) -> &Ident {
        match self {
            RsTestItem::Fixture(Fixture { ref name, .. }) => name
        }
    }
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
