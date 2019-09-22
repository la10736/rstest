/// `fixture`'s related data and parsing

use syn::{Ident, Token, parse_quote,
          parse::{Parse, ParseStream, Result},
          };

use super::{Fixture, Attributes, parse_vector_trailing};
use crate::parse::Attribute;

#[derive(PartialEq, Debug, Default)]
pub(crate) struct FixtureInfo {
    pub(crate) data: FixtureData,
    pub(crate) attributes: FixtureModifiers,
}

impl Parse for FixtureModifiers {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(input.parse::<Attributes>()?.into())
    }
}

impl Parse for FixtureInfo {
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
pub(crate) struct FixtureData {
    pub items: Vec<FixtureItem>
}

impl FixtureData {
    pub(crate) fn fixtures(&self) -> impl Iterator<Item=&Fixture> {
        self.items.iter()
            .filter_map(|f|
                match f {
                    FixtureItem::Fixture(ref fixture) => Some(fixture),
                    #[allow(unreachable_patterns)]
                    _ => None,
                }
            )
    }
}

impl Parse for FixtureData {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(Token![::]) {
            Ok(Default::default())
        } else {
            Ok(Self { items: parse_vector_trailing::<_, Token![,]>(input)? })
        }
    }
}

#[derive(PartialEq, Debug)]
pub(crate) enum FixtureItem {
    Fixture(Fixture)
}

impl FixtureItem {
    pub(crate) fn name(&self) -> &Ident {
        match self {
            FixtureItem::Fixture(Fixture { ref name, .. }) => name
        }
    }
}

impl From<Fixture> for FixtureItem {
    fn from(f: Fixture) -> Self {
        FixtureItem::Fixture(f)
    }
}

impl Parse for FixtureItem {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse().map(FixtureItem::Fixture)
    }
}

wrap_attributes!(FixtureModifiers);

impl FixtureModifiers {
    const DEFAULT_RET_ATTR: &'static str = "default";

    pub(crate) fn extract_default_type(self) -> Option<syn::ReturnType> {
        self.iter()
            .filter_map(|m|
                match m {
                    Attribute::Type(name, t) if name == Self::DEFAULT_RET_ATTR =>
                        Some(parse_quote!{ -> #t}),
                    _ => None
                })
            .next()
    }
}


#[cfg(test)]
mod should_understand_attributes {
    use crate::test::{*, assert_eq};
    use super::FixtureInfo;
    use crate::parse::{Attributes, Attribute};

    fn parse_fixture<S: AsRef<str>>(fixture_data: S) -> FixtureInfo {
        parse_meta(fixture_data)
    }

    #[test]
    fn happy_path() {
        let data = parse_fixture(r#"my_fixture(42, "other"), other(vec![42])
            :: trace :: no_trace(some)"#);

        let expected = FixtureInfo {
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
        let data = parse_fixture(r#"::trace::no_trace(some)"#);

        let expected = FixtureInfo {
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
        let data = parse_fixture(r#"my_fixture(42, "other")"#);

        let expected = FixtureInfo {
            data: vec![
                fixture("my_fixture", vec!["42", r#""other""#]).into(),
            ].into(),
            ..Default::default()
        };

        assert_eq!(expected, data);
    }

    #[test]
    fn should_accept_trailing_comma() {
        let fixtures = vec![
            parse_fixture(r#"first(42),"#),
            // See #52
            //    parse_fixture(r#"fixture(42, "other"), :: trace"#),
        ];

        for f in fixtures {
            assert_eq!(1, f.data.fixtures().count());
        }
    }
}
