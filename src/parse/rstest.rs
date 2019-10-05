use syn::{Ident, Token,
          parse::{Parse, ParseStream, Result},
          };

use super::{Fixture, Attribute, Attributes, parse_vector_trailing_till_double_comma};
use super::parametrize::TestCase;
use crate::refident::{RefIdent, MaybeIdent};
use quote::ToTokens;
use proc_macro2::{TokenStream, Span};

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
    pub(crate) fn case_args(&self) -> impl Iterator<Item=&Ident> {
        self.items.iter()
            .filter_map(|it|
                match it {
                    RsTestItem::CaseArgName(ref arg) => Some(arg),
                    _ => None
                }
            )
    }

    pub(crate) fn cases(&self) -> impl Iterator<Item=&TestCase> {
        self.items.iter()
            .filter_map(|it|
                match it {
                    RsTestItem::TestCase(ref case) => Some(case),
                    _ => None
                }
            )
    }

    pub(crate) fn fixtures(&self) -> impl Iterator<Item=&Fixture> {
        self.items.iter().filter_map(|it|
            match it {
                RsTestItem::Fixture(ref fixture) => Some(fixture),
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
            Ok(Self { items: parse_vector_trailing_till_double_comma::<_, Token![,]>(input)? })
        }
    }
}

#[derive(PartialEq, Debug)]
pub(crate) enum RsTestItem {
    Fixture(Fixture),
    CaseArgName(Ident),
    TestCase(TestCase),
}

impl From<Fixture> for RsTestItem {
    fn from(f: Fixture) -> Self {
        RsTestItem::Fixture(f)
    }
}

impl Parse for RsTestItem {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.fork().parse::<TestCase>().is_ok() {
            input.parse::<TestCase>().map(RsTestItem::TestCase)
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
            _ => None
        }
    }
}

impl ToTokens for RsTestItem {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        use RsTestItem::*;
        match self {
            Fixture(ref fixture) => fixture.to_tokens(tokens),
            CaseArgName(ref case_arg) => case_arg.to_tokens(tokens),
            TestCase(ref case) => case.to_tokens(tokens)
        }
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

        fn parse_rstest<S: AsRef<str>>(rstest_data: S) -> RsTestInfo {
            parse_meta(rstest_data)
        }

        mod no_cases {
            use super::{*, assert_eq};
            use crate::parse::{Attributes, Attribute};

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

        mod parametrize_cases {
            use super::{*, assert_eq};

            #[test]
            fn one_simple_case_one_arg() {
                let data = parse_rstest(r#"arg, case(42)"#).data;

                let args = data.case_args().collect::<Vec<_>>();
                let cases = data.cases().collect::<Vec<_>>();

                assert_eq!(1, args.len());
                assert_eq!(1, cases.len());
                assert_eq!("arg", &args[0].to_string());
                assert_eq!(to_args!(["42"]), cases[0].args())
            }

            #[test]
            fn happy_path() {
                let info = parse_rstest(r#"
                    my_fixture(42,"foo"),
                    arg1, arg2, arg3,
                    case(1,2,3),
                    case(11,12,13),
                    case(21,22,23)
                "#);

                let data = info.data;
                let fixtures = data.fixtures()
                    .cloned()
                    .collect::<Vec<_>>();

                assert_eq!(vec![fixture("my_fixture", vec!["42", r#""foo""#])], fixtures);
                assert_eq!(to_strs!(vec!["arg1", "arg2", "arg3"]), data.case_args()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>());

                let cases = data.cases().collect::<Vec<_>>();

                assert_eq!(3, cases.len());
                assert_eq!(to_args!(["1", "2", "3"]), cases[0].args());
                assert_eq!(to_args!(["11", "12", "13"]), cases[1].args());
                assert_eq!(to_args!(["21", "22", "23"]), cases[2].args());
            }

            #[test]
            fn should_accept_comma_at_the_end_of_cases() {
                let data = parse_rstest(r#"
                    arg,
                    case(42),
                "#).data;

                let args = data.case_args().collect::<Vec<_>>();
                let cases = data.cases().collect::<Vec<_>>();

                assert_eq!(1, args.len());
                assert_eq!(1, cases.len());
                assert_eq!("arg", &args[0].to_string());
                assert_eq!(to_args!(["42"]), cases[0].args())
            }

            #[test]
            fn integrated_1() {
                let data = parse_rstest(r#"
                    u,a,d,
                    case(42, A{}, D{})
                "#).data;

                assert_eq!(to_strs!(vec!["u", "a", "d"]), data.case_args()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>());

                let cases = data.cases().collect::<Vec<_>>();

                assert_eq!(1, cases.len());
                assert_eq!(to_args!(["42", "A{}", "D{}"]), cases[0].args());
            }

            #[test]
            fn integrated_2() {
                let data = parse_rstest(r#"
                    ret,
                    case::should_success(Ok(())),
                    case::should_fail(Err("Return Error"))
                "#).data;

                assert_eq!(to_strs!(vec!["ret"]), data.case_args()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>());

                let cases = data.cases().collect::<Vec<_>>();

                assert_eq!(2, cases.len());
                assert_eq!(to_args!(["Ok(())"]), cases[0].args());
                assert_eq!("should_success", &cases[0].description.as_ref().unwrap().to_string());
                assert_eq!(to_args!([r#"Err("Return Error")"#]), cases[1].args());
                assert_eq!("should_fail", &cases[1].description.as_ref().unwrap().to_string());
            }

            #[test]
            #[should_panic]
            fn should_not_accept_invalid_separator_from_args_and_cases() {
                parse_rstest(r#"
                    ret
                    case::should_success(Ok(())),
                    case::should_fail(Err("Return Error"))
                "#);
                }

            #[test]
            fn should_accept_any_order() {
                let data = parse_rstest(r#"
                    u,
                    case(42, A{}, D{}),
                    a,
                    case(43, A{}, D{}),
                    the_fixture(42),
                    d
                "#).data;

                assert_eq!(to_strs!(vec!["u", "a", "d"]), data.case_args()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>());

                let cases = data.cases().collect::<Vec<_>>();

                assert_eq!(2, cases.len());
                assert_eq!(to_args!(["42", "A{}", "D{}"]), cases[0].args());
                assert_eq!(to_args!(["43", "A{}", "D{}"]), cases[1].args());

                let fixtures = data.fixtures().cloned().collect::<Vec<_>>();

                assert_eq!(vec![fixture("the_fixture", vec!["42"])], fixtures);
            }

            #[test]
            fn case_could_be_arg_name() {
                let data = parse_rstest(r#"
                    case,
                    case(42)
                "#).data;

                assert_eq!("case", &data.case_args().next().unwrap().to_string());

                let cases = data.cases().collect::<Vec<_>>();

                assert_eq!(1, cases.len());
                assert_eq!(to_args!(["42"]), cases[0].args());
            }
        }
    }
}
