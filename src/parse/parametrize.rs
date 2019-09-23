use syn::{Ident, Token,
          parse::{Error, Parse, ParseStream, Result},
          punctuated::Punctuated};

use super::{Fixture, Attributes, CaseArg, parse_vector_trailing};

use proc_macro2::{Span, TokenStream};
use quote::ToTokens;
use crate::refident::{RefIdent, MaybeIdent};

#[derive(Default, Debug)]
/// Parametrize
pub(crate) struct ParametrizeInfo {
    pub(crate) data: ParametrizeData,
    pub(crate) attributes: Attributes,
}

impl Parse for ParametrizeInfo {
    fn parse(input: ParseStream) -> Result<Self> {
        // Parse a `ParametrizeData` and then look for `Modifiers` after `::` token if any.
        // Example
        //  |-    u,a,d
        //  |-    case(42, r("A{}"), Unwrap("D{}"))
        //  |  ::trace::notrace(a))
        //  |    ^^^^^^^^^^^^^^^^^^ Modifiers
        //  |______________________ ParametrizeData
        Ok(
            ParametrizeInfo {
                data: input.parse()?,
                attributes: input.parse::<Token![::]>()
                    .or_else(|_| Ok(Default::default()))
                    .and_then(|_| input.parse())?,
            }
        )
    }
}

#[derive(Default, Debug)]
pub(crate) struct ParametrizeData {
    pub(crate) data: Vec<ParametrizeItem>,
}

impl Parse for ParametrizeData {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(ParametrizeData { data: parse_vector_trailing::<_, Token![,]>(input)? })
    }
}

impl ParametrizeData {
    pub(crate) fn args(&self) -> impl Iterator<Item=&Ident> {
        self.data.iter()
            .filter_map(|it|
                match it {
                    ParametrizeItem::CaseArgName(ref arg) => Some(arg),
                    _ => None
                }
            )
    }

    pub(crate) fn cases(&self) -> impl Iterator<Item=&TestCase> {
        self.data.iter()
            .filter_map(|it|
                match it {
                    ParametrizeItem::TestCase(ref case) => Some(case),
                    _ => None
                }
            )
    }

    pub(crate) fn fixtures(&self) -> impl Iterator<Item=&Fixture> {
        self.data.iter()
            .filter_map(|it|
                match it {
                    ParametrizeItem::Fixture(ref fixture) => Some(fixture),
                    _ => None
                }
            )
    }
}

#[derive(Debug)]
pub(crate) enum ParametrizeItem {
    Fixture(Fixture),
    CaseArgName(Ident),
    TestCase(TestCase),
}

impl Parse for ParametrizeItem {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.fork().parse::<TestCase>().is_ok() {
            input.parse::<TestCase>().map(ParametrizeItem::TestCase)
        } else if input.fork().parse::<Fixture>().is_ok() {
            input.parse::<Fixture>().map(ParametrizeItem::Fixture)
        } else if input.fork().parse::<Ident>().is_ok() {
            input.parse::<Ident>().map(ParametrizeItem::CaseArgName)
        } else {
            Err(syn::Error::new(Span::call_site(), "Cannot parse parametrize info"))
        }
    }
}

impl MaybeIdent for ParametrizeItem {
    fn maybe_ident(&self) -> Option<&Ident> {
        match self {
            ParametrizeItem::Fixture( ref fixture) => Some(fixture.ident()),
            ParametrizeItem::CaseArgName(ref arg) => Some(arg),
            _ => None,
        }
    }
}

impl ToTokens for ParametrizeItem {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.maybe_ident().map(|ident| ident.to_tokens(tokens));
    }
}

#[derive(Debug)]
/// A test case instance data. Contains a list of arguments. It is parsed by parametrize
/// attributes.
pub(crate) struct TestCase {
    pub(crate) args: Vec<CaseArg>,
    pub(crate) description: Option<Ident>,
}

impl Parse for TestCase {
    fn parse(input: ParseStream) -> Result<Self> {
        let case: Ident = input.parse()?;
        if case == "case" {
            let mut description = None;
            if input.peek(Token![::]) {
                let _ = input.parse::<Token![::]>();
                description = Some(input.parse()?);
            }
            let content;
            let _ = syn::parenthesized!(content in input);
            let args = Punctuated::<CaseArg, Token![,]>::parse_terminated(&content)?
                .into_iter()
                .collect();
            Ok(TestCase { args, description })
        } else {
            Err(Error::new(case.span(), "expected a test case"))
        }
    }
}

impl ToTokens for TestCase {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.args.iter().for_each(|c| c.to_tokens(tokens))
    }
}

#[cfg(test)]
mod should {
    use super::*;
    use crate::test::{*, assert_eq};

    mod parse_test_case {
        use super::{*, assert_eq};

        fn parse_test_case<S: AsRef<str>>(test_case: S) -> TestCase {
            parse_meta(test_case)
        }

        #[test]
        fn two_literal_args() {
            let test_case = parse_test_case(r#"case(42, "value")"#);
            let args = test_case.args();

            let expected = to_args!(["42", r#""value""#]);

            assert_eq!(expected, args);
        }

        #[test]
        fn some_literals() {
            let args_expressions = literal_expressions_str();
            let test_case = parse_test_case(&format!("case({})", args_expressions.join(", ")));
            let args = test_case.args();

            assert_eq!(to_args!(args_expressions), args);
        }

        #[test]
        fn raw_code() {
            let test_case = parse_test_case(r#"case(vec![1,2,3])"#);
            let args = test_case.args();

            assert_eq!(to_args!(["vec![1, 2, 3]"]), args);
        }

        #[test]
        #[should_panic(expected = r#"Cannot parse due"#)]
        fn raw_code_with_parsing_error() {
            parse_test_case(r#"case(some:<>(1,2,3))"#);
        }

        #[test]
        fn should_read_test_description_if_any() {
            let test_case = parse_test_case(r#"case::this_test_description(42)"#);
            let args = test_case.args();

            assert_eq!("this_test_description", &test_case.description.unwrap().to_string());
            assert_eq!(to_args!(["42"]), args);
        }

        #[test]
        fn should_read_test_description_also_with_more_args() {
            let test_case = parse_test_case(r#"case :: this_test_description (42, 24)"#);
            let args = test_case.args();

            assert_eq!("this_test_description", &test_case.description.unwrap().to_string());
            assert_eq!(to_args!(["42", "24"]), args);
        }

        #[test]
        fn should_parse_arbitrary_rust_code_as_expression() {
            let test_case = parse_test_case(r##"
        case(42, -42,
        pippo("pluto"),
        Vec::new(),
        String::from(r#"prrr"#),
        {
            let mut sum=0;
            for i in 1..3 {
                sum += i;
            }
            sum
        },
        vec![1,2,3]
        )"##);
            let args = test_case.args();

            assert_eq!(to_args!(["42", "-42", r#"pippo("pluto")"#, "Vec::new()",
        r##"String::from(r#"prrr"#)"##, r#"{let mut sum=0;for i in 1..3 {sum += i;}sum}"#,
        "vec![1,2,3]"]), args);
        }
    }

    mod parse_parametrize {
        use super::{*, assert_eq};

        fn parse_data<S: AsRef<str>>(test_case: S) -> ParametrizeData {
            parse_meta(test_case)
        }

        #[test]
        fn one_simple_case_one_arg() {
            let data = parse_data(r#"arg, case(42)"#);

            let args = data.args().collect::<Vec<_>>();
            let cases = data.cases().collect::<Vec<_>>();

            assert_eq!(1, args.len());
            assert_eq!(1, cases.len());
            assert_eq!("arg", &args[0].to_string());
            assert_eq!(to_args!(["42"]), cases[0].args())
        }

        #[test]
        fn happy_path() {
            let data = parse_data(r#"
            my_fixture(42,"foo"),
            arg1, arg2, arg3,
            case(1,2,3),
            case(11,12,13),
            case(21,22,23)
            "#);

            let fixtures = data.fixtures()
                .cloned()
                .collect::<Vec<_>>();
            assert_eq!(vec![fixture("my_fixture", vec!["42", r#""foo""#])], fixtures);
            assert_eq!(to_strs!(vec!["arg1", "arg2", "arg3"]), data.args()
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
            let data = parse_data(r#"
            arg,
            case(42),
            "#);

            let args = data.args().collect::<Vec<_>>();
            let cases = data.cases().collect::<Vec<_>>();

            assert_eq!(1, args.len());
            assert_eq!(1, cases.len());
            assert_eq!("arg", &args[0].to_string());
            assert_eq!(to_args!(["42"]), cases[0].args())
        }

        #[test]
        fn integrated_1() {
            let data = parse_data(r#"
            u,a,d,
            case(42, A{}, D{})
            "#);

            assert_eq!(to_strs!(vec!["u", "a", "d"]), data.args()
                .map(ToString::to_string)
                .collect::<Vec<_>>());
            let cases = data.cases().collect::<Vec<_>>();
            assert_eq!(1, cases.len());
            assert_eq!(to_args!(["42", "A{}", "D{}"]), cases[0].args());
        }

        #[test]
        fn integrated_2() {
            let data = parse_data(r#"
            ret,
            case::should_success(Ok(())),
            case::should_fail(Err("Return Error"))
            "#);

            assert_eq!(to_strs!(vec!["ret"]), data.args()
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
            parse_data(r#"
            ret
            case::should_success(Ok(())),
            case::should_fail(Err("Return Error"))
            "#);
        }

        #[test]
        fn should_accept_any_order() {
            let data = parse_data(r#"
            u,
            case(42, A{}, D{}),
            a,
            case(43, A{}, D{}),
            the_fixture(42),
            d
            "#);

            assert_eq!(to_strs!(vec!["u", "a", "d"]), data.args()
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
            let data = parse_data(r#"
            case,
            case(42)
            "#);

            assert_eq!("case", &data.args().next().unwrap().to_string());
            let cases = data.cases().collect::<Vec<_>>();
            assert_eq!(1, cases.len());
            assert_eq!(to_args!(["42"]), cases[0].args());
        }
    }
}
