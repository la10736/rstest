use syn::{Ident, Token, parse::{Error, Parse, ParseStream, Result}, punctuated::Punctuated, Expr};

use proc_macro2::TokenStream;
use quote::ToTokens;

#[derive(PartialEq, Debug)]
/// A test case instance data. Contains a list of arguments. It is parsed by parametrize
/// attributes.
pub(crate) struct TestCase {
    pub(crate) args: Vec<Expr>,
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
            let args = Punctuated::<Expr, Token![,]>::parse_terminated(&content)?
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
    #[should_panic]
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

        assert_eq!(
            to_args!(["42", "-42", r#"pippo("pluto")"#, "Vec::new()",
                    r##"String::from(r#"prrr"#)"##,
                    r#"{let mut sum=0;for i in 1..3 {sum += i;}sum}"#,
                    "vec![1,2,3]"
                    ]),
            args);
    }
}
