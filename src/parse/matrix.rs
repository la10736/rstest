use proc_macro2::{Span, TokenStream};
use quote::ToTokens;
use syn::{Ident, parse::{Parse, ParseStream, Result}, Token, Expr};

use crate::refident::RefIdent;

use super::{Attributes, Fixture, parse_vector_trailing_till_double_comma};

#[derive(Default)]
pub(crate) struct MatrixInfo {
    pub(crate) args: MatrixData,
    pub(crate) attributes: Attributes,
}

impl Parse for MatrixInfo {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(
            MatrixInfo {
                args: parse_vector_trailing_till_double_comma::<_, Token![,]>(input)
                    .map(|items| MatrixData { items } )?,
                attributes: input.parse::<Token![::]>()
                    .or_else(|_| Ok(Default::default()))
                    .and_then(|_| input.parse())?,
            }
        )
    }
}

#[derive(Default)]
pub(crate) struct MatrixData { pub(crate) items: Vec<MatrixItem> }

impl MatrixData {
    pub(crate) fn list_values(&self) -> impl Iterator<Item=&ValueList> {
        self.items.iter().filter_map(|mv|
            match mv {
                MatrixItem::ValueList(ref value_list) => Some(value_list),
                _ => None
            }
        )
    }

    pub(crate) fn fixtures(&self) -> impl Iterator<Item=&Fixture> {
        self.items.iter().filter_map(|mv|
            match mv {
                MatrixItem::Fixture(ref fixture) => Some(fixture),
                _ => None
            }
        )
    }
}

pub(crate) enum MatrixItem {
    ValueList(ValueList),
    Fixture(Fixture),
}

impl Parse for MatrixItem {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek2(Token![=>]) {
            input.parse::<ValueList>().map(Self::from)
        } else if input.fork().parse::<Fixture>().is_ok() {
            input.parse::<Fixture>().map(Self::from)
        } else {
            Err(syn::Error::new(Span::call_site(), "Cannot parse matrix info"))
        }
    }
}

impl From<ValueList> for MatrixItem {
    fn from(value_list: ValueList) -> Self {
        MatrixItem::ValueList(value_list)
    }
}

impl From<Fixture> for MatrixItem {
    fn from(fixture: Fixture) -> Self {
        MatrixItem::Fixture(fixture)
    }
}

impl RefIdent for MatrixItem {
    fn ident(&self) -> &Ident {
        match self {
            MatrixItem::ValueList(ref values) => values.ident(),
            MatrixItem::Fixture(ref fixture) => fixture.ident(),
        }
    }
}

impl ToTokens for MatrixItem {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.ident().to_tokens(tokens)
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct ValueList {
    pub(crate) arg: Ident,
    pub(crate) values: Vec<Expr>,
}

impl Parse for ValueList {
    fn parse(input: ParseStream) -> Result<Self> {
        let arg = input.parse()?;
        let _to: Token![=>] = input.parse()?;
        let content;
        let paren = syn::bracketed!(content in input);
        let values = content
            .parse_terminated::<_, Token![,]>(Parse::parse)?
            .into_iter()
            .collect();

        let ret = Self {
            arg,
            values,
        };
        if ret.values.len() == 0 {
            Err(syn::Error::new(paren.span, "Values list should not be empty"))
        } else {
            Ok(ret)
        }
    }
}

impl RefIdent for ValueList {
    fn ident(&self) -> &Ident {
        &self.arg
    }
}

impl ToTokens for ValueList {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.arg.to_tokens(tokens)
    }
}

#[cfg(test)]
mod should {
    use crate::test::{*, assert_eq};

    use super::*;

    mod parse_values_list {
        use super::*;
        use super::assert_eq;

        fn parse_values_list<S: AsRef<str>>(values_list: S) -> ValueList {
            parse_meta(values_list)
        }

        #[test]
        fn some_literals() {
            let literals = literal_expressions_str();
            let name = "argument";

            let values_list = parse_values_list(
                format!(r#"{} => [{}]"#, name,
                        literals
                            .iter()
                            .map(ToString::to_string)
                            .collect::<Vec<String>>()
                            .join(", "))
            );

            assert_eq!(name, &values_list.arg.to_string());
            assert_eq!(values_list.args(), to_args!(literals));
        }

        #[test]
        fn raw_code() {
            let values_list = parse_values_list(r#"no_mater => [vec![1,2,3]]"#);

            assert_eq!(values_list.args(), to_args!(["vec![1, 2, 3]"]));
        }

        #[test]
        #[should_panic]
        fn raw_code_with_parsing_error() {
            parse_values_list(r#"other => [some:<>(1,2,3)]"#);
        }

        #[test]
        #[should_panic(expected = r#"expected square brackets"#)]
        fn forget_brackets() {
            parse_values_list(r#"other => 42"#);
        }
    }

    mod parse_matrix_info {
        use itertools::Itertools;

        use crate::parse::Attribute;

        use super::*;
        use super::assert_eq;

        fn parse_matrix_info<S: AsRef<str>>(matrix_info: S) -> MatrixInfo {
            parse_meta(matrix_info)
        }

        #[test]
        fn happy_path() {
            let info = parse_matrix_info(r#"
            expected => [12, 34 * 2],
            input => [format!("aa_{}", 2), "other"],
        "#);

            let value_ranges = info.args.list_values().collect_vec();
            assert_eq!(2, value_ranges.len());
            assert_eq!(to_args!(["12", "34 * 2"]), value_ranges[0].args());
            assert_eq!(to_args!([r#"format!("aa_{}", 2)"#, r#""other""#]), value_ranges[1].args());
            assert_eq!(info.attributes, Default::default());
        }

        #[test]
        fn should_parse_attributes_too() {
            let info = parse_matrix_info(r#"
                                            a => [12, 24, 42]
                                            ::trace
                                        "#);

            assert_eq!(Attributes { attributes: vec![Attribute::attr("trace")] },
                       info.attributes);
        }

        #[test]
        fn should_parse_injected_fixtures_too() {
            let info = parse_matrix_info(
                r#"
                a => [12, 24, 42],
                fixture_1(42, "foo"),
                fixture_2("bar")
                "#);

            let fixtures = info.args.fixtures().cloned().collect_vec();

            assert_eq!(vec![fixture("fixture_1", vec!["42", r#""foo""#]),
                            fixture("fixture_2", vec![r#""bar""#])],
                       fixtures);
        }

        #[test]
        #[should_panic(expected = "should not be empty")]
        fn should_not_compile_if_empty_expression_slice() {
            parse_matrix_info(r#"
                invalid => []
                "#);
        }
    }
}
