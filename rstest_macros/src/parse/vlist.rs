use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::{
    parse::{Parse, ParseStream, Result},
    punctuated::Punctuated,
    Attribute, Expr, ExprLit, Ident, Lit, Pat, Token,
};

use crate::{refident::IntoPat, utils::sanitize_ident};

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Value {
    pub(crate) expr: Expr,
    pub(crate) description: Option<String>,
}

impl Value {
    pub(crate) fn new(expr: Expr, description: Option<String>) -> Self {
        Self { expr, description }
    }

    pub(crate) fn description(&self) -> String {
        self.description
            .clone()
            .unwrap_or_else(|| self.expr.to_token_stream().to_string())
    }
}

impl From<Expr> for Value {
    fn from(expr: Expr) -> Self {
        Self::new(expr, None)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct ValueList {
    pub(crate) arg: Pat,
    pub(crate) values: Vec<Value>,
}

/// A single value in a matrix values list, possibly with attributes.
#[derive(Debug)]
pub(crate) struct MatrixValue(Value);

impl MatrixValue {
    fn into_value(self) -> Value {
        self.0
    }
}

/// Extracts a joined documentation comment from the attributes.
fn extract_doc_comment(attrs: &[Attribute]) -> Option<String> {
    let comment = attrs
        .iter()
        .filter_map(|attr| {
            let meta = attr.meta.require_name_value().ok()?;
            if !meta.path.is_ident("doc") {
                return None;
            }

            match &meta.value {
                Expr::Lit(ExprLit {
                    lit: Lit::Str(doc), ..
                }) => {
                    let trimmed = doc.value();
                    let trimmed = trimmed.trim();
                    if trimmed.is_empty() {
                        None
                    } else {
                        Some(trimmed.to_owned())
                    }
                }
                _ => None,
            }
        })
        .collect::<Vec<_>>();

    if comment.is_empty() {
        None
    } else {
        let comment = comment.join(" ").replace(' ', "_");
        Some(sanitize_ident(&comment))
    }
}

impl Parse for MatrixValue {
    fn parse(input: ParseStream) -> Result<Self> {
        let attrs: Vec<Attribute> = input.call(Attribute::parse_outer)?;
        let expr: Expr = input.parse()?;
        Ok(MatrixValue(Value::new(expr, extract_doc_comment(&attrs))))
    }
}

/// A list of matrix values.
pub(crate) struct MatrixValues(Vec<Value>);

impl MatrixValues {
    pub(crate) fn into_values(self) -> Vec<Value> {
        self.0
    }
}

impl Parse for MatrixValues {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self(
            input
                .parse_terminated(MatrixValue::parse, Token![,])?
                .into_iter()
                .map(MatrixValue::into_value)
                .collect(),
        ))
    }
}

impl Parse for ValueList {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident: Ident = input.parse()?;
        let _to: Token![=>] = input.parse()?;
        let content;
        let paren = syn::bracketed!(content in input);

        let values = Punctuated::<MatrixValue, Token![,]>::parse_terminated(&content)?
            .into_iter()
            .map(MatrixValue::into_value)
            .collect::<Vec<_>>();

        if values.is_empty() {
            Err(syn::Error::new(
                paren.span.join(),
                "Values list should not be empty",
            ))
        } else {
            Ok(Self {
                arg: ident.into_pat(),
                values,
            })
        }
    }
}

impl ToTokens for ValueList {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.arg.to_tokens(tokens)
    }
}

#[cfg(test)]
mod should {
    use crate::test::{assert_eq, *};

    use super::*;
    use quote::ToTokens;

    mod parse_values_list {
        use super::assert_eq;
        use super::*;

        fn parse_values_list<S: AsRef<str>>(values_list: S) -> ValueList {
            parse_meta(values_list)
        }

        #[test]
        fn some_literals() {
            let literals = literal_expressions_str();
            let name = "argument";

            let values_list = parse_values_list(format!(
                r#"{} => [{}]"#,
                name,
                literals
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<String>>()
                    .join(", ")
            ));

            assert_eq!(name, &values_list.arg.display_code());
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

        #[test]
        fn doc_comment_overrides_description() {
            let vl = parse_values_list(
                r#"
                number => [
                    /// forty two
                    42,
                    /// one hundred
                    100,
                    /// Sanitized 999 Value 😁
                    999,
                    2,
                ]
            "#,
            );

            assert_eq!("number", &vl.arg.display_code());
            assert_eq!("forty_two", vl.values[0].description());
            assert_eq!("one_hundred", vl.values[1].description());
            assert_eq!("Sanitized_999_Value_", vl.values[2].description());

            assert_eq!("2", vl.values[3].description());
            assert_eq!("42", vl.values[0].expr.to_token_stream().to_string());
            assert_eq!("100", vl.values[1].expr.to_token_stream().to_string());
            assert_eq!("999", vl.values[2].expr.to_token_stream().to_string());
            assert_eq!("2", vl.values[3].expr.to_token_stream().to_string());
        }

        #[test]
        #[should_panic]
        fn should_reject_values_without_commas() {
            parse_values_list(
                r#"
                broken => [
                    1
                    2
                ]
            "#,
            );
        }
    }
}
