use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::{
    parse::{Parse, ParseStream, Result},
    Attribute, Expr, ExprLit, Ident, Lit, Meta, MetaNameValue, Pat, Token,
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

/// Extracts the documentation comment from the attributes.
fn extract_doc_comment(attrs: &[Attribute]) -> Option<String> {
    attrs.iter().find_map(|attr| match &attr.meta {
        Meta::NameValue(MetaNameValue {
            path,
            value: Expr::Lit(ExprLit {
                lit: Lit::Str(doc), ..
            }),
            ..
        }) if path.is_ident("doc") => Some(sanitize_ident(&doc.value().trim(), false)),
        _ => None,
    })
}

impl Parse for ValueList {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident: Ident = input.parse()?;
        let _to: Token![=>] = input.parse()?;
        let content;
        let paren = syn::bracketed!(content in input);

        let mut values = Vec::new();
        while !content.is_empty() {
            // Set the description from doc comments if possible
            let attrs: Vec<Attribute> = content.call(Attribute::parse_outer)?;
            let description = extract_doc_comment(&attrs);

            // Parse the expression inside the brackets
            let expr: Expr = content.parse()?;
            values.push(Value::new(expr, description));

            // Consume the trailing comma
            let _ = content.parse::<Token![,]>();
        }

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
                    999
                    2,
                ]
            "#,
            );

            assert_eq!("number", &vl.arg.display_code());
            assert_eq!("forty_two", vl.values[0].description());
            assert_eq!("one_hundred", vl.values[1].description());
            assert_eq!("Sanitized_999_Value_", vl.values[2].description());
            // fallback to the expression name
            assert_eq!("2", vl.values[3].description());
        }
    }
}
