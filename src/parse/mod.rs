use proc_macro2::{Span, TokenStream};
use syn::{Expr, Ident, Lit, LitStr, Meta, MetaList, NestedMeta, Token,
          parse::{Error, Parse, ParseStream, Result},
          punctuated::Punctuated,
          spanned::Spanned};

use cfg_if::cfg_if;
use quote::ToTokens;

// To use the macros this should be the first one module
#[macro_use]
pub(crate) mod macros;

pub(crate) mod fixture;
pub(crate) mod rstest;
pub(crate) mod parametrize;

#[derive(Debug, Clone)]
/// A test case's argument as an expression that can be assigned.
pub struct CaseArg {
    expr: Expr,
}

impl CaseArg {
    pub fn new(expr: Expr) -> Self {
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
            .map(|m| *m.value())
            .and_then(Self::nested_meta_literal_str)
            .ok_or(syn::Error::new_spanned(&nested,
                                           &format!("Invalid {} argument", arg.ident)))
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
            NestedMeta::Meta(Meta::List(ref arg)) if arg.ident == Self::UNWRAP_NAME => Ok(arg),
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
                            .map(|m| *m.value())
                            .and_then(UnwrapRustCode::nested_meta_literal_str)
                            .map(|content| {
                                eprintln!(r#"{}("<code>") is deprecated. Case argument accepts arbitrary rust code now."#,
                                          arg.ident);
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
            NestedMeta::Literal(Lit::Str(lit)) => Some(lit),
            _ => None
        }
    }
}

#[derive(Default, Debug, PartialEq)]
pub struct Attributes {
    pub attributes: Vec<Attribute>
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
pub enum Attribute {
    Attr(Ident),
    Tagged(Ident, Vec<Ident>),
    Type(Ident, syn::Type),
}

fn no_literal_nested(nested: NestedMeta) -> Result<Meta> {
    match nested {
        NestedMeta::Meta(m) => Ok(m),
        NestedMeta::Literal(l) => Err(Error::new(l.span(), "Unexpected literal"))
    }
}

fn just_word_meta(meta: Meta) -> Result<Ident> {
    match meta {
        Meta::Word(ident) => Ok(ident),
        other => Err(Error::new(other.span(), "Should be an ident"))
    }
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
                Word(ident) => Ok(Attribute::Attr(ident)),
                List(l) =>
                    Ok(Attribute::Tagged(l.ident,
                                         l.nested.into_iter()
                                                   .map(no_literal_nested)
                                                   .collect::<Result<Vec<Meta>>>()?
                                                   .into_iter().map(just_word_meta)
                                                   .collect::<Result<Vec<Ident>>>()?,
                    )),
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

pub struct ValueList {
    pub arg: Ident,
    pub values: Vec<CaseArg>,
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

pub enum MatrixItem {
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

#[derive(Default)]
pub struct MatrixValues(pub Vec<MatrixItem>);

impl MatrixValues {
    pub fn list_values(&self) -> impl Iterator<Item=&ValueList> {
        self.0.iter().filter_map(|mv|
            match mv {
                MatrixItem::ValueList(ref value_list) => Some(value_list),
                _ => None
            }
        )
    }

    pub fn fixtures(&self) -> impl Iterator<Item=&Fixture> {
        self.0.iter().filter_map(|mv|
            match mv {
                MatrixItem::Fixture(ref fixture) => Some(fixture),
                _ => None
            }
        )
    }
}

#[derive(Default)]
pub struct MatrixInfo {
    pub args: MatrixValues,
    pub attributes: Attributes,
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

impl Parse for MatrixInfo {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(
            MatrixInfo {
                args: parse_vector_trailing::<_, Token![,]>(input)
                    .map(MatrixValues)?,
                attributes: input.parse::<Token![::]>()
                    .or_else(|_| Ok(Default::default()))
                    .and_then(|_| input.parse())?,
            }
        )
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Fixture {
    pub name: Ident,
    pub positional: Vec<syn::Expr>,
}

impl Fixture {
    pub fn new(name: Ident, positional: Vec<syn::Expr>) -> Self {
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

#[cfg(test)]
mod should {
    use super::*;
    use crate::test::*;

    mod parse_fixture {
        use super::*;
        use super::assert_eq;
        use super::super::fixture::FixtureInfo;

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
        #[should_panic(expected = r#"Cannot parse due"#)]
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
        use super::*;
        use super::assert_eq;
        use itertools::Itertools;

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
            let info = parse_matrix_info(r#"
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

