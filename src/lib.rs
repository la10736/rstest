#![feature(proc_macro, try_from, underscore_lifetimes)]

extern crate proc_macro;
extern crate proc_macro2;
#[macro_use]
extern crate quote;
extern crate syn;

use proc_macro2::TokenStream;
use quote::ToTokens;
use std::convert::TryFrom;
use syn::*;
use syn::buffer::TokenBuffer;

fn attribute(input: &str) -> Attribute {
    let tokens = input.parse::<TokenStream>().unwrap();
    let buf = TokenBuffer::new2(tokens);
    match Attribute::parse_outer(buf.begin()) {
        Ok((e, rest)) => {
            assert!(rest.eof());
            e
        }
        Err(err) => panic!(err),
    }
}


fn parse_meta_list<S: AsRef<str>>(meta: S) -> Option<Vec<NestedMeta>> {
    let meta = format!("#[foo{}]", meta.as_ref().to_string());
    attribute(&meta).interpret_meta().map(
        |m| match m {
            Meta::List(data) => {
                data.nested.into_iter().collect()
            }
            _ => { panic!("Unexpected!") }
        }
    )
}

#[derive(Debug)]
enum RsTestError {
    UnknownParameterOption(String),
    UnknownErr,
}

#[derive(Default, Debug)]
struct ParametrizeInfo {
    args: Vec<Ident>,
    cases: Vec<TestCase>,
}

#[derive(PartialEq, Eq, Debug)]
struct TestCase(Vec<Expr>);

impl<'a> TryFrom<&'a MetaList> for TestCase {
    type Error = RsTestError;

    fn try_from(l: &'a MetaList) -> Result<Self, Self::Error> {
        if l.ident.as_ref() == "case" {
            let res: Result<Vec<_>, _> = l.nested.iter().map(
                |e|
                    parse_str::<Expr>(&format!("{}", e.into_tokens()
                    )).or(Err(RsTestError::UnknownErr))
            ).collect();
            res.map(TestCase)
        } else {
            Err(RsTestError::UnknownParameterOption(l.ident.to_string()))
        }
    }
}

impl<'a, S: AsRef<str>> From<&'a [S]> for TestCase {
    fn from(strings: &[S]) -> Self {
        TestCase(strings
            .iter()
            .map(|s| parse_str(s.as_ref()).unwrap())
            .collect()
        )
    }
}

impl ParametrizeInfo {}

enum ParametrizeElement {
    Arg(Ident),
    Case(TestCase),
}

fn parse_meta_item(meta: Meta) -> Option<ParametrizeElement> {
    use syn::Meta::*;
    use ParametrizeElement::*;
    match meta {
        Word(ident) => Some(Arg(ident)),
        List(ref l) if l.ident.as_ref() == "case" => {
            TestCase::try_from(l).map(Case).ok()
        }
        _ => None
    }
}

fn parse_parametrize_data<S: AsRef<str>>(meta_args: S) -> Result<ParametrizeInfo, RsTestError> {
    let metas = parse_meta_list(meta_args);
    let mut args = vec![];
    let mut cases = vec![];
    use syn::NestedMeta::*;
    use ParametrizeElement::*;
    for meta in metas.unwrap_or_default() {
        match meta {
            Meta(m) => {
                if let Some(item) = parse_meta_item(m) {
                    match item {
                        Arg(arg) => args.push(arg),
                        Case(case) => cases.push(case),
                    }
                }
            }
            _ => {}
        };
    }
    Ok(ParametrizeInfo {
        args,
        cases,
    })
}

fn arg_2_fixture_call_str(arg: &syn::FnArg) -> Option<String> {
    if let &syn::FnArg::Captured(ref a) = arg {
        let declaration = a.pat.clone().into_tokens();
        let name = if let syn::Pat::Ident(ref p) = a.pat {
            p.ident
        } else {
            panic!("Argument should be a identity")
        };
        let t = a.ty.clone().into_tokens();
        Some(format!("let {}: {} = {}();", declaration, t, name))
    } else {
        None
    }
}

fn arg_2_fixture_call(arg: &syn::FnArg) -> Option<syn::Stmt> {
    arg_2_fixture_call_str(arg).and_then(|line| syn::parse_str(&line).ok())
}

#[proc_macro_attribute]
pub fn rstest(_args: proc_macro::TokenStream,
              input: proc_macro::TokenStream)
              -> proc_macro::TokenStream {
    let ast = syn::parse(input.clone()).unwrap();
    if let syn::Item::Fn(ref item_fn) = ast {
        let name = item_fn.ident;
        let inner = item_fn.block.clone();
        let fixtures = item_fn.decl.inputs.iter().filter_map(arg_2_fixture_call).collect::<Vec<_>>();
        let res: quote::Tokens = quote! {
            #[test]
            fn #name() {
                #(#fixtures)*
                #inner
            }
        };
        res.into()
    } else {
        input
    }
}

#[proc_macro_attribute]
pub fn rstest_parametrize(args: proc_macro::TokenStream,
                          input: proc_macro::TokenStream)
                          -> proc_macro::TokenStream {
    let params = parse_parametrize_data(format!("{}", args)).unwrap();
    let mut res = quote::Tokens::default();

    let ast = syn::parse(input).unwrap();
    if let syn::Item::Fn(ref item_fn) = ast {
        let fname = item_fn.ident;
        let inner = item_fn.block.clone();

        for (n, case) in params.cases.iter().enumerate() {
            let args = &params.args;
            let vals = &case.0;
            let name = Ident::from(format!("{}_case_{}", fname, n));
            let tcase = quote! {
            #[test]
            fn #name() {
                #(let #args = #vals; )*
                #inner
            }
        };
            res.append_all(tcase);
        };
    }

    res.into()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_invalid_meta() {
        assert!(parse_meta_list(" aa bb").is_none());
    }

    #[test]
    fn parse_empty_meta() {
        assert!(parse_meta_list("()").unwrap().is_empty());
    }

    fn meta<M: From<Meta>>(name: &str) -> M {
        Meta::Word(name.into()).into()
    }

    #[test]
    fn parse_simple_meta() {
        let expected: Vec<NestedMeta> = vec![meta("first"), meta("second")];

        assert_eq!(expected, parse_meta_list("(first, second)").unwrap());
    }

    #[test]
    fn extract_parametrize_no_names_happy_path() {
        let meta_args = r#"(
            expected, input,
            case(5, "ciao"),
            case(3, "Foo")
        )"#;

        let data = parse_parametrize_data(meta_args).unwrap();

        assert_eq!(&vec![Ident::from("expected"), Ident::from("input")], &data.args);

        let c0: TestCase = ["5", "\"ciao\""].as_ref().into();
        let c1 = ["3", "\"Foo\""].as_ref().into();

        assert_eq!(&vec![c0, c1], &data.cases);
    }

    #[test]
    fn parse_complex_meta() {
        let meta_args = r#"(
            case(4, pippo="pluto", Unwrap("vec![1,3]"), name="my_name")
        )"#;

        let l = parse_meta_list(meta_args).unwrap();

        assert!(!l.is_empty());
    }

    fn fn_args(item: &syn::Item) -> syn::punctuated::Iter<'_, syn::FnArg, syn::token::Comma> {
        if let &syn::Item::Fn(ref item_fn) = item {
            return item_fn.decl.inputs.iter();
        } else {
            panic!("Wrong ast!")
        }
    }

    #[test]
    fn extract_fixture_call_arg() {
        let ast = syn::parse_str("fn foo(fix: String) {}").unwrap();
        let args = fn_args(&ast).next().unwrap();

        let line = arg_2_fixture_call_str(args);

        assert_eq!("let fix: String = fix();", &line.unwrap());
    }

    #[test]
    fn extract_fixture_should_add_mut_too() {
        let ast = syn::parse_str("fn foo(mut fix: String) {}").unwrap();
        let args = fn_args(&ast).next().unwrap();

        let line = arg_2_fixture_call_str(args);

        assert_eq!("let mut fix: String = fix();", &line.unwrap());
    }

//    #[test]
//    fn parametrize_no_name_vec_and_array() {
//        let meta_args = r#"(
//            expected, input,
//            case(4, Unwrap("vec![1,3]")),
//            case(10, Unwrap("[2,3,5]"), name=)
//        )"#;
//
//        let data = parse_parametrize_data(meta_args).unwrap();
//
//        let c0: TestCase = ["4", "vec![1, 3]"].as_ref().into();
//        let c1: TestCase = ["10", "[2,3,5]"].as_ref().into();
//
//        assert_eq!(&vec![Ident::from("expected"), Ident::from("input")],
//                   &data.args);
//        assert_eq!(&vec![c0, c1], &data.cases);
//    }
}

