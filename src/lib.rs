#![feature(proc_macro, try_from, underscore_lifetimes, match_default_bindings,
conservative_impl_trait)]

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

fn parse_expression<S: AsRef<str>>(s: S) -> Result<Expr, RsTestError> {
    parse_str::<Expr>(s.as_ref())
        .or(Err(RsTestError::UnknownErr))
}

fn parse_case_arg(a: &syn::NestedMeta) -> Result<Expr, RsTestError> {
    match a {
        &syn::NestedMeta::Literal(ref l) =>
            parse_expression(format!("{}", l.into_tokens())),
        &syn::NestedMeta::Meta(ref opt) => {
            match opt {
                &syn::Meta::List(ref arg) if arg.ident.as_ref() == "Unwrap" =>
                    match &arg.nested.first().unwrap().value() {
                        &syn::NestedMeta::Literal(syn::Lit::Str(inner_unwrap)) =>
                            parse_expression(inner_unwrap.value()),
                        _ => panic!("Unexpected case argument: {:?}", opt),
                    },
                nested_case => panic!("Unexpected case attribute: {:?}", nested_case)
            }
        }
    }
}

impl<'a> TryFrom<&'a MetaList> for TestCase {
    type Error = RsTestError;

    fn try_from(l: &'a MetaList) -> Result<Self, Self::Error> {
        if l.ident.as_ref() == "case" {
            let res: Result<Vec<_>, _> = l.nested.iter().map(
                |e|
                    parse_case_arg(e)
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

fn default_fixture_name(a: &syn::ArgCaptured) -> syn::Expr {
    if let syn::Pat::Ident(ref p) = a.pat {
        syn::parse_str(&format!("{}()", p.ident)).unwrap()
    } else {
        panic!("Argument should be a identity")
    }
}

fn captured_arg(arg: &syn::FnArg) -> &syn::ArgCaptured {
    if let syn::FnArg::Captured(ref a) = arg {
        a
    } else {
        panic!("Not a valid arg '{:?}'", arg)
    }
}

fn arg_name(arg: &syn::FnArg) -> &syn::Ident {
    if let syn::Pat::Ident(ref a) = captured_arg(arg).pat {
        &a.ident
    } else {
        panic!("Not a valid arg '{:?}'", arg)
    }
}

fn arg_2_fixture_str(arg: &syn::FnArg, resolver: &Resolver) -> Option<String> {
    if let &syn::FnArg::Captured(ref a) = arg {
        let declaration = a.pat.clone().into_tokens();
        let fixture = resolver
            .resolve(arg).map(|e| e.clone())
            .unwrap_or_else(|| default_fixture_name(a));
        Some(format!("let {} = {};", declaration, fixture.into_tokens()))
    } else {
        None
    }
}

fn arg_2_fixture(arg: &syn::FnArg, resolver: &Resolver) -> Option<syn::Stmt> {
    arg_2_fixture_str(arg, resolver).and_then(|line| syn::parse_str(&line).ok())
}

#[derive(Default)]
struct Resolver<'a>(std::collections::HashMap<String, &'a syn::Expr>);

impl<'a> Resolver<'a> {
    fn new(args: &Vec<syn::Ident>, case: &'a TestCase) -> Self {
        Resolver(
            args.iter()
                .zip(case.0.iter())
                .map(|(&name, expr)| (name.to_string(), expr))
                .collect()
        )
    }

    fn resolve(&self, arg: &syn::FnArg) -> Option<&syn::Expr> {
        if let syn::FnArg::Captured(_) = arg {
            self.0.get(&arg_name(arg).to_string())
                .map(|&a| a)
        } else {
            None
        }
    }
}

fn fixtures<'a>(item_fn: &'a syn::ItemFn, resolver: &'a Resolver) -> impl Iterator<Item=syn::Stmt> + 'a {
    item_fn.decl.inputs
        .iter()
        .filter_map(move |arg| arg_2_fixture(arg, resolver))
}

#[proc_macro_attribute]
pub fn rstest(_args: proc_macro::TokenStream,
              input: proc_macro::TokenStream)
              -> proc_macro::TokenStream {
    let ast = syn::parse(input.clone()).unwrap();
    if let syn::Item::Fn(ref item_fn) = ast {
        let name = item_fn.ident;
        let inner = item_fn.block.clone();
        let resolver = Resolver::default();
        let fixtures = fixtures(item_fn, &resolver);
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

fn add_parametrize_cases(item_fn: &syn::ItemFn, params: ParametrizeInfo) -> quote::Tokens {
    let mut res = quote::Tokens::default();
    let fname = item_fn.ident;

    let orig = item_fn.clone();

    res.append_all(quote! {
            #[cfg(test)]
            #orig
        }
    );
    for (n, case) in params.cases.iter().enumerate() {
        let resolver = Resolver::new(&params.args, &case);
        let fixtures = fixtures(item_fn, &resolver);
        let name = Ident::from(format!("{}_case_{}", fname, n));
        let args = item_fn.decl.inputs.iter().map(arg_name);
        let tcase = quote! {
                #[test]
                fn #name() {
                    #(#fixtures)*
                    #fname(#(#args),*)
                }
            };
        res.append_all(tcase);
    };
    println!("{}", res);
    res
}

#[proc_macro_attribute]
pub fn rstest_parametrize(args: proc_macro::TokenStream,
                          input: proc_macro::TokenStream)
                          -> proc_macro::TokenStream {
    let params = parse_parametrize_data(format!("{}", args)).unwrap();
    if let syn::Item::Fn(ref item_fn) = syn::parse(input).unwrap() {
        add_parametrize_cases(item_fn, params).into()
    } else {
        panic!("Should be a fn item");
    }
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
            item_fn.decl.inputs.iter()
        } else {
            panic!("Wrong ast!")
        }
    }

    #[test]
    fn extract_fixture_call_arg() {
        let ast = syn::parse_str("fn foo(fix: String) {}").unwrap();
        let args = fn_args(&ast).next().unwrap();
        let resolver = Resolver::default();

        let line = arg_2_fixture_str(args, &resolver);

        assert_eq!("let fix = fix ( );", &line.unwrap());
    }

    #[test]
    fn extract_fixture_should_add_mut_too() {
        let ast = syn::parse_str("fn foo(mut fix: String) {}").unwrap();
        let args = fn_args(&ast).next().unwrap();
        let resolver = Resolver::default();

        let line = arg_2_fixture_str(args, &resolver);

        assert_eq!("let mut fix = fix ( );", &line.unwrap());
    }

    #[test]
    fn arg_2_fixture_str_should_use_passed_fixture_if_any() {
        let ast = syn::parse_str("fn foo(fix: String) {}").unwrap();
        let call = syn::parse_str("bar()").unwrap();
        let args = fn_args(&ast).next().unwrap();
        let mut resolver = Resolver::default();
        resolver.add("fix", &call);

        let line = arg_2_fixture_str(args, &resolver);

        assert_eq!("let fix = bar ( );", &line.unwrap());
    }

    impl<'a> Resolver<'a> {
        fn add<S: AsRef<str>>(&mut self, ident: S, expr: &'a syn::Expr) {
            self.0.insert(ident.as_ref().to_string(), expr);
        }
    }


    #[test]
    fn resolver_should_return_the_given_expression() {
        let ast = syn::parse_str("fn function(foo: String) {}").unwrap();
        let arg = fn_args(&ast).next().unwrap();
        let expected = syn::parse_str("bar()").unwrap();
        let mut resolver = Resolver::default();

        resolver.add("foo", &expected);

        assert_eq!(&expected, resolver.resolve(&arg).unwrap())
    }

    #[test]
    fn resolver_should_return_none_for_unknown_argument() {
        let ast = syn::parse_str("fn function(foo: String) {}").unwrap();
        let arg = fn_args(&ast).next().unwrap();
        let resolver = Resolver::default();

        assert!(resolver.resolve(&arg).is_none())
    }

    #[test]
    fn resolver_build_from_test_case_and_args() {
        let ast = syn::parse_str("fn tcase(expected: usize, bar: u32, input: &str) {}").unwrap();
        let args = fn_args(&ast).collect::<Vec<_>>();
        let meta_args = r#"(
            expected, input,
            case(5, "ciao"),
            case(3, "Foo")
        )"#;

        let parametrize = parse_parametrize_data(meta_args).unwrap();

        let resolver = Resolver::new(&parametrize.args, &parametrize.cases[0]);

        assert_eq!(&parametrize.cases[0].0[0], resolver.resolve(&args[0]).unwrap());
        assert_eq!(&parametrize.cases[0].0[1], resolver.resolve(&args[2]).unwrap());
        assert!(resolver.resolve(&args[1]).is_none());
    }

    #[test]
    fn parametrize_no_name_vec_and_array() {
        let meta_args = r#"(
            expected, input,
            case(4, Unwrap("vec![1,3]")),
            case(10, Unwrap("[2,3,5]"))
        )"#;

        let data = parse_parametrize_data(meta_args).unwrap();

        let c0: TestCase = ["4", "vec![1, 3]"].as_ref().into();
        let c1: TestCase = ["10", "[2,3,5]"].as_ref().into();

        assert_eq!(&vec![Ident::from("expected"), Ident::from("input")],
                   &data.args);
        assert_eq!(&c0, &data.cases[0]);
        assert_eq!(&c1, &data.cases[1]);
        assert_eq!(&vec![c0, c1], &data.cases);
    }
}

