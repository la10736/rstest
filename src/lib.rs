#![cfg_attr(all(feature = "nightly", feature = "dump_args"), feature(specialization))]
extern crate proc_macro;
extern crate proc_macro2;
#[macro_use]
extern crate quote;
extern crate syn;

use proc_macro2::TokenStream;
use quote::TokenStreamExt;
use quote::ToTokens;
use syn::*;
use syn::parse::Parse;
use syn::parse::ParseStream;
use syn::parse::Result as PResult;
use syn::punctuated::Punctuated;

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
        syn::NestedMeta::Literal(l) =>
            parse_expression(format!("{}", l.into_token_stream())),
        syn::NestedMeta::Meta(opt) => {
            match opt {
                syn::Meta::List(arg) if &arg.ident == "Unwrap" =>
                    match arg.nested.first().unwrap().value() {
                        syn::NestedMeta::Literal(syn::Lit::Str(inner_unwrap)) =>
                            parse_expression(inner_unwrap.value()),
                        _ => panic!("Unexpected case argument: {:?}", opt),
                    },
                syn::Meta::Word(term) => {
                    parse_expression(term.to_string())
                }
                nested_case => panic!("Unexpected case attribute: {:?}", nested_case)
            }
        }
    }
}

trait TryFrom<T>: Sized
    where T: Sized
{
    type Error;

    fn try_from(t: T) -> Result<Self, Self::Error>;
}

impl<'a> TryFrom<&'a MetaList> for TestCase {
    type Error = RsTestError;

    fn try_from(l: &'a MetaList) -> Result<Self, Self::Error> {
        if l.ident == "case" {
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
    use ParametrizeElement::*;
    match meta {
        syn::Meta::Word(ident) => Some(Arg(ident)),
        syn::Meta::List(ref l) if l.ident == "case" => { TestCase::try_from(l).map(Case).ok() }
        _ => None
    }
}

fn extract_meta(nm: NestedMeta) -> Option<Meta> {
    match nm {
        syn::NestedMeta::Meta(m) => Some(m),
        _ => None
    }
}

fn parse_parametrize_data(metas: Vec<NestedMeta>) -> PResult<ParametrizeInfo> {
    let mut args = vec![];
    let mut cases = vec![];
    use ParametrizeElement::*;

    metas.into_iter()
        .filter_map(extract_meta)
        .filter_map(parse_meta_item)
        .for_each(
            |item| {
                match item {
                    Arg(arg) => args.push(arg),
                    Case(case) => cases.push(case),
                }
            }
        );
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
        let fixture = resolver
            .resolve(arg).map(|e| e.clone())
            .unwrap_or_else(|| default_fixture_name(a));
        Some(format!("let {name} = {fix};", name = arg_name(arg), fix = fixture.into_token_stream()))
    } else {
        None
    }
}

fn arg_2_fixture(arg: &syn::FnArg, resolver: &Resolver) -> Option<syn::Stmt> {
    arg_2_fixture_str(arg, resolver).and_then(|line| syn::parse_str(&line).ok())
}

fn arg_2_fixture_dump_str(arg: &syn::FnArg, _resolver: &Resolver) -> Option<String> {
    if let &syn::FnArg::Captured(ref _a) = arg {
        Some(format!(r#"println!("{name} = {{}}", &{name}.display_string());"#, name = arg_name(arg)))
    } else {
        None
    }
}

fn arg_2_fixture_dump(arg: &syn::FnArg, resolver: &Resolver) -> Option<syn::Stmt> {
    arg_2_fixture_dump_str(arg, resolver).and_then(|line| syn::parse_str(&line).ok())
}

#[derive(Default)]
struct Resolver<'a>(std::collections::HashMap<String, &'a syn::Expr>);

impl<'a> Resolver<'a> {
    fn new(args: &Vec<syn::Ident>, case: &'a TestCase) -> Self {
        Resolver(
            args.iter()
                .zip(case.0.iter())
                .map(|(ref name, expr)| (name.to_string(), expr))
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

fn fixtures_apply<'a, A>( args: impl Iterator<Item=&'a syn::FnArg> + 'a, resolver: &'a Resolver, f: A)
    -> impl Iterator<Item=syn::Stmt> + 'a
    where A: Fn(&'a syn::FnArg, &'a Resolver) -> Option<syn::Stmt> + 'a
{
    args.filter_map(move |arg| f(arg, resolver))
}


fn fixtures<'a>(args: impl Iterator<Item=&'a syn::FnArg> + 'a, resolver: &'a Resolver) -> impl Iterator<Item=syn::Stmt> + 'a {
    fixtures_apply(args, resolver, arg_2_fixture)
}

fn fixtures_dump<'a>(args: impl Iterator<Item=&'a syn::FnArg> + 'a, resolver: &'a Resolver) -> impl Iterator<Item=syn::Stmt> + 'a {
    fixtures_apply(args, resolver, arg_2_fixture_dump)
}

fn fn_args(item_fn: &syn::ItemFn) -> impl Iterator<Item=&syn::FnArg> {
    item_fn.decl.inputs.iter()
}

fn fn_args_name(item_fn: &syn::ItemFn) -> impl Iterator<Item=&syn::Ident> {
    fn_args(item_fn).map(arg_name)
}

#[proc_macro_attribute]
pub fn rstest(_args: proc_macro::TokenStream,
              input: proc_macro::TokenStream)
              -> proc_macro::TokenStream {
    let test = parse_macro_input!(input as ItemFn);
    let name = &test.ident;
    let attrs = &test.attrs;
    let resolver = Resolver::default();
    let fixtures = fixtures(fn_args(&test), &resolver);
    let fixtures_dump = fixtures_dump(fn_args(&test), &resolver);
    let args = fn_args_name(&test);
    let res = quote! {
        #[test]
        #(#attrs)*
        fn #name() {
            #test
            #(#fixtures)*
            println!("{:-^40}", " TEST ARGUMENTS ");
            #(#fixtures_dump)*
            println!("{:-^40}", " TEST START ");
            #name(#(#args),*)
        }
    };
    res.into()
}

fn add_parametrize_cases(test: syn::ItemFn, params: ParametrizeInfo) -> TokenStream {
    let fname = &test.ident;

    let mut res = quote! {
            #[cfg(test)]
            #test
        };

    // TODO: Move to fold trait impl

    for (n, case) in params.cases.iter().enumerate() {
        let resolver = Resolver::new(&params.args, &case);
        let fixtures = fixtures(fn_args(&test), &resolver);
        let name = Ident::new(&format!("{}_case_{}", fname, n), fname.span());
        let attrs = &test.attrs;
        let args = fn_args_name(&test);
        let tcase = quote! {
                #[test]
                #(#attrs)*
                fn #name() {
                    #(#fixtures)*
                    #fname(#(#args),*)
                }
            };
        res.append_all(tcase);
    };
    res
}

impl Parse for ParametrizeInfo {
    fn parse(input: ParseStream) -> PResult<Self> {
        let all = Punctuated::<NestedMeta, Token![,]>::parse_separated_nonempty(input)?;
        parse_parametrize_data(all.into_iter().collect())
    }
}

#[proc_macro_attribute]
pub fn rstest_parametrize(args: proc_macro::TokenStream, input: proc_macro::TokenStream)
                          -> proc_macro::TokenStream
{
    let params = parse_macro_input!(args as ParametrizeInfo);

    let test = parse_macro_input!(input as ItemFn);

    add_parametrize_cases(test, params).into()
}

#[cfg(test)]
mod test {
    use super::*;

    fn fn_args(item: &syn::Item) -> syn::punctuated::Iter<'_, syn::FnArg> {
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
    fn extract_fixture_should_not_add_mut() {
        let ast = syn::parse_str("fn foo(mut fix: String) {}").unwrap();
        let args = fn_args(&ast).next().unwrap();
        let resolver = Resolver::default();

        let line = arg_2_fixture_str(args, &resolver);

        assert_eq!("let fix = fix ( );", &line.unwrap());
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
}

