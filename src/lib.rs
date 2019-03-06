extern crate proc_macro;

use quote::{quote, TokenStreamExt, ToTokens};
use syn::{
    ArgCaptured, Expr, FnArg, Ident, ItemFn,
    parse_macro_input, parse_str, Pat,
    Stmt,
};

use error::error;
use proc_macro2::TokenStream;

mod parse;

trait Tokenize {
    fn into_tokens(self) -> TokenStream;
}

impl<T: ToTokens> Tokenize for T {
    fn into_tokens(self) -> TokenStream {
        quote! { #self }
    }
}

fn default_fixture_resolve(ident: &Ident) -> parse::CaseArg {
    let e = parse_str::<Expr>(&format!("{}()", ident.to_string())).unwrap();
    parse::CaseArg::from(e)
}

fn fn_arg_ident(arg: &FnArg) -> Option<&Ident> {
    match arg {
        FnArg::Captured(ArgCaptured { pat: Pat::Ident(ident), .. }) => Some(&ident.ident),
        _ => None
    }
}

fn arg_2_fixture(ident: &Ident, resolver: &Resolver) -> TokenStream {
    let fixture = resolver
        .resolve(ident)
        .map(|e| e.clone())
        .unwrap_or_else(|| default_fixture_resolve(ident));
    quote! {
        let #ident = #fixture;
    }
}

fn arg_2_fixture_dump_str(ident: &Ident, _resolver: &Resolver) -> String {
    format!(r#"println!("{name} = {{:?}}", {name});"#, name = ident)
}

fn arg_2_fixture_dump(ident: &Ident, resolver: &Resolver, modifiers: &Modifiers) -> Option<Stmt> {
    if modifiers.trace_me(ident) {
        parse_str(&arg_2_fixture_dump_str(ident, resolver)).ok()
    } else {
        None
    }
}

#[derive(Default)]
struct Resolver<'a> (std::collections::HashMap<String, &'a parse::CaseArg>);

impl<'a> Resolver<'a> {
    fn new(args: &Vec<Ident>, case: &'a parse::TestCase) -> Self {
        Resolver(
            args.iter()
                .zip(case.args.iter())
                .map(|(ref name, case_arg)| (name.to_string(), case_arg))
                .collect()
        )
    }

    fn resolve(&self, ident: &Ident) -> Option<&parse::CaseArg> {
        self.0.get(&ident.to_string()).map(|&a| a)
    }
}

fn fixtures<'a>(args: impl Iterator<Item=&'a FnArg> + 'a, resolver: &'a Resolver) -> impl Iterator<Item=TokenStream> + 'a {
    args.filter_map(fn_arg_ident)
        .map(move |arg|
            arg_2_fixture(arg, resolver)
        )
}

fn fixtures_dump<'a>(args: impl Iterator<Item=&'a FnArg> + 'a,
                     resolver: &'a Resolver, modifiers: &'a Modifiers)
                     -> impl Iterator<Item=Stmt> + 'a
{
    args.filter_map(fn_arg_ident)
        .filter_map(move |arg|
            arg_2_fixture_dump(arg, resolver, modifiers)
        )
}

fn fn_args(item_fn: &ItemFn) -> impl Iterator<Item=&FnArg> {
    item_fn.decl.inputs.iter()
}

fn fn_args_name(item_fn: &ItemFn) -> impl Iterator<Item=&Ident> {
    fn_args(item_fn).filter_map(fn_arg_ident)
}

const TRACE_VARIABLE_ATTR: &'static str = "trace";
const NOTRACE_VARIABLE_ATTR: &'static str = "notrace";

use parse::{Modifiers, RsTestAttribute};

impl Modifiers {
    fn trace_me(&self, ident: &Ident) -> bool {
        if self.should_trace() {
            self.modifiers
                .iter()
                .filter(|&m|
                    Modifiers::is_notrace(ident, m)
                ).next().is_none()
        } else { false }
    }

    fn is_notrace(ident: &Ident, m: &RsTestAttribute) -> bool {
        match m {
            RsTestAttribute::Tagged(i, args) if i == NOTRACE_VARIABLE_ATTR =>
                args.iter().find(|&a| a == ident).is_some(),
            _ => false
        }
    }

    fn should_trace(&self) -> bool {
        self.modifiers
            .iter()
            .filter(|&m|
                Modifiers::is_trace(m)
            ).next().is_some()
    }

    fn is_trace(m: &RsTestAttribute) -> bool {
        match m {
            RsTestAttribute::Attr(i) if i == TRACE_VARIABLE_ATTR => true,
            _ => false
        }
    }
}

fn trace_arguments<'a>(args: impl Iterator<Item=&'a FnArg> + 'a, resolver: &'a Resolver, modifiers: &'a Modifiers) -> proc_macro2::TokenStream {
    let mut fixtures_dump = fixtures_dump(args, resolver, modifiers).peekable();
    if fixtures_dump.peek().is_some() {
        quote! {
            println!("{:-^40}", " TEST ARGUMENTS ");
            #(#fixtures_dump)*
        }
    } else {
        Default::default()
    }
}

#[proc_macro_attribute]
pub fn rstest(args: proc_macro::TokenStream,
              input: proc_macro::TokenStream)
              -> proc_macro::TokenStream {
    let test = parse_macro_input!(input as ItemFn);
    let modifiers = parse_macro_input!(args as Modifiers);
    let name = &test.ident;
    let attrs = &test.attrs;
    let resolver = Resolver::default();
    let fixtures = fixtures(fn_args(&test), &resolver);
    let trace_args = trace_arguments(fn_args(&test), &resolver, &modifiers);
    let args = fn_args_name(&test);
    let res = quote! {
        #[test]
        #(#attrs)*
        fn #name() {
            #test
            #(#fixtures)*
            #trace_args
            println!("{:-^40}", " TEST START ");
            #name(#(#args),*)
        }
    };
    res.into()
}

fn fn_args_has_ident(fn_decl: &ItemFn, ident: &Ident) -> bool {
    fn_args(fn_decl)
        .filter_map(fn_arg_ident)
        .find(|&id| id == ident)
        .is_some()
}

mod error {
    use proc_macro2::*;
    use quote::quote_spanned;

    pub fn error(s: &str, start: Span, end: Span) -> TokenStream {
        let mut msg = quote_spanned! {
            start => compile_error!
        };
        msg.extend(
            quote_spanned! {
                end => (#s)
            }
        );
        msg
    }
}

fn error_report(test: &ItemFn, params: &parse::ParametrizeData) -> Option<TokenStream> {
    let invalid_args = params.args.iter()
        .filter(|&p| !fn_args_has_ident(test, p));

    let mut tokens = TokenStream::default();
    for missed in invalid_args {
        let span = missed.span().into();
        let message = format!("Missed argument: '{}' should be a test function argument.", missed);
        tokens.extend(error(&message, span, span));
    }

    if !tokens.is_empty() {
        Some(tokens)
    } else {
        None
    }
}

fn add_parametrize_cases(test: ItemFn, params: parse::ParametrizeInfo) -> TokenStream {
    let fname = &test.ident;
    let parse::ParametrizeInfo { data: params, modifier } = params;

    if let Some(tokens) = error_report(&test, &params) {
        return tokens;
    }

    let mut res = quote! {
            #[cfg(test)]
            #test
        };

    // TODO: Move to fold trait impl

    for (n, case) in params.cases.iter().enumerate() {
        res.append_all(
            if case.args.len() != params.args.len() {
                error("Wrong case signature: should match the given parameters list.",
                      case.span_start(), case.span_end())
            } else {
                let resolver = Resolver::new(&params.args, &case);
                let fixtures = fixtures(fn_args(&test), &resolver);
                let trace_args = trace_arguments(fn_args(&test), &resolver, &modifier);
                let name = Ident::new(&format!("{}_case_{}", fname, n), fname.span());
                let attrs = &test.attrs;
                let args = fn_args_name(&test);
                quote! {
                    #[test]
                    #(#attrs)*
                    fn #name() {
                        #(#fixtures)*
                        #trace_args
                        println!("{:-^40}", " TEST START ");
                        #fname(#(#args),*)
                    }
                }
            }
        )
    };
    res
}

#[proc_macro_attribute]
pub fn rstest_parametrize(args: proc_macro::TokenStream, input: proc_macro::TokenStream)
                          -> proc_macro::TokenStream
{
    let params = parse_macro_input!(args as parse::ParametrizeInfo);

    let test = parse_macro_input!(input as ItemFn);

    add_parametrize_cases(test, params).into()
}

#[cfg(test)]
mod test {
    use super::*;
    use syn::{Item, punctuated};
    use pretty_assertions::assert_eq;
    use syn::parse2;
    use syn::export::Debug;
    use crate::parse::*;

    fn fn_args(item: &Item) -> punctuated::Iter<'_, FnArg> {
        if let &Item::Fn(ref item_fn) = item {
            item_fn.decl.inputs.iter()
        } else {
            panic!("Wrong ast!")
        }
    }

    fn first_arg_ident(ast: &Item) -> &Ident {
        let arg = fn_args(&ast).next().unwrap();
        fn_arg_ident(arg).unwrap()
    }

    fn assert_syn_eq<P, S>(expected: S, ast: P) where
        S: AsRef<str>,
        P: syn::parse::Parse + Debug + Eq
    {
        assert_eq!(
            parse_str::<P>(expected.as_ref()).unwrap(),
            ast
        )
    }

    fn assert_statement_eq<T, S>(expected: S, tokens: T) where
        T: Into<TokenStream>,
        S: AsRef<str>
    {
        assert_syn_eq::<Stmt, _>(expected, parse2::<Stmt>(tokens.into()).unwrap())
    }

    #[test]
    fn extract_fixture_call_arg() {
        let ast = parse_str("fn foo(mut fix: String) {}").unwrap();
        let arg = first_arg_ident(&ast);
        let resolver = Resolver::default();

        let line = arg_2_fixture(arg, &resolver);

        assert_statement_eq("let fix = fix();", line);
    }

    #[test]
    fn extract_fixture_should_not_add_mut() {
        let ast = parse_str("fn foo(mut fix: String) {}").unwrap();
        let arg = first_arg_ident(&ast);
        let resolver = Resolver::default();

        let line = arg_2_fixture(arg, &resolver);

        assert_statement_eq("let fix = fix();", line);
    }

    fn case_arg<S: AsRef<str>>(s: S) -> CaseArg {
        parse_str::<Expr>(s.as_ref()).unwrap().into()
    }

    #[test]
    fn arg_2_fixture_str_should_use_passed_fixture_if_any() {
        let ast = parse_str("fn foo(mut fix: String) {}").unwrap();
        let arg = first_arg_ident(&ast);
        let call = case_arg("bar()");
        let mut resolver = Resolver::default();
        resolver.add("fix", &call);

        let line = arg_2_fixture(arg, &resolver);

        assert_statement_eq("let fix = bar();", line);
    }

    impl<'a> Resolver<'a> {
        fn add<S: AsRef<str>>(&mut self, ident: S, expr: &'a CaseArg) {
            self.0.insert(ident.as_ref().to_string(), expr);
        }
    }

    #[test]
    fn resolver_should_return_the_given_expression() {
        let ast = parse_str("fn function(mut foo: String) {}").unwrap();
        let arg = first_arg_ident(&ast);
        let expected = case_arg("bar()");
        let mut resolver = Resolver::default();

        resolver.add("foo", &expected);

        assert_eq!(&expected, resolver.resolve(&arg).unwrap())
    }

    #[test]
    fn resolver_should_return_none_for_unknown_argument() {
        let ast = parse_str("fn function(mut fix: String) {}").unwrap();
        let arg = first_arg_ident(&ast);
        let resolver = Resolver::default();

        assert!(resolver.resolve(&arg).is_none())
    }
}

