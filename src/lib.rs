extern crate proc_macro;

use proc_macro2::TokenStream;
use quote::{quote, TokenStreamExt, ToTokens};
use syn::{
    ArgCaptured, Expr, FnArg, Ident, ItemFn, Lit, LitStr, Meta, MetaList, NestedMeta,
    parse::{self, Error, Parse, ParseStream}, parse_macro_input, parse_str, Pat, punctuated::Punctuated, spanned::Spanned,
    Stmt,
    Token,
    token,
};

use error::error;
use proc_macro2::Span;

#[derive(Default, Debug)]
struct ParametrizeData {
    args: Vec<Ident>,
    cases: Vec<TestCase>,
}

#[derive(Default, Debug)]
struct ParametrizeInfo {
    data: ParametrizeData,
    modifier: Modifiers,
}

#[derive(Debug)]
struct TestCase {
    args: Punctuated<CaseArg, Token![,]>,
}

impl TestCase {
    fn span_start(&self) -> Span {
        self.args.first().map(|arg| arg.span()).unwrap_or(Span::call_site())
    }
    fn span_end(&self) -> Span {
        self.args.last().map(|arg| arg.span()).unwrap_or(Span::call_site())
    }
}

#[derive(Debug, Clone)]
struct CaseArg {
    tokens: TokenStream,
    span: Span
}

impl ToTokens for CaseArg {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.tokens.to_tokens(tokens)
    }
}

impl CaseArg {
    fn new(tokens: TokenStream, span: Span) -> Self {
        Self { tokens , span }
    }
}

impl From<TokenStream> for CaseArg {
    fn from(tokens: TokenStream) -> Self {
        CaseArg::new(tokens, Span::call_site())
    }
}

fn is_arbitrary_rust_code(meta: &MetaList) -> bool {
    ["Unwrap", "r"].iter().any(|&n| meta.ident == n)
}

fn nested_meta_literal_str(nested: &NestedMeta) -> Option<&LitStr> {
    match nested {
        NestedMeta::Literal(Lit::Str(lit)) => Some(lit),
        _ => None
    }
}

fn compile_lit_str(lit: &LitStr) -> parse::Result<TokenStream> {
    lit.parse::<Expr>()
        .map(|e| quote! { #e })
        .or_else(|e| Err(Error::new(
            lit.span(),
            &format!("Cannot parse '{}' due {}", lit.value(), e)
        ))
        )
}

impl Parse for CaseArg {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        let nested: NestedMeta = input.parse()?;
        let tokens = match &nested {
            NestedMeta::Literal(l) =>
                quote! {#l},
            NestedMeta::Meta(Meta::Word(term)) => {
                quote! { #term }
            },
            NestedMeta::Meta(Meta::List(arg)) if is_arbitrary_rust_code(arg) => {
                arg.nested.first()
                    .map(|m| *m.value())
                    .and_then(nested_meta_literal_str)
                    .map(compile_lit_str)
                    .unwrap_or_else(
                        || Ok(error(&format!("Invalid {} argument", arg.ident), nested.span(), nested.span()))
                    )?
            }
            NestedMeta::Meta(Meta::List(arg)) =>
                error(&format!("Invalid case argument: `{}`", arg.ident), arg.span(), arg.span()),
            NestedMeta::Meta(nested) =>
                error(&format!("Unexpected case argument: {:?}", nested), nested.span(), nested.span())
        };
        Ok(CaseArg::new(tokens, nested.span()))
    }
}

impl Parse for TestCase {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        let case: Ident = input.parse()?;
        if case == "case" {
            let content;
            let _ = syn::parenthesized!(content in input);
            let args = content.parse_terminated(CaseArg::parse)?;
            Ok(TestCase { args})
        } else {
            Err(parse::Error::new(case.span(), "expected a test case"))
        }
    }
}

impl<'a, S: AsRef<str>> From<&'a [S]> for TestCase {
    fn from(strings: &[S]) -> Self {
        let args = strings
            .iter()
            .map(|s| {
                let e = parse_str::<Expr>(s.as_ref()).unwrap();
                CaseArg::from(quote! { # e })
            })
            .collect();
        TestCase { args }
    }
}

fn default_fixture_resolve(ident: &Ident) -> CaseArg {
    let e = parse_str::<Expr>(&format!("{}()", ident.to_string())).unwrap();
    CaseArg::from(quote! { #e })
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
struct Resolver<'a> (std::collections::HashMap<String, &'a CaseArg>);

impl<'a> Resolver<'a> {
    fn new(args: &Vec<Ident>, case: &'a TestCase) -> Self {
        Resolver(
            args.iter()
                .zip(case.args.iter())
                .map(|(ref name, case_arg)| (name.to_string(), case_arg))
                .collect()
        )
    }

    fn resolve(&self, ident: &Ident) -> Option<&CaseArg> {
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

#[derive(Debug)]
enum RsTestAttribute {
    Attr(Ident),
    Tagged(Ident, Vec<Ident>),
}

fn no_literal_nested(nested: NestedMeta) -> parse::Result<Meta> {
    match nested {
        NestedMeta::Meta(m) => Ok(m),
        NestedMeta::Literal(l) => Err(parse::Error::new(l.span(), "Unexpected literal"))
    }
}

fn just_word_meta(meta: Meta) -> parse::Result<Ident> {
    match meta {
        Meta::Word(ident) => Ok(ident),
        other => Err(parse::Error::new(other.span(), "Should be an ident"))
    }
}

impl Parse for RsTestAttribute {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        let meta = no_literal_nested(NestedMeta::parse(input)?)?;
        use Meta::*;
        match meta {
            Word(ident) => Ok(RsTestAttribute::Attr(ident)),
            List(l) =>
                Ok(RsTestAttribute::Tagged(l.ident,
                                           l.nested.into_iter()
                                               .map(no_literal_nested)
                                               .collect::<parse::Result<Vec<Meta>>>()?
                                               .into_iter().map(just_word_meta)
                                               .collect::<parse::Result<Vec<Ident>>>()?,
                )),
            NameValue(nv) => Err(parse::Error::new(nv.span(), "Invalid attribute"))
        }
    }
}

#[derive(Default, Debug)]
struct Modifiers {
    modifiers: Vec<RsTestAttribute>
}

impl Parse for Modifiers {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        let vars = Punctuated::<RsTestAttribute, Token![::]>::parse_terminated(input)?;
        Ok(Modifiers {
            modifiers: vars.into_iter().collect(),
        })
    }
}

const TRACE_VARIABLE_ATTR: &'static str = "trace";
const NOTRACE_VARIABLE_ATTR: &'static str = "notrace";

impl Modifiers {
    fn trace_me(&self, ident: &Ident) -> bool {
        if self.should_trace() {
            self.modifiers
                .iter()
                .filter(|&m|
                    match m {
                        RsTestAttribute::Tagged(i, args) if i == NOTRACE_VARIABLE_ATTR =>
                            args.iter().find(|&a| a == ident).is_some(),
                        _ => false
                    }
                ).next().is_none()
        } else { false }
    }

    fn should_trace(&self) -> bool {
        self.modifiers
            .iter()
            .filter(|&m|
                match m {
                    RsTestAttribute::Attr(i) if i == TRACE_VARIABLE_ATTR => true,
                    _ => false
                }
            ).next().is_some()
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
                end => (#s);
            }
        );
        msg
    }
}

fn error_report(test: &ItemFn, params: &ParametrizeData) -> Option<TokenStream> {
    let invalid_args = params.args.iter()
        .filter(|p| !fn_args_has_ident(test, p));

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

fn add_parametrize_cases(test: ItemFn, params: ParametrizeInfo) -> TokenStream {
    let fname = &test.ident;
    let ParametrizeInfo { data: params, modifier } = params;

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

impl Parse for ParametrizeData {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        let mut args = vec![];
        loop {
            if input.peek2(token::Paren) {
                break;
            }
            if let Ok(arg) = input.parse() {
                args.push(arg);
                if input.parse::<Token![,]>().is_err() {
                    break;
                }
            } else {
                break;
            }
        }

        let cases: Vec<_> =
            Punctuated::<TestCase, Token![,]>::parse_separated_nonempty(input)?
                .into_iter().collect();
        Ok(ParametrizeData {
            args,
            cases,
        })
    }
}

impl Parse for ParametrizeInfo {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        let data = input.parse()?;
        let modifiers = if input.parse::<Token![::]>().is_ok() {
            Some(input.parse()?)
        } else {
            None
        };
        Ok(ParametrizeInfo {
            data,
            modifier: modifiers.unwrap_or_default(),
        })
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
    use syn::{Item, punctuated};
    use pretty_assertions::assert_eq;
    use syn::parse2;
    use syn::export::Debug;

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
        {
            let e = parse_str::<Expr>(s.as_ref()).unwrap();
            quote!{ #e }
        }.into()
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

    impl PartialEq for CaseArg {
        fn eq(&self, other: &Self) -> bool {
            format!("{:?}", self.tokens) == format!("{:?}", other.tokens)
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

