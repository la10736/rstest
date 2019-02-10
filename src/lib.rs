extern crate proc_macro;

use proc_macro2::TokenStream;
use quote::{quote, TokenStreamExt, ToTokens};

use syn::{
    Token, parse_macro_input, NestedMeta, Ident, Expr, parse_str, MetaList, Meta, ItemFn,
    Lit, Pat, FnArg, token, ArgCaptured, Stmt,
    parse::{self, Error, Parse, ParseStream},
    punctuated::Punctuated,
    spanned::Spanned
};

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

#[derive(PartialEq, Eq, Debug)]
struct TestCase(Vec<Expr>);

fn parse_expression<S: AsRef<str>>(s: S) -> Result<Expr, String> {
    parse_str::<Expr>(s.as_ref())
        .or(Err(format!("Cannot parse expression '{}'", s.as_ref())))
}

fn parse_case_arg(a: &NestedMeta) -> Result<Expr, Error> {
    match a {
        NestedMeta::Literal(l) =>
            parse_expression(format!("{}", l.into_token_stream())),
        NestedMeta::Meta(opt) => {
            match opt {
                Meta::List(arg) if &arg.ident == "Unwrap" =>
                    match arg.nested.first().unwrap().value() {
                        NestedMeta::Literal(Lit::Str(inner_unwrap)) =>
                            parse_expression(inner_unwrap.value()),
                        _ => panic!("Unexpected case argument: {:?}", opt),
                    },
                Meta::Word(term) => {
                    parse_expression(term.to_string())
                }
                nested_case => panic!("Unexpected case attribute: {:?}", nested_case)
            }
        }
    }.map_err(|m| Error::new(a.span(), m))
}

trait TryFrom<T>: Sized
    where T: Sized
{
    type Error;

    fn try_from(t: T) -> Result<Self, Self::Error>;
}

impl<'a> TryFrom<&'a MetaList> for TestCase {
    type Error = Error;

    fn try_from(l: &'a MetaList) -> parse::Result<Self> {
        if l.ident == "case" {
            let res: Result<Vec<_>, _> = l.nested.iter().map(
                |e|
                    parse_case_arg(e)
            ).collect();
            res.map(TestCase)
        } else {
            Err(Error::new(l.span(), format!("Unknown action '{}'", l.ident)))
        }
    }
}

impl Parse for TestCase {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        let meta: Meta = input.parse()?;
        match meta {
            Meta::List(ref l) if l.ident == "case" => {
                TestCase::try_from(l)
            }
            _ => Err(parse::Error::new(meta.span(), "expected a test case"))
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

enum ParametrizeElement {
    Arg(Ident),
    Case(TestCase),
}

impl Parse for ParametrizeElement {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        use self::ParametrizeElement::*;
        let meta: Meta = input.parse()?;
        match meta {
            Meta::Word(ident) => Ok(Arg(ident)),
            Meta::List(ref l) if l.ident == "case" => {
                TestCase::try_from(l).map(Case)
            }
            _ => Err(parse::Error::new(meta.span(), "expected ident or case"))
        }
    }
}

fn default_fixture_resolve(ident: &Ident) -> Expr {
    parse_str(&format!("{}()", ident.to_string())).unwrap()
}

fn fn_arg_ident<'a>(arg: &'a FnArg) -> Option<&'a Ident> {
    match arg {
        FnArg::Captured(ArgCaptured { pat: Pat::Ident(ident), .. }) => Some(&ident.ident),
        _ => None
    }
}

fn arg_2_fixture_str(ident: &Ident, resolver: &Resolver) -> String {
    let fixture = resolver
        .resolve(ident)
        .map(|e| e.clone())
        .unwrap_or_else(|| default_fixture_resolve(ident));
    format!("let {name} = {fix};", name = ident, fix = fixture.into_token_stream())
}

fn arg_2_fixture(ident: &Ident, resolver: &Resolver) -> Option<Stmt> {
    parse_str(&arg_2_fixture_str(ident, resolver)).ok()
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
struct Resolver<'a>(std::collections::HashMap<String, &'a Expr>);

impl<'a> Resolver<'a> {
    fn new(args: &Vec<Ident>, case: &'a TestCase) -> Self {
        Resolver(
            args.iter()
                .zip(case.0.iter())
                .map(|(ref name, expr)| (name.to_string(), expr))
                .collect()
        )
    }

    fn resolve(&self, ident: &Ident) -> Option<&Expr> {
        self.0.get(&ident.to_string()).map(|&a| a)
    }
}

fn fixtures<'a>(args: impl Iterator<Item=&'a FnArg> + 'a, resolver: &'a Resolver) -> impl Iterator<Item=Stmt> + 'a {
    args.filter_map(fn_arg_ident)
        .filter_map(move |arg|
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

    pub fn error(s: &str, span: Span) -> TokenStream {
        let msg = quote_spanned! {
            span => compile_error!(#s);
        };
        msg.into()
    }
}

use error::error;

fn add_parametrize_cases(test: ItemFn, params: ParametrizeInfo) -> TokenStream {
    let fname = &test.ident;
    let ParametrizeInfo { data: params, modifier } = params;

    let mut invalid_args = params.args.iter()
        .filter(|p| ! fn_args_has_ident(&test, p));

    if let Some(missed) = invalid_args.nth(0) {
        let span = missed.span().into();
        let message = format!("Missed argument: '{}' should be a test function argument.", missed);
        return error(&message, span);
    }

    let mut res = quote! {
            #[cfg(test)]
            #test
        };

    // TODO: Move to fold trait impl

    for (n, case) in params.cases.iter().enumerate() {
        let resolver = Resolver::new(&params.args, &case);
        let fixtures = fixtures(fn_args(&test), &resolver);
        let trace_args = trace_arguments(fn_args(&test), &resolver, &modifier);
        let name = Ident::new(&format!("{}_case_{}", fname, n), fname.span());
        let attrs = &test.attrs;
        let args = fn_args_name(&test);
        let tcase = quote! {
                #[test]
                #(#attrs)*
                fn #name() {
                    #(#fixtures)*
                    #trace_args
                    println!("{:-^40}", " TEST START ");
                    #fname(#(#args),*)
                }
            };
        res.append_all(tcase);
    };
    res
}

impl Parse for ParametrizeData {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        let mut args = vec![];
        let mut cases = vec![];

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

        let all = Punctuated::<TestCase, Token![,]>::parse_separated_nonempty(input)?;
        all.into_iter()
            .for_each(
                |case| {
                    cases.push(case)
                }
            );
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
            data: data,

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

    #[test]
    fn extract_fixture_call_arg() {
        let ast = parse_str("fn foo(mut fix: String) {}").unwrap();
        let arg = first_arg_ident(&ast);
        let resolver = Resolver::default();

        let line = arg_2_fixture_str(arg, &resolver);

        assert_eq!("let fix = fix ( );", line);
    }

    #[test]
    fn extract_fixture_should_not_add_mut() {
        let ast = parse_str("fn foo(mut fix: String) {}").unwrap();
        let arg = first_arg_ident(&ast);
        let resolver = Resolver::default();

        let line = arg_2_fixture_str(arg, &resolver);

        assert_eq!("let fix = fix ( );", line);
    }

    #[test]
    fn arg_2_fixture_str_should_use_passed_fixture_if_any() {
        let ast = parse_str("fn foo(mut fix: String) {}").unwrap();
        let arg = first_arg_ident(&ast);
        let call = parse_str("bar()").unwrap();
        let mut resolver = Resolver::default();
        resolver.add("fix", &call);

        let line = arg_2_fixture_str(arg, &resolver);

        assert_eq!("let fix = bar ( );", line);
    }

    impl<'a> Resolver<'a> {
        fn add<S: AsRef<str>>(&mut self, ident: S, expr: &'a Expr) {
            self.0.insert(ident.as_ref().to_string(), expr);
        }
    }


    #[test]
    fn resolver_should_return_the_given_expression() {
        let ast = parse_str("fn function(mut foo: String) {}").unwrap();
        let arg = first_arg_ident(&ast);
        let expected = parse_str("bar()").unwrap();
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

