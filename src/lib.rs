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
            Err(Error::new(l.span(), format!("Unknow action '{}'", l.ident)))
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

fn default_fixture_name(a: &ArgCaptured) -> Expr {
    if let Pat::Ident(ref p) = a.pat {
        parse_str(&format!("{}()", p.ident)).unwrap()
    } else {
        panic!("Argument should be a identity")
    }
}

fn captured_arg(arg: &FnArg) -> &ArgCaptured {
    if let FnArg::Captured(ref a) = arg {
        a
    } else {
        panic!("Not a valid arg '{:?}'", arg)
    }
}

fn arg_name(arg: &FnArg) -> &Ident {
    if let Pat::Ident(ref a) = captured_arg(arg).pat {
        &a.ident
    } else {
        panic!("Not a valid arg '{:?}'", arg)
    }
}

fn arg_2_fixture_str(arg: &FnArg, resolver: &Resolver) -> Option<String> {
    if let &FnArg::Captured(ref a) = arg {
        let fixture = resolver
            .resolve(arg).map(|e| e.clone())
            .unwrap_or_else(|| default_fixture_name(a));
        Some(format!("let {name} = {fix};", name = arg_name(arg), fix = fixture.into_token_stream()))
    } else {
        None
    }
}

fn arg_2_fixture(arg: &FnArg, resolver: &Resolver) -> Option<Stmt> {
    arg_2_fixture_str(arg, resolver).and_then(|line| parse_str(&line).ok())
}

fn arg_2_fixture_dump_str(arg: &FnArg, _resolver: &Resolver) -> Option<String> {
    if let &FnArg::Captured(ref _a) = arg {
        Some(format!(r#"println!("{name} = {{}}", &{name}.display_string());"#, name = arg_name(arg)))
    } else {
        None
    }
}

fn arg_2_fixture_dump(arg: &FnArg, resolver: &Resolver, modifiers: &Modifiers) -> Option<Stmt> {
    if modifiers.trace_me(arg_name(arg)) {
        arg_2_fixture_dump_str(arg, resolver).and_then(|line| parse_str(&line).ok())
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

    fn resolve(&self, arg: &FnArg) -> Option<&Expr> {
        if let FnArg::Captured(_) = arg {
            self.0.get(&arg_name(arg).to_string())
                .map(|&a| a)
        } else {
            None
        }
    }
}

fn fixtures_apply<'a, A>(args: impl Iterator<Item=&'a FnArg> + 'a, resolver: &'a Resolver, f: A)
                         -> impl Iterator<Item=Stmt> + 'a
    where A: Fn(&'a FnArg, &'a Resolver) -> Option<Stmt> + 'a
{
    args.filter_map(move |arg| f(arg, resolver))
}


fn fixtures<'a>(args: impl Iterator<Item=&'a FnArg> + 'a, resolver: &'a Resolver) -> impl Iterator<Item=Stmt> + 'a {
    fixtures_apply(args, resolver, arg_2_fixture)
}

fn fixtures_dump<'a>(args: impl Iterator<Item=&'a FnArg> + 'a,
                     resolver: &'a Resolver, modifiers: &'a Modifiers)
                     -> impl Iterator<Item=Stmt> + 'a
{
    fixtures_apply(args, resolver, move |a, r| arg_2_fixture_dump(a, r, modifiers))
}

fn fn_args(item_fn: &ItemFn) -> impl Iterator<Item=&FnArg> {
    item_fn.decl.inputs.iter()
}

fn fn_args_name(item_fn: &ItemFn) -> impl Iterator<Item=&Ident> {
    fn_args(item_fn).map(arg_name)
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

impl Modifiers {
    fn trace_me(&self, ident: &Ident) -> bool {
        if self.autotrace() {
            self.modifiers
                .iter()
                .filter(|&m|
                    match m {
                        RsTestAttribute::Tagged(i, args) if i == "notrace" =>
                            args.iter().find(|&a| a == ident).is_some(),
                        _ => false
                    }
                ).next().is_none()
        } else { false }
    }

    fn autotrace(&self) -> bool {
        if !cfg!(feature = "trace_all") {
            self.modifiers
                .iter()
                .filter(|&m|
                    match m {
                        RsTestAttribute::Attr(i) if i == "autotrace" => true,
                        _ => false
                    }
                ).next().is_some()
        } else {
            true
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

fn add_parametrize_cases(test: ItemFn, params: ParametrizeInfo) -> TokenStream {
    let fname = &test.ident;
    let ParametrizeInfo { data: params, modifier } = params;

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

    #[test]
    fn extract_fixture_call_arg() {
        let ast = parse_str("fn foo(fix: String) {}").unwrap();
        let args = fn_args(&ast).next().unwrap();
        let resolver = Resolver::default();

        let line = arg_2_fixture_str(args, &resolver);

        assert_eq!("let fix = fix ( );", &line.unwrap());
    }

    #[test]
    fn extract_fixture_should_not_add_mut() {
        let ast = parse_str("fn foo(mut fix: String) {}").unwrap();
        let args = fn_args(&ast).next().unwrap();
        let resolver = Resolver::default();

        let line = arg_2_fixture_str(args, &resolver);

        assert_eq!("let fix = fix ( );", &line.unwrap());
    }

    #[test]
    fn arg_2_fixture_str_should_use_passed_fixture_if_any() {
        let ast = parse_str("fn foo(fix: String) {}").unwrap();
        let call = parse_str("bar()").unwrap();
        let args = fn_args(&ast).next().unwrap();
        let mut resolver = Resolver::default();
        resolver.add("fix", &call);

        let line = arg_2_fixture_str(args, &resolver);

        assert_eq!("let fix = bar ( );", &line.unwrap());
    }

    impl<'a> Resolver<'a> {
        fn add<S: AsRef<str>>(&mut self, ident: S, expr: &'a Expr) {
            self.0.insert(ident.as_ref().to_string(), expr);
        }
    }


    #[test]
    fn resolver_should_return_the_given_expression() {
        let ast = parse_str("fn function(foo: String) {}").unwrap();
        let arg = fn_args(&ast).next().unwrap();
        let expected = parse_str("bar()").unwrap();
        let mut resolver = Resolver::default();

        resolver.add("foo", &expected);

        assert_eq!(&expected, resolver.resolve(&arg).unwrap())
    }

    #[test]
    fn resolver_should_return_none_for_unknown_argument() {
        let ast = parse_str("fn function(foo: String) {}").unwrap();
        let arg = fn_args(&ast).next().unwrap();
        let resolver = Resolver::default();

        assert!(resolver.resolve(&arg).is_none())
    }
}

