use std::borrow::Cow;

use proc_macro2::TokenStream;
use quote::quote;
use syn::{parse_quote, Expr, FnArg, Ident, Stmt, Type};

use crate::{
    parse::arguments::ArgumentsInfo,
    refident::{MaybeIdent, MaybeType},
    resolver::Resolver,
    utils::{fn_arg_mutability, IsLiteralExpression},
};

pub(crate) fn resolve_aruments<'a>(
    args: impl Iterator<Item = &'a FnArg>,
    resolver: &impl Resolver,
    generic_types: &[Ident],
    await_policy: &impl AwaitPolicy,
) -> TokenStream {
    let define_vars =
        args.map(|arg| ArgumentResolver::new(resolver, generic_types, await_policy).resolve(arg));
    quote! {
        #(#define_vars)*
    }
}

pub(crate) trait AwaitPolicy {
    fn should_await(&self, ident: &Ident) -> bool;
}

impl<A: AwaitPolicy> AwaitPolicy for &A {
    fn should_await(&self, ident: &Ident) -> bool {
        (*self).should_await(ident)
    }
}

struct AwaitNone;
impl AwaitPolicy for AwaitNone {
    fn should_await(&self, _ident: &Ident) -> bool {
        false
    }
}

struct ArgumentResolver<'resolver, 'idents, 'f, R, A>
where
    R: Resolver + 'resolver,
    A: AwaitPolicy + 'resolver,
{
    resolver: &'resolver R,
    generic_types_names: &'idents [Ident],
    await_policy: A,
    magic_conversion: &'f dyn Fn(Cow<Expr>, &Type) -> Expr,
}

impl<'resolver, 'idents, 'f, R, A> ArgumentResolver<'resolver, 'idents, 'f, R, A>
where
    R: Resolver + 'resolver,
    A: AwaitPolicy + 'resolver,
{
    fn new(resolver: &'resolver R, generic_types_names: &'idents [Ident], await_policy: A) -> Self {
        Self {
            resolver,
            generic_types_names,
            await_policy,
            magic_conversion: &handling_magic_conversion_code,
        }
    }

    fn resolve(&self, arg: &FnArg) -> Option<Stmt> {
        let ident = arg.maybe_ident()?;
        let mutability = fn_arg_mutability(arg);
        let unused_mut: Option<syn::Attribute> = mutability
            .as_ref()
            .map(|_| parse_quote! {#[allow(unused_mut)]});
        let arg_type = arg.maybe_type()?;
        let fixture_name = self.fixture_name(ident);

        let mut fixture = self
            .resolver
            .resolve(ident)
            .or_else(|| self.resolver.resolve(&fixture_name))
            .unwrap_or_else(|| default_fixture_resolve(&fixture_name));

        if fixture.is_literal() && self.type_can_be_get_from_literal_str(arg_type) {
            fixture = Cow::Owned((self.magic_conversion)(fixture, arg_type));
        } else if self.await_policy.should_await(ident) {
            fixture = Cow::Owned(parse_quote! { #fixture.await });
        }
        Some(parse_quote! {
            #unused_mut
            let #mutability #ident = #fixture;
        })
    }

    fn fixture_name<'a>(&self, ident: &'a Ident) -> Cow<'a, Ident> {
        let id_str = ident.to_string();
        if id_str.starts_with('_') && !id_str.starts_with("__") {
            Cow::Owned(Ident::new(&id_str[1..], ident.span()))
        } else {
            Cow::Borrowed(ident)
        }
    }

    fn type_can_be_get_from_literal_str(&self, t: &Type) -> bool {
        // Check valid type to apply magic conversion
        match t {
            Type::ImplTrait(_)
            | Type::TraitObject(_)
            | Type::Infer(_)
            | Type::Group(_)
            | Type::Macro(_)
            | Type::Never(_)
            | Type::Paren(_)
            | Type::Verbatim(_)
            | Type::Slice(_) => return false,
            _ => {}
        }
        match t.maybe_ident() {
            Some(id) => !self.generic_types_names.contains(id),
            None => true,
        }
    }
}

fn default_fixture_resolve(ident: &Ident) -> Cow<Expr> {
    Cow::Owned(parse_quote! { #ident::default() })
}

fn handling_magic_conversion_code(fixture: Cow<Expr>, arg_type: &Type) -> Expr {
    parse_quote! {
        {
            use rstest::magic_conversion::*;
            (&&&Magic::<#arg_type>(std::marker::PhantomData)).magic_conversion(#fixture)
        }
    }
}

impl AwaitPolicy for ArgumentsInfo {
    fn should_await(&self, ident: &Ident) -> bool {
        return self.is_future_await(ident);
    }
}

#[cfg(test)]
mod should {
    use super::*;
    use crate::{
        test::{assert_eq, *},
        utils::fn_args,
    };
    use syn::Ident;

    #[rstest]
    #[case::as_is("fix: String", "let fix = fix::default();")]
    #[case::without_underscore("_fix: String", "let _fix = fix::default();")]
    #[case::do_not_remove_inner_underscores("f_i_x: String", "let f_i_x = f_i_x::default();")]
    #[case::do_not_remove_double_underscore("__fix: String", "let __fix = __fix::default();")]
    #[case::preserve_mut_but_annotate_as_allow_unused_mut(
        "mut fix: String",
        "#[allow(unused_mut)] let mut fix = fix::default();"
    )]
    fn call_fixture(#[case] arg_str: &str, #[case] expected: &str) {
        let arg = arg_str.ast();

        let injected = ArgumentResolver::new(&EmptyResolver {}, &[], AwaitNone)
            .resolve(&arg)
            .unwrap();

        assert_eq!(injected, expected.ast());
    }

    #[rstest]
    #[case::as_is("fix: String", ("fix", expr("bar()")), "let fix = bar();")]
    #[case::with_allow_unused_mut("mut fix: String", ("fix", expr("bar()")), "#[allow(unused_mut)] let mut fix = bar();")]
    #[case::without_undescore("_fix: String", ("fix", expr("bar()")), "let _fix = bar();")]
    #[case::without_remove_underscore_if_value("_orig: S", ("_orig", expr("S{}")), r#"let _orig = S{};"#)]
    fn call_given_fixture(
        #[case] arg_str: &str,
        #[case] rule: (&str, Expr),
        #[case] expected: &str,
    ) {
        let arg = arg_str.ast();
        let mut resolver = std::collections::HashMap::new();
        resolver.insert(rule.0.to_owned(), &rule.1);

        let injected = ArgumentResolver::new(&resolver, &[], AwaitNone)
            .resolve(&arg)
            .unwrap();

        assert_eq!(injected, expected.ast());
    }

    struct AwaitAll;
    impl AwaitPolicy for AwaitAll {
        fn should_await(&self, _ident: &Ident) -> bool {
            true
        }
    }

    impl<'ident> AwaitPolicy for &'ident [Ident] {
        fn should_await(&self, ident: &Ident) -> bool {
            self.contains(ident)
        }
    }

    impl<'ident> AwaitPolicy for Vec<Ident> {
        fn should_await(&self, ident: &Ident) -> bool {
            self.as_slice().should_await(ident)
        }
    }

    #[rstest]
    #[case::no_await("fix: String", AwaitNone, "let fix = fix::default();")]
    #[case::await_all("fix: String", AwaitAll, "let fix = fix::default().await;")]
    #[case::await_present("fix: String", vec![ident("fix")], "let fix = fix::default().await;")]
    #[case::await_missed("fix: String", vec![ident("something_else"), ident("other")], "let fix = fix::default();")]
    #[case::await_missed_empty("fix: String", Vec::<Ident>::new(), "let fix = fix::default();")]
    fn honorate_await_policy(
        #[case] arg_str: &str,
        #[case] policy: impl AwaitPolicy,
        #[case] expected: &str,
    ) {
        let arg = arg_str.ast();

        let injected = ArgumentResolver::new(&EmptyResolver, &[], policy)
            .resolve(&arg)
            .unwrap();

        assert_eq!(injected, expected.ast());
    }

    fn _mock_conversion_code(fixture: Cow<Expr>, arg_type: &Type) -> Expr {
        parse_quote! {
            #fixture as #arg_type
        }
    }

    #[rstest]
    #[case::implement_it(
        "fn test(arg: MyType){}",
        0,
        r#"let arg = "value to convert" as MyType;"#
    )]
    #[case::discard_impl(
        "fn test(arg: impl AsRef<str>){}",
        0,
        r#"let arg = "value to convert";"#
    )]
    #[case::discard_generic_type(
        "fn test<S: AsRef<str>>(arg: S){}",
        0,
        r#"let arg = "value to convert";"#
    )]
    fn handle_magic_conversion(#[case] fn_str: &str, #[case] n_arg: usize, #[case] expected: &str) {
        let function = fn_str.ast();
        let arg = fn_args(&function).nth(n_arg).unwrap();
        let generics = function
            .sig
            .generics
            .type_params()
            .map(|tp| &tp.ident)
            .cloned()
            .collect::<Vec<_>>();

        let mut resolver = std::collections::HashMap::new();
        let expr = expr(r#""value to convert""#);
        resolver.insert(arg.maybe_ident().unwrap().to_string(), &expr);

        let ag = ArgumentResolver {
            resolver: &resolver,
            generic_types_names: &generics,
            await_policy: AwaitNone,
            magic_conversion: &_mock_conversion_code,
        };

        let injected = ag.resolve(&arg).unwrap();

        assert_eq!(injected, expected.ast());
    }
}
