use std::borrow::Cow;

use proc_macro2::TokenStream;
use quote::quote;
use syn::{parse_quote, Expr, FnArg, Ident, Stmt, Type};

use crate::{
    refident::{MaybeIdent, MaybeType},
    resolver::Resolver,
    utils::IsLiteralExpression,
};

pub(crate) fn resolve_aruments<'a>(
    args: impl Iterator<Item = &'a FnArg>,
    resolver: &impl Resolver,
    generic_types: &[Ident],
) -> TokenStream {
    let define_vars = args.map(|arg| ArgumentResolver::new(resolver, generic_types).resolve(arg));
    quote! {
        #(#define_vars)*
    }
}

struct ArgumentResolver<'resolver, 'idents, 'f, R>
where
    R: Resolver + 'resolver,
{
    resolver: &'resolver R,
    generic_types_names: &'idents [Ident],
    magic_conversion: &'f dyn Fn(Cow<Expr>, &Type) -> Expr,
}

impl<'resolver, 'idents, 'f, R> ArgumentResolver<'resolver, 'idents, 'f, R>
where
    R: Resolver + 'resolver,
{
    fn new(resolver: &'resolver R, generic_types_names: &'idents [Ident]) -> Self {
        Self {
            resolver,
            generic_types_names,
            magic_conversion: &handling_magic_conversion_code,
        }
    }

    fn resolve(&self, arg: &FnArg) -> Option<Stmt> {
        let ident = arg.maybe_ident()?;
        let arg_type = arg.maybe_type()?;
        let fixture_name = self.fixture_name(ident);

        let mut fixture = self
            .resolver
            .resolve(&fixture_name)
            .map(|e| e.clone())
            .unwrap_or_else(|| default_fixture_resolve(&fixture_name));

        if fixture.is_literal() && self.type_can_be_get_from_literal_str(arg_type) {
            fixture = Cow::Owned((self.magic_conversion)(fixture, arg_type));
        }
        Some(parse_quote! {
            let #ident = #fixture;
        })
    }

    fn fixture_name<'a>(&self, ident: &'a Ident) -> Cow<'a, Ident> {
        let id_str = ident.to_string();
        if id_str.starts_with("_") && !id_str.starts_with("__") {
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
            struct __Wrap<T>(std::marker::PhantomData<T>);

            trait __ViaParseDebug<'a, T> {
                fn magic_conversion(&self, input: &'a str) -> T;
            }

            impl<'a, T> __ViaParseDebug<'a, T> for &&__Wrap<T>
            where
                T: std::str::FromStr,
                T::Err: std::fmt::Debug,
            {
                fn magic_conversion(&self, input: &'a str) -> T {
                    T::from_str(input).unwrap()
                }
            }

            trait __ViaParse<'a, T> {
                fn magic_conversion(&self, input: &'a str) -> T;
            }

            impl<'a, T> __ViaParse<'a, T> for &__Wrap<T>
            where
                T: std::str::FromStr,
            {
                fn magic_conversion(&self, input: &'a str) -> T {
                    match T::from_str(input) {
                        Ok(v) => v,
                        Err(_) => {
                            panic!("Cannot parse '{}' to get {}", input, std::stringify!(#arg_type));
                        }
                    }
                }
            }

            trait __ViaIdent<'a, T> {
                fn magic_conversion(&self, input: &'a str) -> T;
            }

            impl<'a> __ViaIdent<'a, &'a str> for &&__Wrap<&'a str> {
                fn magic_conversion(&self, input: &'a str) -> &'a str {
                    input
                }
            }
            (&&&__Wrap::<#arg_type>(std::marker::PhantomData)).magic_conversion(#fixture)
        }
    }
}
