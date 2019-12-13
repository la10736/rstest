/// Define `Resolver` trait and implement it on some hashmaps and also define the `Resolver` tuple
/// composition. Provide also some utility functions related to how to create a `Resolver` and
/// resolving render.
///

use std::borrow::Cow;
use std::collections::HashMap;

use syn::{parse_quote, Expr};
use proc_macro2::{Ident, Span};

use crate::parse::Fixture;

pub(crate) fn fixture_resolver<'a>(fixtures: impl Iterator<Item=&'a Fixture>) -> impl Resolver + 'a {
    fixtures.map(|f|
        ( f.name.to_string(), extract_resolve_expression(f).into() )
    ).collect::<HashMap<_, Expr>>()
}

/// A trait that `resolves` the given ident to expression code to assign the value.
pub(crate) trait Resolver {
    fn resolve(&self, ident: &Ident) -> Option<Cow<Expr>>;
}

impl<'a> Resolver for HashMap<String, &'a Expr> {
    fn resolve(&self, ident: &Ident) -> Option<Cow<Expr>> {
        let ident = ident.to_string();
        self.get(&ident)
            .map(|&c| Cow::Borrowed(c) )
    }
}

impl<'a> Resolver for HashMap<String, Expr> {
    fn resolve(&self, ident: &Ident) -> Option<Cow<Expr>> {
        let ident = ident.to_string();
        self.get(&ident)
            .map(|c| Cow::Borrowed(c) )
    }
}

impl<R1: Resolver, R2: Resolver> Resolver for (R1, R2) {
    fn resolve(&self, ident: &Ident) -> Option<Cow<Expr>> {
        self.0.resolve(ident).or_else(|| self.1.resolve(ident))
    }
}

impl<R: Resolver + ?Sized> Resolver for &R {
    fn resolve(&self, ident: &Ident) -> Option<Cow<Expr>> {
        (*self).resolve(ident)
    }
}

impl<R: Resolver + ?Sized> Resolver for Box<R> {
    fn resolve(&self, ident: &Ident) -> Option<Cow<Expr>> {
        (**self).resolve(ident)
    }
}

impl Resolver for (String, Expr) {
    fn resolve(&self, ident: &Ident) -> Option<Cow<Expr>> { 
        if self.0 == ident.to_string() {
            Some(Cow::Borrowed(&self.1))
        } else {
            None
        }
    }
    
}

fn extract_resolve_expression(fixture: &Fixture) -> syn::Expr {
    let name = &fixture.name;
    let positional= &fixture.positional;
    let pname = format!("partial_{}", positional.len());
    let partial = Ident::new(&pname, Span::call_site());
    parse_quote! { #name::#partial(#(#positional), *) }
}
