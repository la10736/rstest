use std::borrow::Cow;

use quote::format_ident;
use syn::{parse_quote, visit_mut::VisitMut, Expr, FnArg, Ident, Lifetime, Signature};

use crate::{
    parse::future::extend_generics_with_lifetimes, refident::MaybeIdent, resolver::Resolver,
};

#[derive(Default)]
pub(crate) struct ReplaceAwaitAttribute<'a> {
    await_args: &'a [Ident],
    lifetimes: Vec<Lifetime>,
}

impl<'a> ReplaceAwaitAttribute<'a> {
    pub(crate) fn replace(await_args: &[Ident], mut sig: Signature) -> Signature {
        let mut visitor = Self::default();
        visitor.await_args = await_args;
        visitor.visit_signature_mut(&mut sig);
        if !visitor.lifetimes.is_empty() {
            sig.generics = extend_generics_with_lifetimes(
                sig.generics.params.iter(),
                visitor.lifetimes.iter(),
            );
        }
        sig
    }
}

impl<'a> VisitMut for ReplaceAwaitAttribute<'a> {
    fn visit_fn_arg_mut(&mut self, node: &mut FnArg) {
        let ident = if let Some(ident) = node.maybe_ident().cloned() {
            ident
        } else {
            return;
        };
        match node {
            FnArg::Typed(t) => {
                if !self.await_args.contains(&ident) {
                    return;
                }
                let ty = &mut t.ty;
                use syn::Type::*;
                if let Reference(tr) = ty.as_mut() {
                    if tr.lifetime.is_none() {
                        let lifetime = syn::Lifetime {
                            apostrophe: ident.span(),
                            ident: format_ident!("_{}", ident),
                        };
                        self.lifetimes.push(lifetime.clone());
                        tr.lifetime = lifetime.into();
                    }
                }

                t.ty = parse_quote! {
                    impl std::future::Future<Output = #ty>
                };
            }
            FnArg::Receiver(_) => {}
        }
    }
}

pub(crate) struct AwaitResolver<'a, SubResolver>
where
    SubResolver: Resolver,
{
    pub sub_resolver: SubResolver,
    pub await_args: &'a [Ident],
}

impl<'a, SubResolver> Resolver for AwaitResolver<'a, SubResolver>
where
    SubResolver: Resolver,
{
    fn resolve(&self, ident: &Ident) -> Option<Cow<Expr>> {
        self.sub_resolver.resolve(ident).map(|expr| {
            if self.await_args.contains(ident) {
                Cow::Owned(parse_quote! { #expr.await })
            } else {
                expr
            }
        })
    }
}
