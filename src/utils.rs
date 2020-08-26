use crate::refident::MaybeIdent;
use quote::format_ident;

/// Contains some unsorted functions used across others modules
///
use syn::{Attribute, FnArg, Ident, ItemFn};

/// Return an iterator over fn arguments items.
///
pub(crate) fn fn_args_idents(test: &ItemFn) -> impl Iterator<Item = &Ident> {
    fn_args(&test).filter_map(MaybeIdent::maybe_ident)
}

/// Return if function declaration has an ident
///
pub(crate) fn fn_args_has_ident(fn_decl: &ItemFn, ident: &Ident) -> bool {
    fn_args_idents(fn_decl).find(|&id| id == ident).is_some()
}

/// Return an iterator over fn arguments.
///
pub(crate) fn fn_args(item_fn: &ItemFn) -> impl Iterator<Item = &FnArg> {
    item_fn.sig.inputs.iter()
}

pub(crate) fn attr_ends_with(attr: &Attribute, segment: &syn::PathSegment) -> bool {
    &attr.path.segments.iter().last() == &Some(segment)
}

pub(crate) fn attr_starts_with(attr: &Attribute, segment: &syn::PathSegment) -> bool {
    &attr.path.segments.iter().nth(0) == &Some(segment)
}

pub(crate) fn attr_is(attr: &Attribute, name: &str) -> bool {
    attr.path.is_ident(&format_ident!("{}", name))
}

#[cfg(test)]
mod test {
    use syn::parse_quote;

    use super::*;
    use crate::test::{assert_eq, *};

    #[test]
    fn fn_args_idents_should() {
        let item_fn = parse_quote! {
            fn the_functon(first: u32, second: u32) {}
        };

        let mut args = fn_args_idents(&item_fn);

        assert_eq!("first", args.next().unwrap().to_string());
        assert_eq!("second", args.next().unwrap().to_string());
    }

    #[test]
    fn fn_args_has_ident_should() {
        let item_fn = parse_quote! {
            fn the_functon(first: u32, second: u32) {}
        };

        assert!(fn_args_has_ident(&item_fn, &ident("first")));
        assert!(!fn_args_has_ident(&item_fn, &ident("third")));
    }
}
