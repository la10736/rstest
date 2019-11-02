/// Contains some unsorted functions used across others modules
/// 

use syn::{Ident, ItemFn, FnArg};
use crate::refident::MaybeIdent;

/// Return an iterator over fn arguments items.
/// 
pub(crate) fn fn_args(item_fn: &ItemFn) -> impl Iterator<Item=&FnArg> {
    item_fn.sig.inputs.iter()
}

/// Return if function declaration has an ident
/// 
pub(crate) fn fn_args_has_ident(fn_decl: &ItemFn, ident: &Ident) -> bool {
    fn_args(fn_decl)
        .filter_map(MaybeIdent::maybe_ident)
        .find(|&id| id == ident)
        .is_some()
}

#[cfg(test)]
mod test {
    use syn::parse_quote;
    
    use super::*;
    use crate::test::{*, assert_eq};
    
    #[test]
    fn fn_args_should() {
        let item_fn = parse_quote! {
                fn the_functon(first: u32, second: u32) {}
            };

        let mut args = fn_args(&item_fn);

        assert_eq!("first", args.next().and_then(|a| a.maybe_ident()).unwrap().to_string());
        assert_eq!("second", args.next().and_then(|a| a.maybe_ident()).unwrap().to_string());
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