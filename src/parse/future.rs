use syn::{parse_quote, visit_mut::VisitMut, FnArg, ItemFn};

use crate::{error::ErrorsVec, utils::attr_is};

#[derive(Default)]
pub(crate) struct ReplaceFutureAttribute(Vec<syn::Error>);

impl ReplaceFutureAttribute {
    pub(crate) fn replace(item_fn: &mut ItemFn) -> Result<(), ErrorsVec> {
        let mut visitor = Self::default();
        visitor.visit_item_fn_mut(item_fn);
        if visitor.0.is_empty() {
            Ok(())
        } else {
            Err(visitor.0.into())
        }
    }
}

impl VisitMut for ReplaceFutureAttribute {
    fn visit_fn_arg_mut(&mut self, node: &mut FnArg) {
        match node {
            FnArg::Typed(t) => {
                let attrs = std::mem::take(&mut t.attrs);
                let (futures, attrs): (Vec<_>, Vec<_>) =
                    attrs.into_iter().partition(|a| attr_is(a, "future"));
                if futures.len() > 0 {
                    let ty = &t.ty;
                    t.ty = parse_quote! {
                        impl std::future::Future<Output = #ty>
                    }
                }
                t.attrs = attrs;
            }
            FnArg::Receiver(_) => {}
        }
    }
}
