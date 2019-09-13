use proc_macro2::Ident;
use syn::{FnArg, ArgCaptured, Pat};

/// Provide `RefIdent` trait that gives a shortcut to extract identity ref (`syn::Ident` struct)
/// if any.

pub trait RefIdent {
    /// Return the reference to ident if any
    fn ident(&self) -> Option<&Ident>;
}

impl RefIdent for FnArg {
    fn ident(&self) -> Option<&Ident> {
        match self {
            FnArg::Captured(ArgCaptured { pat: Pat::Ident(ident), .. }) => Some(&ident.ident),
            _ => None
        }
    }
}

impl RefIdent for syn::Type {
    fn ident(&self) -> Option<&Ident> {
        match self {
            syn::Type::Path(tp) if tp.qself.is_none() && tp.path.segments.len() == 1 => {
                tp.path.segments.first()
                    .map(|pair| &pair.value().ident)
            }
            _ => None
        }
    }
}
