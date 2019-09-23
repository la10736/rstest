use proc_macro2::Ident;
use syn::{FnArg, ArgCaptured, Pat};

/// Provide `RefIdent` trait that gives a shortcut to extract identity reference (`syn::Ident` struct)
/// if any.

pub trait RefIdent {
    /// Return the reference to ident if any
    fn ident(&self) -> &Ident;
}

pub trait MaybeIdent {
    /// Return the reference to ident if any
    fn maybe_ident(&self) -> Option<&Ident>;
}

impl <I: RefIdent> MaybeIdent for I {
    fn maybe_ident(&self) -> Option<&Ident> {
        Some(self.ident())
    }
}

impl RefIdent for Ident {
    fn ident(&self) -> &Ident {
        self
    }
}

impl<'a> RefIdent for &'a Ident {
    fn ident(&self) -> &Ident {
        *self
    }
}

impl MaybeIdent for FnArg {
    fn maybe_ident(&self) -> Option<&Ident> {
        match self {
            FnArg::Captured(ArgCaptured { pat: Pat::Ident(ident), .. }) => Some(&ident.ident),
            _ => None
        }
    }
}

impl MaybeIdent for syn::Type {
    fn maybe_ident(&self) -> Option<&Ident> {
        match self {
            syn::Type::Path(tp) if tp.qself.is_none() && tp.path.segments.len() == 1 => {
                tp.path.segments.first()
                    .map(|pair| &pair.value().ident)
            }
            _ => None
        }
    }
}
