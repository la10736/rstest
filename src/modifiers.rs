use proc_macro2::Ident;
use syn::parse_quote;
use crate::parse::{Modifiers, RsTestAttribute};

macro_rules! wrap_modifiers {
    ($ident:ident) => {
        #[derive(Default, Debug, PartialEq)]
        pub(crate) struct $ident {
            inner: Modifiers,
        }

        impl From<Modifiers> for $ident {
            fn from(inner: Modifiers) -> Self {
                $ident { inner }
            }
        }

        impl $ident {
            fn iter(&self) -> impl Iterator<Item=&RsTestAttribute> {
                self.inner.modifiers.iter()
            }
        }
    };
}

wrap_modifiers!(RsTestModifiers);

impl RsTestModifiers {
    const TRACE_VARIABLE_ATTR: &'static str = "trace";
    const NOTRACE_VARIABLE_ATTR: &'static str = "notrace";

    pub(crate) fn trace_me(&self, ident: &Ident) -> bool {
        if self.should_trace() {
            self.iter()
                .filter(|&m|
                    Self::is_notrace(ident, m)
                ).next().is_none()
        } else { false }
    }

    fn is_notrace(ident: &Ident, m: &RsTestAttribute) -> bool {
        match m {
            RsTestAttribute::Tagged(i, args) if i == Self::NOTRACE_VARIABLE_ATTR =>
                args.iter().find(|&a| a == ident).is_some(),
            _ => false
        }
    }

    fn should_trace(&self) -> bool {
        self.iter()
            .filter(|&m|
                Self::is_trace(m)
            ).next().is_some()
    }

    fn is_trace(m: &RsTestAttribute) -> bool {
        match m {
            RsTestAttribute::Attr(i) if i == Self::TRACE_VARIABLE_ATTR => true,
            _ => false
        }
    }
}

wrap_modifiers!(FixtureModifiers);

impl FixtureModifiers {
    const DEFAULT_RET_ATTR: &'static str = "default";

    pub(crate) fn extract_default_type(self) -> Option<syn::ReturnType> {
        self.iter()
            .filter_map(|m|
                match m {
                    RsTestAttribute::Type(name, t) if name == Self::DEFAULT_RET_ATTR =>
                        Some(parse_quote!{ -> #t}),
                    _ => None
                })
            .next()
    }
}
