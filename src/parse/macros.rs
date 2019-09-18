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
