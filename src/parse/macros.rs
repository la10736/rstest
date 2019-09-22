macro_rules! wrap_attributes {
    ($ident:ident) => {
        #[derive(Default, Debug, PartialEq)]
        pub(crate) struct $ident {
            inner: Attributes,
        }

        impl From<Attributes> for $ident {
            fn from(inner: Attributes) -> Self {
                $ident { inner }
            }
        }

        impl $ident {
            fn iter(&self) -> impl Iterator<Item=&Attribute> {
                self.inner.attributes.iter()
            }
        }
    };
}
