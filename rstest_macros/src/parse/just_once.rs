use std::marker::PhantomData;

use quote::ToTokens;
use syn::{visit_mut::VisitMut, Attribute, FnArg, Ident};

use crate::{error::ErrorsVec, refident::MaybeIdent, utils::attr_is};

pub trait Builder {
    type Out;

    fn build(attr: Attribute, ident: &Ident) -> syn::Result<Self::Out>;
}

pub trait Validator {
    fn validate(_arg: &FnArg) -> syn::Result<()> {
        Ok(())
    }
}

impl Builder for () {
    type Out = Ident;

    fn build(_attr: Attribute, ident: &Ident) -> syn::Result<Self::Out> {
        Ok(ident.clone())
    }
}

impl Validator for () {}

/// Simple struct used to visit function attributes and extract attributes that match
/// the `name`: Only one attribute is allowed for arguments.
pub struct JustOnceAttributeExtractor<'a, B = ()>
where
    B: Builder,
{
    name: &'a str,
    elements: Vec<B::Out>,
    errors: Vec<syn::Error>,
    _phantom: PhantomData<B>,
}

impl<'a> From<&'a str> for JustOnceAttributeExtractor<'a, ()> {
    fn from(value: &'a str) -> Self {
        Self::new(value)
    }
}

impl<'a, B> JustOnceAttributeExtractor<'a, B>
where
    B: Builder,
{
    pub fn new(name: &'a str) -> Self {
        Self {
            name,
            elements: Default::default(),
            errors: Default::default(),
            _phantom: PhantomData,
        }
    }

    pub fn take(self) -> Result<Vec<B::Out>, ErrorsVec> {
        if self.errors.is_empty() {
            Ok(self.elements)
        } else {
            Err(self.errors.into())
        }
    }
}

impl<B> VisitMut for JustOnceAttributeExtractor<'_, B>
where
    B: Builder,
    B: Validator,
{
    fn visit_fn_arg_mut(&mut self, node: &mut FnArg) {
        let name = if let Some(name) = node.maybe_ident().cloned() {
            name
        } else {
            return;
        };
        if let FnArg::Typed(ref mut arg) = node {
            // Extract interesting attributes
            let attrs = std::mem::take(&mut arg.attrs);
            let (extracted, remain): (Vec<_>, Vec<_>) =
                attrs.into_iter().partition(|a| attr_is(a, self.name));

            arg.attrs = remain;

            let parsed = extracted
                .into_iter()
                .map(|attr| B::build(attr.clone(), &name).map(|t| (attr, t)))
                .collect::<Result<Vec<_>, _>>();
            match parsed {
                Ok(data) => match data.len() {
                    1 => match B::validate(node) {
                        Ok(_) => self.elements.extend(data.into_iter().map(|(_attr, t)| t)),
                        Err(e) => {
                            self.errors.push(e);
                        }
                    },

                    0 => {}
                    _ => {
                        self.errors
                            .extend(data.into_iter().skip(1).map(|(attr, _t)| {
                                syn::Error::new_spanned(
                                    attr.into_token_stream(),
                                    format!("Cannot use #[{}] more than once.", self.name),
                                )
                            }));
                    }
                },
                Err(e) => {
                    self.errors.push(e);
                }
            }
        } else {
            return;
        }
    }
}
