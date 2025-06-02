use syn::{parse_quote, Attribute, ItemFn};
use syn::visit_mut::VisitMut;
use crate::error::ErrorsVec;
use crate::parse::arguments::TestAttr;
use crate::parse::just_once::{AttrBuilder, JustOnceFnAttributeExtractor, Validator};
use crate::utils::attr_ends_with;

pub(crate) fn extract_test_attr(item_fn: &mut ItemFn) -> Result<Option<TestAttr>, ErrorsVec> {
    let mut extractor = JustOnceFnAttributeExtractor::<TestAttrBuilder>::new("test_attr");

    extractor.visit_item_fn_mut(item_fn);
    extractor.take().map(
        |extracted| match extracted {
            Some(a) => Some(a.into()),
            None => {
                if first_valid_test_attrs(item_fn.attrs.as_slice()).is_some() {
                    Some(TestAttr::InAttrs)
                } else {
                    None
                }
            }
        }
    )
}

pub fn first_valid_test_attrs(
    attributes: &[Attribute],
) -> Option<&Attribute> {
   attributes
       .iter()
       .find(|attr| attr_ends_with(attr, &parse_quote! { test }))
}

struct TestAttrBuilder;

impl AttrBuilder<ItemFn> for TestAttrBuilder {
    type Out = syn::Attribute;

    fn build(attr: syn::Attribute, _ident: &ItemFn) -> syn::Result<Self::Out> {
        match &attr.meta {
            syn::Meta::List(meta_list) => {
                let tokens = &meta_list.tokens;
                Ok(parse_quote! {
                #[#tokens]
            })
            },
            syn::Meta::Path(p) => {
                Ok(parse_quote! {
                #[#p]
            })
            },
            _ => Err(syn::Error::new_spanned(attr, "invalid `test_attr` syntax; should be `#[test_attr(<test attribute>)]`"))
        }
    }
}

impl Validator<ItemFn> for TestAttrBuilder {
    fn validate(item_fn: &ItemFn) -> syn::Result<()> {
        match first_valid_test_attrs(item_fn.attrs.as_slice()) {
            None => Ok(()),
            Some(t) => Err(syn::Error::new_spanned(t, "You cannot use both explicit `test_attr` and a valid test attribute"))
        }
    }
}
