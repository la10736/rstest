extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::{self, parse, parse::Parse, parse_macro_input, Ident, ItemFn, Token};

struct MergeAttrs {
    template: ItemFn,
    function: ItemFn,
}

impl Parse for MergeAttrs {
    fn parse(input: parse::ParseStream) -> syn::Result<Self> {
        let template = input.parse()?;
        let _comma: Token![,] = input.parse()?;
        let function = input.parse()?;
        Ok(Self { template, function })
    }
}

#[proc_macro]
pub fn merge_attrs(item: TokenStream) -> TokenStream {
    let MergeAttrs {template, mut function} = parse_macro_input!(item as MergeAttrs);
    let mut attrs = template.attrs;
    attrs.append(&mut function.attrs);
    function.attrs = attrs;

    let tokens = quote! {
        #function
    };
    tokens.into()
}

#[proc_macro_attribute]
pub fn apply(args: proc_macro::TokenStream, input: proc_macro::TokenStream) -> TokenStream {
    let template: Ident = parse(args).unwrap();
    let test: ItemFn = parse(input).unwrap();
    let tokens = quote! {
        #template! {

            #test
        }
    };
    tokens.into()
}

#[proc_macro_attribute]
pub fn template(_args: proc_macro::TokenStream, input: proc_macro::TokenStream) -> TokenStream {
    let template: ItemFn = parse(input).unwrap();
    let macro_name = template.sig.ident.clone();
    let tokens = quote! {
        macro_rules! #macro_name {
            ( $test:item ) => {
                        $crate::rstest_reuse::merge_attrs! {
                            #template,
                            $test
                        }
                    }
        }
    };
    tokens.into()
}
