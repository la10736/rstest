use quote::format_ident;
use syn::parse_quote;

pub fn crate_name() -> syn::Path {
    use proc_macro_crate::FoundCrate;

    match proc_macro_crate::crate_name("rstest").expect("rstest is present in `Cargo.toml` qed") {
        FoundCrate::Itself => parse_quote! { rstest },
        FoundCrate::Name(name) => {
            let myself = format_ident!("{name}");
            parse_quote! { #myself }
        }
    }
}
