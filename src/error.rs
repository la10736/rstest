use proc_macro2::*;
use quote::quote_spanned;

pub fn error(s: &str, start: Span, end: Span) -> TokenStream {
    let mut msg = quote_spanned! {
        start => compile_error!
    };
    msg.extend(
        quote_spanned! {
            end => (#s)
        }
    );
    msg
}

pub fn error_statement(s: &str, start: Span, end: Span) -> TokenStream {
    let e = error(s, start, end);
    quote_spanned! {
        end => #e;
    }
}
