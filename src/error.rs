/// Module for error rendering stuff
use std::collections::HashMap;

use proc_macro2::TokenStream;
use syn::spanned::Spanned;
use syn::ItemFn;

use crate::parse::{
    fixture::FixtureInfo,
    rstest::{RsTestData, RsTestInfo},
};
use crate::refident::MaybeIdent;

use super::utils::fn_args_has_ident;

pub(crate) fn rstest(test: &ItemFn, info: &RsTestInfo) -> TokenStream {
    missed_arguments(test, info.data.items.iter())
        .chain(duplicate_arguments(info.data.items.iter()))
        .chain(invalid_cases(&info.data))
        .chain(case_args_without_cases(&info.data))
        .map(|e| e.to_compile_error())
        .collect()
}

pub(crate) fn fixture(test: &ItemFn, info: &FixtureInfo) -> TokenStream {
    missed_arguments(test, info.data.items.iter())
        .chain(duplicate_arguments(info.data.items.iter()))
        .map(|e| e.to_compile_error())
        .collect()
}

type Errors<'a> = Box<dyn Iterator<Item = syn::Error> + 'a>;

fn missed_arguments<'a, I: MaybeIdent + Spanned + 'a>(
    test: &'a ItemFn,
    args: impl Iterator<Item = &'a I> + 'a,
) -> Errors<'a> {
    Box::new(
        args.filter_map(|it| it.maybe_ident().map(|ident| (it, ident)))
            .filter(move |(_, ident)| !fn_args_has_ident(test, ident))
            .map(|(missed, ident)| {
                syn::Error::new(
                    missed.span(),
                    &format!(
                        "Missed argument: '{}' should be a test function argument.",
                        ident
                    ),
                )
            }),
    )
}

fn duplicate_arguments<'a, I: MaybeIdent + Spanned + 'a>(
    args: impl Iterator<Item = &'a I> + 'a,
) -> Errors<'a> {
    let mut used = HashMap::new();
    Box::new(
        args.filter_map(|it| it.maybe_ident().map(|ident| (it, ident)))
            .filter_map(move |(it, ident)| {
                let name = ident.to_string();
                let is_duplicate = used.contains_key(&name);
                used.insert(name, it);
                match is_duplicate {
                    true => Some((it, ident)),
                    false => None,
                }
            })
            .map(|(duplicate, ident)| {
                syn::Error::new(
                    duplicate.span(),
                    &format!("Duplicate argument: '{}' is already defined.", ident),
                )
            }),
    )
}

fn invalid_cases(params: &RsTestData) -> Errors {
    let n_args = params.case_args().count();
    Box::new(
        params
            .cases()
            .filter(move |case| case.args.len() != n_args)
            .map(|case| {
                syn::Error::new_spanned(
                    &case,
                    "Wrong case signature: should match the given parameters list.",
                )
            }),
    )
}

fn case_args_without_cases(params: &RsTestData) -> Errors {
    if !params.has_cases() {
        return Box::new(
            params
                .case_args()
                .map(|a| syn::Error::new(a.span(), "No cases for this argument.")),
        );
    }
    Box::new(std::iter::empty())
}
