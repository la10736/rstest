/// Module for error rendering stuff
use std::collections::HashMap;

use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::{spanned::Spanned, visit::Visit};
use syn::{visit, Expr, ItemFn, Pat};

use crate::parse::arguments::ArgumentsInfo;
use crate::parse::{
    fixture::FixtureInfo,
    rstest::{RsTestData, RsTestInfo},
    testcase::{CaseArgFallback, TestCase},
};
use crate::refident::{MaybeIdent, MaybePat};
use crate::utils::attr_is;

use super::utils::fn_args_has_pat;

pub mod messages {
    pub const DESTRUCT_WITHOUT_FROM : &str = "To destruct a fixture you should provide a path to resolve it by '#[from(...)]' attribute.";
    pub fn use_more_than_once(name: &str) -> String {
        format!("You cannot use '{name}' attribute more than once for the same argument")
    }
}

pub(crate) fn rstest(test: &ItemFn, info: &RsTestInfo) -> TokenStream {
    missed_arguments(test, info.data.items.iter())
        .chain(duplicate_arguments(info.data.items.iter()))
        .chain(invalid_cases(info))
        .chain(case_fallbacks_on_future_args(info))
        .chain(case_args_without_cases(&info.data))
        .chain(destruct_fixture_without_from(test, info))
        .chain(test_attributes(test, &info.arguments))
        .map(|e| e.to_compile_error())
        .collect()
}

pub(crate) fn fixture(test: &ItemFn, info: &FixtureInfo) -> TokenStream {
    missed_arguments(test, info.data.items.iter())
        .chain(duplicate_arguments(info.data.items.iter()))
        .chain(async_once(test, info))
        .chain(generics_once(test, info))
        .chain(destruct_fixture_without_from(test, info))
        .map(|e| e.to_compile_error())
        .collect()
}

fn async_once<'a>(test: &'a ItemFn, info: &FixtureInfo) -> Errors<'a> {
    match (test.sig.asyncness, info.arguments.get_once()) {
        (Some(_asyncness), Some(once)) => Box::new(std::iter::once(syn::Error::new_spanned(
            once,
            "Cannot apply #[once] to async fixture.",
        ))),
        _ => Box::new(std::iter::empty()),
    }
}

#[derive(Default)]
struct SearchImpl(bool);

impl<'ast> Visit<'ast> for SearchImpl {
    fn visit_type(&mut self, i: &'ast syn::Type) {
        if self.0 {
            return;
        }
        if let syn::Type::ImplTrait(_) = i {
            self.0 = true
        }
        visit::visit_type(self, i);
    }
}

impl SearchImpl {
    fn function_has_some_impl(f: &ItemFn) -> bool {
        let mut s = SearchImpl::default();
        visit::visit_item_fn(&mut s, f);
        s.0
    }
}

fn has_some_generics(test: &ItemFn) -> bool {
    !test.sig.generics.params.is_empty() || SearchImpl::function_has_some_impl(test)
}

fn generics_once<'a>(test: &'a ItemFn, info: &FixtureInfo) -> Errors<'a> {
    match (has_some_generics(test), info.arguments.get_once()) {
        (true, Some(once)) => Box::new(std::iter::once(syn::Error::new_spanned(
            once,
            "Cannot apply #[once] on generic fixture.",
        ))),
        _ => Box::new(std::iter::empty()),
    }
}

trait IsImplicitFixture {
    fn is_implicit_fixture(&self, pat: &Pat) -> bool;
}

impl IsImplicitFixture for FixtureInfo {
    fn is_implicit_fixture(&self, pat: &Pat) -> bool {
        !self.data.fixtures().any(|f| &f.arg == pat)
    }
}

impl IsImplicitFixture for RsTestInfo {
    fn is_implicit_fixture(&self, pat: &Pat) -> bool {
        !self.data.case_args().any(|a| a == pat)
            && !self.data.list_values().any(|a| &a.arg == pat)
            && !self.data.fixtures().any(|f| &f.arg == pat)
    }
}

fn destruct_fixture_without_from<'a>(
    function: &'a ItemFn,
    info: &'a impl IsImplicitFixture,
) -> Errors<'a> {
    Box::new(
        function
            .sig
            .inputs
            .iter()
            .filter_map(|a| a.maybe_pat().map(|p| (a, p)))
            .filter(|&(_, p)| p.maybe_ident().is_none())
            .filter(|&(_, p)| info.is_implicit_fixture(p))
            .map(|(a, _)| syn::Error::new_spanned(a, messages::DESTRUCT_WITHOUT_FROM)),
    )
}

#[derive(Debug, Default)]
pub struct ErrorsVec(Vec<syn::Error>);

pub(crate) fn _merge_errors<R1, R2>(
    r1: Result<R1, ErrorsVec>,
    r2: Result<R2, ErrorsVec>,
) -> Result<(R1, R2), ErrorsVec> {
    match (r1, r2) {
        (Ok(r1), Ok(r2)) => Ok((r1, r2)),
        (Ok(_), Err(e)) | (Err(e), Ok(_)) => Err(e),
        (Err(mut e1), Err(mut e2)) => {
            e1.append(&mut e2);
            Err(e1)
        }
    }
}

macro_rules! merge_errors {
    ($e:expr) => {
        $e
    };
    ($e:expr, $($es:expr), + $(,)?) => {
        crate::error::_merge_errors($e, merge_errors!($($es),*))
    };
}

macro_rules! composed_tuple {
    ($i:ident) => {
        $i
    };
    ($i:ident, $($is:ident), +) => {
        ($i, composed_tuple!($($is),*))
    };
}

impl std::ops::Deref for ErrorsVec {
    type Target = Vec<syn::Error>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for ErrorsVec {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<syn::Error> for ErrorsVec {
    fn from(errors: syn::Error) -> Self {
        vec![errors].into()
    }
}

impl From<Vec<syn::Error>> for ErrorsVec {
    fn from(errors: Vec<syn::Error>) -> Self {
        Self(errors)
    }
}

impl From<ErrorsVec> for Vec<syn::Error> {
    fn from(v: ErrorsVec) -> Self {
        v.0
    }
}

impl quote::ToTokens for ErrorsVec {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(self.0.iter().map(|e| e.to_compile_error()))
    }
}

impl From<ErrorsVec> for proc_macro::TokenStream {
    fn from(v: ErrorsVec) -> Self {
        use quote::ToTokens;
        v.into_token_stream().into()
    }
}

type Errors<'a> = Box<dyn Iterator<Item = syn::Error> + 'a>;

fn missed_arguments<'a, I: MaybePat + Spanned + 'a>(
    test: &'a ItemFn,
    args: impl Iterator<Item = &'a I> + 'a,
) -> Errors<'a> {
    Box::new(
        args.filter_map(|it| it.maybe_pat().map(|pat| (it, pat)))
            .filter(move |(_, pat)| !fn_args_has_pat(test, pat))
            .map(|(missed, pat)| {
                syn::Error::new(
                    missed.span(),
                    format!(
                        "Missed argument: '{}' should be a test function argument.",
                        pat.render_type()
                    ),
                )
            }),
    )
}

fn duplicate_arguments<'a, I: MaybePat + Spanned + 'a>(
    args: impl Iterator<Item = &'a I> + 'a,
) -> Errors<'a> {
    let mut used = HashMap::new();
    Box::new(
        args.filter_map(|it| it.maybe_pat().map(|pat| (it, pat)))
            .filter_map(move |(it, pat)| {
                let is_duplicate = used.contains_key(&pat);
                used.insert(pat, it);
                match is_duplicate {
                    true => Some((it, pat)),
                    false => None,
                }
            })
            .map(|(duplicate, pat)| {
                syn::Error::new(
                    duplicate.span(),
                    format!(
                        "Duplicate argument: '{}' is already defined.",
                        pat.render_type()
                    ),
                )
            }),
    )
}

fn invalid_cases(info: &RsTestInfo) -> Errors<'_> {
    let case_args = info.data.case_args_with_fallback().collect::<Vec<_>>();
    let has_fallbacks = case_args.iter().any(|(_, fallback)| fallback.is_some());
    Box::new(
        info.data
            .cases()
            .flat_map(move |case| invalid_case(&case_args, has_fallbacks, case)),
    )
}

const OMIT_CASE_VALUES: &str =
    "only #[case(opt)], #[case(default)] or #[case(default(...))] arguments can be omitted.";

fn invalid_case(
    case_args: &[(&Pat, Option<&CaseArgFallback>)],
    has_fallbacks: bool,
    case: &TestCase,
) -> Vec<syn::Error> {
    let wrong_signature = || {
        case_error(
            case,
            "Wrong case signature: should match the given parameters list.",
        )
    };
    if case.args.len() > case_args.len() {
        return vec![wrong_signature()];
    }
    let mut errors = Vec::new();
    let mut missed = Vec::new();
    for (i, (arg, _)) in case_args
        .iter()
        .enumerate()
        .filter(|(_, (_, fallback))| fallback.is_none())
    {
        match case.args.get(i) {
            Some(Expr::Infer(infer)) => errors.push(syn::Error::new_spanned(
                infer,
                format!("Cannot omit '{}' value: {OMIT_CASE_VALUES}", arg_name(arg)),
            )),
            Some(_) => {}
            None => missed.push(arg_name(arg)),
        }
    }
    match missed.as_slice() {
        [] => {}
        _ if !has_fallbacks => errors.push(wrong_signature()),
        [name] => errors.push(case_error(
            case,
            &format!("Missing '{name}' value: {OMIT_CASE_VALUES}"),
        )),
        names => errors.push(case_error(
            case,
            &format!(
                "Missing {} values: {OMIT_CASE_VALUES}",
                names
                    .iter()
                    .map(|name| format!("'{name}'"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        )),
    }
    errors
}

/// Point to the case values or to the parentheses if the case is empty
fn case_error(case: &TestCase, message: &str) -> syn::Error {
    if case.args.is_empty() {
        syn::Error::new(case.paren.span.join(), message)
    } else {
        syn::Error::new_spanned(case, message)
    }
}

fn arg_name(arg: &Pat) -> String {
    arg.maybe_ident()
        .map(ToString::to_string)
        .unwrap_or_else(|| {
            // Compact the tokens spacing (e.g. `T (x , y)` -> `T(x, y)`)
            arg.to_token_stream()
                .to_string()
                .replace(" ,", ",")
                .replace(" (", "(")
                .replace("( ", "(")
                .replace(" )", ")")
        })
}

fn case_fallbacks_on_future_args(info: &RsTestInfo) -> Errors<'_> {
    Box::new(
        info.data
            .case_args_with_fallback()
            .filter(|(arg, _)| info.arguments.is_future(arg))
            .filter_map(|(_, fallback)| match fallback {
                Some(CaseArgFallback::Optional(opt)) => Some(syn::Error::new_spanned(
                    opt,
                    "Cannot use #[case(opt)] with #[future] arguments.",
                )),
                Some(CaseArgFallback::Default(default)) => Some(syn::Error::new_spanned(
                    default,
                    "Cannot use #[case(default)] with #[future] arguments: \
                    use #[case(default(<future expression>))] instead.",
                )),
                _ => None,
            }),
    )
}

fn case_args_without_cases(params: &RsTestData) -> Errors<'_> {
    if !params.has_cases() {
        return Box::new(
            params
                .case_args()
                .map(|a| syn::Error::new(a.span(), "No cases for this argument.")),
        );
    }
    Box::new(std::iter::empty())
}

trait RenderType {
    fn render_type(&self) -> String;
}

impl RenderType for syn::Pat {
    fn render_type(&self) -> String {
        match self {
            syn::Pat::Ident(ref i) => i.ident.to_string(),
            other => format!("{other:?}"),
        }
    }
}

fn test_attributes<'a>(test: &'a ItemFn, arguments: &'a ArgumentsInfo) -> Errors<'a> {
    Box::new(
        async_test_without_test_attribute(test, arguments)
            .chain(malformed_explicit_test_attr(test)),
    )
}

fn async_test_without_test_attribute<'a>(
    test: &'a ItemFn,
    arguments: &'a ArgumentsInfo,
) -> Errors<'a> {
    if test.sig.asyncness.is_some() && arguments.test_attr().is_none() {
        let span = test.sig.ident.span();
        Box::new(std::iter::once(syn::Error::new(span, "async test requires either explicit `test_attr` or implicit (attribute path ends with `test`)")))
    } else {
        Box::new(std::iter::empty())
    }
}

fn malformed_explicit_test_attr(test: &ItemFn) -> Errors<'_> {
    let Some(explicit_test_attr) = test.attrs.iter().find(|attr| attr_is(attr, "test_attr")) else {
        return Box::new(std::iter::empty());
    };
    match explicit_test_attr.meta {
        syn::Meta::List(_) => Box::new(std::iter::empty()),
        syn::Meta::Path(_) | syn::Meta::NameValue(_) => {
            Box::new(std::iter::once(syn::Error::new_spanned(
                explicit_test_attr.path(),
                "invalid `test_attr` syntax; should be `#[test_attr(<test attribute>)]`",
            )))
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{
        parse::ExtendWithFunctionAttrs,
        test::{assert_eq, *},
    };
    use rstest_test::assert_in;

    use super::*;

    #[rstest]
    #[case::generics("fn f<G: SomeTrait>(){}")]
    #[case::const_generics("fn f<const N: usize>(){}")]
    #[case::lifetimes("fn f<'a>(){}")]
    #[case::use_impl_in_answer("fn f() -> impl Iterator<Item=u32>{}")]
    #[case::use_impl_in_arguments("fn f(it: impl Iterator<Item=u32>){}")]
    #[should_panic]
    #[case::sanity_check_with_no_generics("fn f() {}")]
    fn generics_once_should_return_error(#[case] f: &str) {
        let f: ItemFn = f.ast();
        let info = FixtureInfo::default().with_once();

        let errors = generics_once(&f, &info);

        let out = errors
            .map(|e| format!("{:?}", e))
            .collect::<Vec<_>>()
            .join("-----------------------\n");

        assert_in!(out, "Cannot apply #[once] on generic fixture.");
    }

    #[rstest]
    #[case::generics("fn f<G: SomeTrait>(){}")]
    #[case::const_generics("fn f<const N: usize>(){}")]
    #[case::lifetimes("fn f<'a>(){}")]
    #[case::use_impl_in_answer("fn f() -> impl Iterator<Item=u32>{}")]
    #[case::use_impl_in_arguments("fn f(it: impl Iterator<Item=u32>){}")]
    fn generics_once_should_not_return_if_no_once(#[case] f: &str) {
        let f: ItemFn = f.ast();
        let info = FixtureInfo::default();

        let errors = generics_once(&f, &info);

        assert_eq!(0, errors.count());
    }

    #[rstest]
    #[case::base_in_fixture("fn f(T{a}: T){}", FixtureInfo::default(), 1)]
    #[case::one_of_two("fn f(T{a}: T, #[from(f)] T{a: c}: T){}", FixtureInfo::default(), 1)]
    #[case::find_all(
        "fn f(T{a}: T, z: u32, S(a,b): S, x: u32, (f , g, h): (u32, String, f32)){}",
        FixtureInfo::default(),
        3
    )]
    #[case::base_in_test("fn f(T{a}: T){}", RsTestInfo::default(), 1)]
    #[case::not_case_or_values(
        "fn f(#[case] T{a}: T, #[values(T::a(),T::b())] T{v}: T, S{e}: S){}",
        RsTestInfo::default(),
        1
    )]
    #[case::mixed_more(
        r#"fn wrong_destruct_fixture(
            T(a, b): T,
            #[case] T(e, f): T,
            #[values(T(1, 2))] T(g, h): T,
        ) {}"#,
        RsTestInfo::default(),
        1
    )]
    fn destruct_implicit_from_should_return_error(
        #[case] f: &str,
        #[case] mut info: impl ExtendWithFunctionAttrs + IsImplicitFixture,
        #[case] n: usize,
    ) {
        let mut f: ItemFn = f.ast();

        info.extend_with_function_attrs(&mut f).unwrap();

        let errors = destruct_fixture_without_from(&f, &info);

        let out = errors
            .map(|e| format!("{:?}", e))
            .collect::<Vec<_>>()
            .join("\n");

        assert_in!(out, messages::DESTRUCT_WITHOUT_FROM);
        assert_eq!(n, out.lines().count())
    }

    #[rstest]
    #[case::happy_fixture("fn f(#[from(b)] T{a}: T){}", FixtureInfo::default())]
    #[case::happy_test("fn f(#[from(b)] T{a}: T){}", RsTestInfo::default())]
    #[case::some_cases_or_values(
        "fn f(#[case] T{a}: T, #[values(T::a(),T::b())] T{v}: T){}",
        RsTestInfo::default()
    )]
    fn destruct_not_implicit_should_not_return_error(
        #[case] f: &str,
        #[case] mut info: impl ExtendWithFunctionAttrs + IsImplicitFixture,
    ) {
        let mut f: ItemFn = f.ast();

        info.extend_with_function_attrs(&mut f).unwrap();

        let errors = destruct_fixture_without_from(&f, &info);

        assert_eq!(0, errors.count());
    }

    fn rstest_info(f: &str) -> RsTestInfo {
        let mut f: ItemFn = f.ast();
        let mut info = RsTestInfo::default();
        info.extend_with_function_attrs(&mut f).unwrap();
        info
    }

    #[rstest]
    #[case::all_values("#[case(1, 2)] fn f(#[case] a: u32, #[case] b: u32) {}")]
    #[case::omit_trailing("#[case(1)] fn f(#[case] a: u32, #[case(default)] b: u32) {}")]
    #[case::omit_more(
        "#[case(1)] fn f(#[case] a: u32, #[case(opt)] b: Option<u32>, #[case(default(3))] c: u32) {}"
    )]
    #[case::infer("#[case(_, 2)] fn f(#[case(default)] a: u32, #[case] b: u32) {}")]
    #[case::empty("#[case()] fn f(#[case(default)] a: u32, #[case(opt)] b: Option<u32>) {}")]
    #[case::infer_inside_expression("#[case(Some(_))] fn f(#[case] a: Option<u32>) {}")]
    fn valid_cases_should_not_return_errors(#[case] f: &str) {
        let info = rstest_info(f);

        assert_eq!(0, invalid_cases(&info).count());
    }

    #[rstest]
    #[case::too_many_values(
        "#[case(1, 2, 3)] fn f(#[case] a: u32, #[case(default)] b: u32) {}",
        &["Wrong case signature"]
    )]
    #[case::too_few_values_without_fallbacks(
        "#[case(1)] fn f(#[case] a: u32, #[case] b: u32) {}",
        &["Wrong case signature"]
    )]
    #[case::missed_value(
        "#[case(1)] fn f(#[case(opt)] a: Option<u32>, #[case] b: u32) {}",
        &["Missing 'b' value: only #[case(opt)]"]
    )]
    #[case::missed_values(
        "#[case()] fn f(#[case(opt)] a: Option<u32>, #[case] b: u32, #[case] mut c: u32) {}",
        &["Missing 'b', 'c' values: only #[case(opt)]"]
    )]
    #[case::infer_without_fallback(
        "#[case(_, 2)] fn f(#[case] a: u32, #[case(default)] b: u32) {}",
        &["Cannot omit 'a' value: only #[case(opt)]"]
    )]
    #[case::more_infer_without_fallback(
        "#[case(_, _)] fn f(#[case] a: u32, #[case] b: u32) {}",
        &["Cannot omit 'a' value", "Cannot omit 'b' value"]
    )]
    #[case::infer_and_missed(
        "#[case(_)] fn f(#[case] a: u32, #[case(default)] b: u32, #[case] c: u32) {}",
        &["Cannot omit 'a' value", "Missing 'c' value"]
    )]
    #[case::destruct(
        "#[case()] fn f(#[case(default)] a: u32, #[case] T(x, y): T) {}",
        &["Missing 'T(x, y)' value"]
    )]
    fn invalid_cases_should_return_errors(#[case] f: &str, #[case] messages: &[&str]) {
        let info = rstest_info(f);

        let errors = invalid_cases(&info)
            .map(|e| e.to_string())
            .collect::<Vec<_>>();

        assert_eq!(messages.len(), errors.len(), "{errors:?}");
        for (error, message) in errors.iter().zip(messages) {
            assert_in!(error, *message);
        }
    }

    #[rstest]
    #[case::opt(
        "#[case(_)] fn f(#[future] #[case(opt)] a: Option<u32>) {}",
        Some("Cannot use #[case(opt)] with #[future] arguments.")
    )]
    #[case::default(
        "#[case(_)] fn f(#[future] #[case(default)] a: u32) {}",
        Some("Cannot use #[case(default)] with #[future] arguments")
    )]
    #[case::default_expression(
        "#[case(_)] fn f(#[future] #[case(default(async { 42 }))] a: u32) {}",
        None
    )]
    #[case::not_future("#[case(_)] fn f(#[case(opt)] a: Option<u32>) {}", None)]
    fn case_fallbacks_on_future_args_should(#[case] f: &str, #[case] message: Option<&str>) {
        let info = rstest_info(f);

        let errors = case_fallbacks_on_future_args(&info)
            .map(|e| e.to_string())
            .collect::<Vec<_>>();

        match message {
            Some(message) => {
                assert_eq!(1, errors.len());
                assert_in!(errors[0], message);
            }
            None => assert!(errors.is_empty(), "{errors:?}"),
        }
    }
}
