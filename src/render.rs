use std::collections::HashMap;

use itertools::Itertools;
use proc_macro2::{Span, TokenStream};
use syn::{FnArg, Generics, Ident, ItemFn, parse_quote, ReturnType, Stmt};

use quote::quote;

use crate::refident::MaybeIdent;
use crate::resolver::{self, arg_2_fixture, Resolver};
use crate::parse::{
    rstest::{RsTestInfo, RsTestData, RsTestAttributes},
    testcase::TestCase,
    fixture::FixtureInfo
};
use crate::fn_args;

fn trace_arguments<'a>(args: impl Iterator<Item=&'a Ident>, attributes: &RsTestAttributes) -> Option<proc_macro2::TokenStream> {
    let mut statements = args
        .filter(|&arg| attributes.trace_me(arg))
        .map(|arg|
            parse_quote! {
                println!("{} = {:?}", stringify!(#arg), #arg);
            }
        )
        .map(|stmt: Stmt| stmt)
        .peekable();
    if statements.peek().is_some() {
        Some(
            quote! {
                println!("{:-^40}", " TEST ARGUMENTS ");
                #(#arguments)*
            }
        )
    } else {
        None
    }
}

fn resolve_args<'a>(args: impl Iterator<Item=&'a Ident>, resolver: &impl Resolver) -> TokenStream {
    let define_vars = args
        .map(|arg|
            arg_2_fixture(arg, resolver)
        );
    quote! {
        #(#define_vars)*
    }
}

fn resolve_fn_args<'a>(args: impl Iterator<Item=&'a FnArg>, resolver: &impl Resolver) -> TokenStream {
    resolve_args(args.filter_map(MaybeIdent::maybe_ident), resolver)
}

fn render_fn_test<'a>(name: Ident, testfn: &ItemFn, test_impl: Option<&ItemFn>,
                      resolver: impl Resolver, attributes: &'a RsTestAttributes)
                      -> TokenStream {
    let testfn_name = &testfn.sig.ident;
    let args = fn_args_idents(&testfn).cloned().collect::<Vec<_>>();
    let attrs = &testfn.attrs;
    let output = &testfn.sig.output;
    let inject = resolve_fn_args(fn_args(&testfn), &resolver);
    let trace_args = trace_arguments(args.iter(), attributes);
    quote! {
        #[test]
        #(#attrs)*
        fn #name() #output {
            #test_impl
            #inject
            #trace_args
            println!("{:-^40}", " TEST START ");
            #testfn_name(#(#args),*)
        }
    }
}

fn where_predicate_bounded_type(wp: &syn::WherePredicate) -> Option<&syn::Type> {
    match wp {
        syn::WherePredicate::Type(pt) => {
            Some(&pt.bounded_ty)
        }
        _ => None
    }
}

//noinspection RsTypeCheck
fn generics_clean_up<'a>(original: &Generics, inputs: impl Iterator<Item=&'a FnArg>, output: &ReturnType) -> syn::Generics {
    use syn::visit::Visit;
    #[derive(Default, Debug)]
    struct Used(std::collections::HashSet<proc_macro2::Ident>);
    impl<'ast> syn::visit::Visit<'ast> for Used {
        fn visit_type_path(&mut self, i: &'ast syn::TypePath) {
            if i.qself.is_none() && i.path.leading_colon.is_none() && i.path.segments.len() == 1 {
                self.0.insert(i.path.segments.first().unwrap().ident.clone());
            }
        }
    }
    let mut outs = Used::default();
    outs.visit_return_type(output);
    inputs.for_each(|fn_arg| outs.visit_fn_arg(fn_arg));
    let mut result: Generics = original.clone();
    result.params = result.params.into_iter().filter(|p|
        match p {
            syn::GenericParam::Type(tp) if !outs.0.contains(&tp.ident) => false,
            _ => true,
        }
    ).collect();
    result.where_clause.as_mut().map(
        |mut w| w.predicates = w.predicates.clone()
            .into_iter()
            .filter(|wp| where_predicate_bounded_type(wp)
                .and_then(MaybeIdent::maybe_ident)
                .map(|t| outs.0.contains(t))
                .unwrap_or(true)
            ).collect()
    );
    result
}

fn render_partial_impl(fixture: &ItemFn, n: usize, resolver: &impl Resolver, info: &FixtureInfo) -> TokenStream {
    let fixture_inputs = &fixture.sig.inputs;

    let output = info.attributes.extract_partial_type(n).unwrap_or(fixture.sig.output.clone());

    let generics = generics_clean_up(&fixture.sig.generics, fixture_inputs.iter().take(n), &output);
    let where_clause = &generics.where_clause;

    let to_resolve_args = fixture_inputs.iter().skip(n).filter_map(MaybeIdent::maybe_ident);
    let inject = resolve_args(to_resolve_args, resolver);

    let sign_args = fixture_inputs.iter().take(n);
    let fixture_args = fixture_inputs.iter().filter_map(MaybeIdent::maybe_ident);
    let name = Ident::new(&format!("partial_{}", n), Span::call_site());

    quote! {
        pub fn #name #generics (#(#sign_args),*) #output #where_clause {
            #inject
            Self::get(#(#fixture_args),*)
        }
    }
}

pub(crate) fn render_fixture<'a>(fixture: ItemFn, info: FixtureInfo)
                                 -> TokenStream {
    let name = &fixture.sig.ident;
    let vargs = fn_args_idents(&fixture).cloned().collect::<Vec<_>>();
    let args = &vargs;
    let orig_args = &fixture.sig.inputs;
    let orig_attrs = &fixture.attrs;
    let generics = &fixture.sig.generics;
    let default_output = info.attributes.extract_default_type().unwrap_or(fixture.sig.output.clone());
    let default_generics = generics_clean_up(&fixture.sig.generics, std::iter::empty(), &default_output);
    let default_where_clause = &default_generics.where_clause;
    let body = &fixture.block;
    let where_clause = &fixture.sig.generics.where_clause;
    let output = &fixture.sig.output;
    let visibility = &fixture.vis;
    let resolver = resolver::fixture_resolver(info.data.fixtures());
    let inject = resolve_fn_args(fn_args(&fixture), &resolver);
    let partials = (1..=orig_args.len())
        .map(|n| render_partial_impl(&fixture, n, &resolver, &info)
        );
    quote! {
        #[allow(non_camel_case_types)]
        #visibility struct #name {}

        impl #name {
            #(#orig_attrs)*
            pub fn get #generics (#orig_args) #output #where_clause {
                #body
            }

            pub fn default #default_generics () #default_output #default_where_clause {
                #inject
                Self::get(#(#args),*)
            }

            #(#partials)*
        }

        #[allow(dead_code)]
        #fixture
    }
}

fn fn_args_idents(test: &ItemFn) -> impl Iterator<Item=&Ident> {
    fn_args(&test)
        .filter_map(MaybeIdent::maybe_ident)
}

pub(crate) fn render_single_case(test: ItemFn, info: RsTestInfo) -> TokenStream {
    let name = &test.sig.ident;
    let resolver = resolver::fixture_resolver(info.data.fixtures());

    render_fn_test(name.clone(), &test, Some(&test), resolver, &info.attributes)
}

struct TestCaseRender<R: Resolver> {
    name: Ident,
    resolver: R,
}

impl<R: Resolver> TestCaseRender<R> {
    pub fn new(name: Ident, resolver: R) -> Self {
        TestCaseRender { name, resolver }
    }

    fn render(self, testfn: &ItemFn, attributes: &RsTestAttributes) -> TokenStream {
        render_fn_test(self.name, testfn, None, self.resolver, attributes)
    }
}

fn render_test_group(test: ItemFn, rendered_cases: TokenStream) -> TokenStream {
    let fname = &test.sig.ident;

    quote! {
        #[cfg(test)]
        #test

        #[cfg(test)]
        mod #fname {
            use super::*;

            #rendered_cases
        }
    }
}

trait DisplayLen {
    fn display_len(&self) -> usize;
}

impl<D: std::fmt::Display> DisplayLen for D {
    fn display_len(&self) -> usize {
        format!("{}", self).len()
    }
}

fn format_case_name(case: &TestCase, index: usize, display_len: usize) -> String {
    let description = case
        .description.as_ref()
        .map(|d| format!("_{}", d))
        .unwrap_or_default();
    format!("case_{:0len$}{d}", index, len = display_len, d = description)
}

fn cases_data(data: &RsTestData, name_span: Span) -> impl Iterator<Item=(Ident, HashMap<String, &syn::Expr>)> {
    let display_len = data.cases().count().display_len();
    data.cases()
        .enumerate()
        .map({
            move |(n, case)|
                {
                    let resolver_case = data.case_args()
                        .map(|a| a.to_string())
                        .zip(case.args.iter())
                        .collect::<HashMap<_, _>>();
                    (Ident::new(&format_case_name(case, n + 1, display_len), name_span), resolver_case)
                }
        }
        )
}

pub(crate) fn render_parametrize_cases(test: ItemFn, info: RsTestInfo) -> TokenStream {
    let RsTestInfo { data, attributes } = info;
    let resolver_fixtures = resolver::fixture_resolver(data.fixtures());

    let rendered_cases = cases_data(&data, test.sig.ident.span())
        .map(|(name, resolver)|
            TestCaseRender::new(name, (resolver, &resolver_fixtures))
        )
        .map(|case| case.render(&test, &attributes))
        .collect();

    render_test_group(test, rendered_cases)
}

fn render_inner_matrix_cases<'a>(test: &ItemFn, attributes: &RsTestAttributes, resolver: &impl Resolver,
                                 list_values: impl Iterator<Item=&'a crate::parse::vlist::ValueList>) -> TokenStream {
    let test_function_span = test.sig.ident.span();
    // Steps:
    // 1. pack data P=(ident, expr, (pos, max_len)) in one iterator for each variable
    // 2. do a cartesian product of iterators to build all cases (every case is a vector of P)
    // 3. format case by packed data vector
    let test_cases = list_values
        .map(|group|
            group.values.iter()
                .enumerate()
                .map(move |(pos, expr)| (&group.arg, expr, (pos, group.values.len())))
        )
        .multi_cartesian_product()
        .map(|c| {
            let args_indexes = c.iter()
                .map(|(_, _, (index, max))|
                    format!("{:0len$}", index + 1, len = max.display_len())
                )
                .collect::<Vec<_>>()
                .join("_");
            let name = format!("case_{}", args_indexes);
            let resolver_case = c.into_iter()
                .map(|(a, e, _)| (a.to_string(), e))
                .collect::<HashMap<_, _>>();
            TestCaseRender::new(Ident::new(&name, test_function_span),
                                (resolver_case, resolver))
        }
        );

    let test_cases = test_cases.map(|test_case| test_case.render(test, attributes));

    quote! { #(#test_cases)* }
}

trait WrapByModule {
    fn wrap_by_mod(&self, mod_name: &Ident) -> TokenStream;
}

impl<T: quote::ToTokens> WrapByModule for T {
    fn wrap_by_mod(&self, mod_name: &Ident) -> TokenStream {
        quote! {
            mod #mod_name {
                use super::*;

                #self
            }
        }
    }
}

pub(crate) fn render_matrix_cases(test: ItemFn, info: RsTestInfo) -> TokenStream {
    let RsTestInfo { data, attributes, .. } = info;
    let span = test.sig.ident.span();

    let cases = cases_data(&data, span)
        .collect::<Vec<_>>();

    let resolver = resolver::fixture_resolver(data.fixtures());
    let rendered_cases = if cases.is_empty() {
        render_inner_matrix_cases(&test, &attributes, &resolver, data.list_values())
    } else {
        cases.into_iter().map(
            |(case_name, case_resolver)|
                render_inner_matrix_cases(&test, &attributes, &(case_resolver, &resolver), data.list_values())
                    .wrap_by_mod(&case_name)
        ).collect()
    };

    render_test_group(test, rendered_cases)
}

#[cfg(test)]
mod test {
    use syn::{
        ItemFn, ItemMod, parse::{Parse, ParseStream, Result}, parse2,
        parse_str, visit::Visit,
    };

    use crate::resolver::*;
    use crate::test::{*, assert_eq, fixture};

    use super::*;

    fn first_arg_ident(ast: &ItemFn) -> &Ident {
        fn_args(&ast)
            .next().unwrap()
            .maybe_ident().unwrap()
    }

    mod arg_2_fixture_should {
        use super::{*, assert_eq};

        #[test]
        fn call_arg() {
            let ast = "fn foo(fix: String) {}".ast();
            let arg = first_arg_ident(&ast);

            let line = arg_2_fixture(arg, &EmptyResolver);

            assert_eq!(line, "let fix = fix::default();".ast());
        }

        #[test]
        fn not_add_mut() {
            let ast = "fn foo(mut fix: String) {}".ast();
            let arg = first_arg_ident(&ast);

            let line = arg_2_fixture(arg, &EmptyResolver);

            assert_eq!(line, "let fix = fix::default();".ast());
        }

        #[test]
        fn use_passed_fixture_if_any() {
            let ast = "fn foo(mut fix: String) {}".ast();
            let arg = first_arg_ident(&ast);
            let call = expr("bar()");
            let mut resolver = HashMap::new();
            resolver.insert("fix".to_string(), &call);

            let line = arg_2_fixture(arg, &resolver);

            assert_eq!(line, "let fix = bar();".ast());
        }
    }

    mod resolver_should {
        use super::{*, assert_eq};

        #[test]
        fn return_the_given_expression() {
            let ast = parse_str("fn function(mut foo: String) {}").unwrap();
            let arg = first_arg_ident(&ast);
            let expected = expr("bar()");
            let mut resolver = HashMap::new();

            resolver.insert("foo".to_string(), &expected);

            assert_eq!(expected, (&resolver).resolve(&arg).unwrap().into_owned())
        }

        #[test]
        fn return_none_for_unknown_argument() {
            let ast = "fn function(mut fix: String) {}".ast();
            let arg = first_arg_ident(&ast);

            assert!(EmptyResolver.resolve(&arg).is_none())
        }
    }

    mod fn_test_should {
        use pretty_assertions::assert_eq;
        use proc_macro2::Span;

        use super::*;

        #[test]
        fn add_return_type_if_any() {
            let ast: ItemFn = "fn function(mut fix: String) -> Result<i32, String> { Ok(42) }".ast();

            let tokens = render_fn_test(Ident::new("new_name", Span::call_site()),
                                        &ast, None, EmptyResolver, &Default::default());

            let result: ItemFn = parse2(tokens).unwrap();

            assert_eq!(result.sig.ident.to_string(), "new_name");
            assert_eq!(result.sig.output, ast.sig.output);
        }

        #[test]
        fn include_given_function() {
            let input_fn: ItemFn = r#"
                pub fn test<R: AsRef<str>, B>(mut s: String, v: &u32, a: &mut [i32], r: R) -> (u32, B, String, &str)
                        where B: Borrow<u32>
                {
                    let some = 42;
                    assert_eq!(42, some);
                }
                "#.ast();

            let tokens = render_fn_test(Ident::new("new_name", Span::call_site()),
                                        &input_fn, Some(&input_fn), EmptyResolver, &Default::default());

            let result: ItemFn = parse2(tokens).unwrap();
            let first_stmt = result.block.stmts.get(0).unwrap();

            let inner_fn: ItemFn = parse_quote! {
                        #first_stmt
                    };

            assert_eq!(inner_fn, inner_fn);
        }
    }

    struct TestsGroup {
        requested_test: ItemFn,
        module: ItemMod,
    }

    impl Parse for TestsGroup {
        fn parse(input: ParseStream) -> Result<Self> {
            Ok(Self {
                requested_test: input.parse()?,
                module: input.parse()?,
            })
        }
    }

    /// To extract all test functions
    struct TestFunctions(Vec<ItemFn>);

    impl TestFunctions {
        fn is_test_fn(item_fn: &ItemFn) -> bool {
            item_fn.attrs.iter().filter(|&a|
                a.path == parse_str::<syn::Path>("test").unwrap())
                .next().is_some()
        }
    }

    impl<'ast> Visit<'ast> for TestFunctions {
        //noinspection RsTypeCheck
        fn visit_item_fn(&mut self, item_fn: &'ast ItemFn) {
            if Self::is_test_fn(item_fn) {
                self.0.push(item_fn.clone())
            }
        }
    }

    /// To extract all submodules
    struct SubModules(Vec<ItemMod>);

    impl<'ast> Visit<'ast> for SubModules {
        //noinspection RsTypeCheck
        fn visit_item_mod(&mut self, item_mod: &'ast ItemMod) {
            self.0.push(item_mod.clone())
        }
    }

    trait ModuleInspector {
        fn get_test_functions(&self) -> Vec<ItemFn>;
        fn get_submodules(&self) -> Vec<ItemMod>;
    }

    impl ModuleInspector for ItemMod {
        fn get_test_functions(&self) -> Vec<ItemFn> {
            let mut f = TestFunctions(vec![]);
            f.visit_item_mod(&self);
            f.0
        }

        fn get_submodules(&self) -> Vec<ItemMod> {
            let mut f = SubModules(vec![]);

            self.content.as_ref().map(|(_, items)|
                items.iter().for_each(|it| f.visit_item(it))
            );
            f.0
        }
    }

    impl ModuleInspector for TestsGroup {
        fn get_test_functions(&self) -> Vec<ItemFn> {
            self.module.get_test_functions()
        }

        fn get_submodules(&self) -> Vec<ItemMod> {
            self.module.get_submodules()
        }
    }

    #[derive(Default, Debug)]
    struct Assignments(HashMap<String, syn::Expr>);

    impl<'ast> Visit<'ast> for Assignments {
        //noinspection RsTypeCheck
        fn visit_local(&mut self, assign: &syn::Local) {
            match &assign {
                syn::Local { pat: syn::Pat::Ident(pat), init: Some((_, expr)), .. } => {
                    self.0.insert(pat.ident.to_string(), expr.as_ref().clone());
                }
                _ => {}
            }
        }
    }

    impl Assignments {
        pub fn collect_assignments(item_fn: &ItemFn) -> Self {
            let mut collect = Self::default();
            collect.visit_item_fn(item_fn);
            collect
        }
    }

    impl From<TokenStream> for TestsGroup {
        fn from(tokens: TokenStream) -> Self {
            syn::parse2::<TestsGroup>(tokens).unwrap()
        }
    }

    mod cases_should {
        use crate::parse::{
            rstest::{RsTestData, RsTestInfo, RsTestItem},
            testcase::TestCase,
        };

        use super::{*, assert_eq};

        fn into_rstest_data(item_fn: &ItemFn) -> RsTestData {
            RsTestData {
                items: fn_args_idents(item_fn)
                    .cloned()
                    .map(RsTestItem::CaseArgName)
                    .collect(),
            }
        }

        fn one_simple_case() -> (ItemFn, RsTestInfo) {
            let item_fn = parse_str::<ItemFn>(
                r#"fn test(mut fix: String) { println!("user code") }"#
            ).unwrap();
            let mut info: RsTestInfo = into_rstest_data(&item_fn).into();
            info.push_case(TestCase::from(r#"String::from("3")"#));
            (item_fn, info)
        }

        fn some_simple_cases(cases: i32) -> (ItemFn, RsTestInfo) {
            let item_fn = parse_str::<ItemFn>(
                r#"fn test(mut fix: String) { println!("user code") }"#
            ).unwrap();
            let mut info: RsTestInfo = into_rstest_data(&item_fn).into();
            info.extend((0..cases).map(|_| TestCase::from(r#"String::from("3")"#)));
            (item_fn, info)
        }

        #[test]
        fn create_a_module_named_as_test_function() {
            let item_fn = parse_str::<ItemFn>("fn should_be_the_module_name(mut fix: String) {}").unwrap();
            let data = into_rstest_data(&item_fn);
            let tokens = render_parametrize_cases(item_fn.clone(), data.into());

            let output = TestsGroup::from(tokens);

            assert_eq!(output.module.ident, "should_be_the_module_name");
        }

        #[test]
        fn copy_user_function() {
            let item_fn = parse_str::<ItemFn>(
                r#"fn should_be_the_module_name(mut fix: String) { println!("user code") }"#
            ).unwrap();
            let data = into_rstest_data(&item_fn);
            let tokens = render_parametrize_cases(item_fn.clone(), data.into());

            let mut output = TestsGroup::from(tokens);

            output.requested_test.attrs = vec![];
            assert_eq!(output.requested_test, item_fn);
        }

        #[test]
        fn mark_user_function_as_test() {
            let item_fn = parse_str::<ItemFn>(
                r#"fn should_be_the_module_name(mut fix: String) { println!("user code") }"#
            ).unwrap();
            let data = into_rstest_data(&item_fn);
            let tokens = render_parametrize_cases(item_fn.clone(), data.into());

            let output = TestsGroup::from(tokens);

            let expected = parse2::<ItemFn>(quote! {
                    #[cfg(test)]
                    fn some() {}
                }).unwrap().attrs;

            assert_eq!(expected, output.requested_test.attrs);
        }

        #[test]
        fn mark_module_as_test() {
            let item_fn = parse_str::<ItemFn>(
                r#"fn should_be_the_module_name(mut fix: String) { println!("user code") }"#
            ).unwrap();
            let data = into_rstest_data(&item_fn);
            let tokens = render_parametrize_cases(item_fn.clone(), data.into());

            let output = TestsGroup::from(tokens);

            let expected = parse2::<ItemMod>(quote! {
                    #[cfg(test)]
                    mod some {}
                }).unwrap().attrs;

            assert_eq!(expected, output.module.attrs);
        }

        #[test]
        fn add_a_test_case() {
            let (item_fn, info) = one_simple_case();

            let tokens = render_parametrize_cases(item_fn.clone(), info);

            let tests = TestsGroup::from(tokens).get_test_functions();

            assert_eq!(1, tests.len());
            assert!(&tests[0].sig.ident.to_string().starts_with("case_"))
        }

        #[test]
        fn starts_case_number_from_1() {
            let (item_fn, info) = one_simple_case();

            let tokens = render_parametrize_cases(item_fn.clone(), info);

            let tests = TestsGroup::from(tokens).get_test_functions();

            assert!(&tests[0].sig.ident.to_string().starts_with("case_1"), "Should starts with case_1 but is {}", tests[0].sig.ident.to_string())
        }

        #[test]
        fn add_all_test_cases() {
            let (item_fn, info) = some_simple_cases(5);

            let tokens = render_parametrize_cases(item_fn.clone(), info);

            let tests = TestsGroup::from(tokens).get_test_functions();

            let valid_names = tests.iter()
                .filter(|it| it.sig.ident.to_string().starts_with("case_"));
            assert_eq!(5, valid_names.count())
        }

        #[test]
        fn left_pad_case_number_by_zeros() {
            let (item_fn, info) = some_simple_cases(1000);

            let tokens = render_parametrize_cases(item_fn.clone(), info);

            let tests = TestsGroup::from(tokens).get_test_functions();

            let first_name = tests[0].sig.ident.to_string();
            let last_name = tests[999].sig.ident.to_string();

            assert!(first_name.ends_with("_0001"), "Should ends by _0001 but is {}", first_name);
            assert!(last_name.ends_with("_1000"), "Should ends by _1000 but is {}", last_name);

            let valid_names = tests.iter()
                .filter(|it| it.sig.ident.to_string().len() == first_name.len());
            assert_eq!(1000, valid_names.count())
        }

        #[test]
        fn use_description_if_any() {
            let (item_fn, mut info) = one_simple_case();
            let description = "show_this_description";

            if let &mut RsTestItem::TestCase(ref mut case) = &mut info.data.items[1] {
                case.description = Some(parse_str::<Ident>(description).unwrap());
            } else {
                panic!("Test case should be the second one");
            }

            let tokens = render_parametrize_cases(item_fn.clone(), info);

            let tests = TestsGroup::from(tokens).get_test_functions();

            assert!(tests[0].sig.ident.to_string().ends_with(&format!("_{}", description)));
        }

        mod matrix_cases_should {
            use unindent::Unindent;

            use crate::parse::vlist::ValueList;

            /// Should test matrix tests render without take in account MatrixInfo to RsTestInfo
            /// transformation

            use super::{*, assert_eq};

            fn into_rstest_data(item_fn: &ItemFn) -> RsTestData {
                RsTestData {
                    items: fn_args_idents(item_fn)
                        .cloned()
                        .map(|it|
                            ValueList { arg: it, values: vec![] }.into()
                        )
                        .collect(),
                }
            }

            fn one_simple_case() -> (ItemFn, RsTestInfo) {
                let item_fn = parse_str::<ItemFn>(
                    r#"fn test(mut fix: String) { println!("user code") }"#
                ).unwrap();
                let info = RsTestInfo {
                    data: RsTestData {
                        items: vec![
                            ValueList { arg: ident("fix"), values: vec![expr(r#""value""#)] }.into()
                        ]
                    },
                    ..Default::default()
                };
                (item_fn, info)
            }

            #[test]
            fn create_a_module_named_as_test_function() {
                let item_fn = parse_str::<ItemFn>("fn should_be_the_module_name(mut fix: String) {}").unwrap();
                let data = into_rstest_data(&item_fn);
                let tokens = render_matrix_cases(item_fn.clone(), data.into());

                let output = TestsGroup::from(tokens);

                assert_eq!(output.module.ident, "should_be_the_module_name");
            }

            #[test]
            fn copy_user_function() {
                let item_fn = parse_str::<ItemFn>(
                    r#"fn should_be_the_module_name(mut fix: String) { println!("user code") }"#
                ).unwrap();
                let data = into_rstest_data(&item_fn);
                let tokens = render_matrix_cases(item_fn.clone(), data.into());

                let mut output = TestsGroup::from(tokens);

                output.requested_test.attrs = vec![];
                assert_eq!(output.requested_test, item_fn);
            }

            #[test]
            fn mark_user_function_as_test() {
                let item_fn = parse_str::<ItemFn>(
                    r#"fn should_be_the_module_name(mut fix: String) { println!("user code") }"#
                ).unwrap();
                let data = into_rstest_data(&item_fn);
                let tokens = render_matrix_cases(item_fn.clone(), data.into());

                let output = TestsGroup::from(tokens);

                let expected = parse2::<ItemFn>(quote! {
                            #[cfg(test)]
                            fn some() {}
                        }).unwrap().attrs;

                assert_eq!(expected, output.requested_test.attrs);
            }

            #[test]
            fn mark_module_as_test() {
                let item_fn = parse_str::<ItemFn>(
                    r#"fn should_be_the_module_name(mut fix: String) { println!("user code") }"#
                ).unwrap();
                let data = into_rstest_data(&item_fn);
                let tokens = render_matrix_cases(item_fn.clone(), data.into());

                let output = TestsGroup::from(tokens);

                let expected = parse2::<ItemMod>(quote! {
                    #[cfg(test)]
                    mod some {}
                    }
                ).unwrap().attrs;

                assert_eq!(expected, output.module.attrs);
            }

            #[test]
            fn add_a_test_case() {
                let (item_fn, info) = one_simple_case();

                let tokens = render_matrix_cases(item_fn.clone(), info);

                let tests = TestsGroup::from(tokens).get_test_functions();

                assert_eq!(1, tests.len());
                assert!(&tests[0].sig.ident.to_string().starts_with("case_"))
            }

            #[test]
            fn add_a_test_cases_from_all_combinations() {
                let item_fn = parse_str::<ItemFn>(
                    r#"fn test(first: u32, second: u32, third: u32) { println!("user code") }"#
                ).unwrap();
                let info = RsTestInfo {
                    data: RsTestData {
                        items: vec![
                            values_list("first", ["1", "2"].as_ref()).into(),
                            values_list("second", ["3", "4"].as_ref()).into(),
                            values_list("third", ["5", "6"].as_ref()).into(),
                        ]
                    },
                    ..Default::default()
                };

                let tokens = render_matrix_cases(item_fn.clone(), info);

                let tests = TestsGroup::from(tokens).get_test_functions();

                let tests = tests.into_iter()
                    .map(|t| t.sig.ident.to_string())
                    .collect::<Vec<_>>()
                    .join("\n");

                assert_eq!(tests, "
                    case_1_1_1
                    case_1_1_2
                    case_1_2_1
                    case_1_2_2
                    case_2_1_1
                    case_2_1_2
                    case_2_2_1
                    case_2_2_2".unindent()
                )
            }

            #[test]
            fn pad_case_index() {
                let item_fn = parse_str::<ItemFn>(
                    r#"fn test(first: u32, second: u32, third: u32) { println!("user code") }"#
                ).unwrap();
                let values = (1..=100).map(|i| i.to_string()).collect::<Vec<_>>();
                let info = RsTestInfo {
                    data: RsTestData {
                        items: vec![
                            values_list("first", values.as_ref()).into(),
                            values_list("second", values[..10].as_ref()).into(),
                            values_list("third", values[..2].as_ref()).into(),
                        ]
                    },
                    ..Default::default()
                };

                let tokens = render_matrix_cases(item_fn.clone(), info);

                let tests = TestsGroup::from(tokens).get_test_functions();

                assert_eq!(tests[0].sig.ident.to_string(), "case_001_01_1");
                assert_eq!(tests.last().unwrap().sig.ident.to_string(), "case_100_10_2");
            }
        }

        mod complete_should {
            use super::{*, assert_eq};

            fn rendered_case(fn_name: &str) -> TestsGroup {
                let item_fn = parse_str::<ItemFn>(&format!(r#"
                        fn {}(
                            fix: u32,
                            a: f64, b: f32,
                            x: i32, y: i32) {{}}"#, fn_name)
                ).unwrap();
                let data = RsTestData {
                    items: vec![fixture("fix", vec!["2"]).into(),
                                ident("a").into(), ident("b").into(),
                                vec!["1f64", "2f32"].into_iter().collect::<TestCase>().into(),
                                TestCase {
                                    description: Some(ident("description")),
                                    ..vec!["3f64", "4f32"].into_iter().collect::<TestCase>()
                                }.into(),
                                values_list("x", &["12", "-2"]).into(),
                                values_list("y", &["-3", "42"]).into(),
                    ],
                };

                render_matrix_cases(item_fn.clone(), data.into()).into()
            }

            fn test_case() -> TestsGroup {
                rendered_case("test_function")
            }

            #[test]
            fn use_function_name_as_outer_module() {
                let rendered = rendered_case("should_be_the_outer_module_name");

                assert_eq!(rendered.module.ident, "should_be_the_outer_module_name")
            }

            #[test]
            fn have_one_module_for_each_parametrized_case() {
                let rendered = test_case();

                assert_eq!(vec!["case_1", "case_2_description"],
                           rendered.get_submodules()
                               .iter()
                               .map(|m| m.ident.to_string())
                               .collect::<Vec<_>>());
            }

            #[test]
            fn implement_exactly_8_tests() {
                let rendered = test_case();

                assert_eq!(8, rendered.get_test_functions().len());
            }

            #[test]
            fn implement_exactly_4_tests_in_each_module() {
                let modules = test_case().module.get_submodules();

                assert_eq!(4, modules[0].get_test_functions().len());
                assert_eq!(4, modules[1].get_test_functions().len());
            }

            #[test]
            fn assign_same_case_value_for_each_test() {
                let modules = test_case().module.get_submodules();

                for f in modules[0].get_test_functions() {
                    let assignments = Assignments::collect_assignments(&f);
                    assert_eq!(assignments.0["a"], expr("1f64"));
                    assert_eq!(assignments.0["b"], expr("2f32"));
                }

                for f in modules[1].get_test_functions() {
                    let assignments = Assignments::collect_assignments(&f);
                    assert_eq!(assignments.0["a"], expr("3f64"));
                    assert_eq!(assignments.0["b"], expr("4f32"));
                }
            }

            #[test]
            fn assign_all_case_combination_in_tests() {
                let modules = test_case().module.get_submodules();

                let cases = vec![("12", "-3"), ("12", "42"), ("-2", "-3"), ("-2", "42")];
                for module in modules {
                    for ((x, y), f) in cases.iter().zip(module.get_test_functions().iter()) {
                        let assignments = Assignments::collect_assignments(f);
                        assert_eq!(assignments.0["x"], expr(x));
                        assert_eq!(assignments.0["y"], expr(y));
                    }
                }
            }
        }
    }

    mod generics_clean_up_should {
        use super::{*, assert_eq};

        #[test]
        fn remove_generics_not_in_output() {
            // Should remove all generics parameters that are not present in output
            let item_fn = parse_str::<ItemFn>(
                r#"
                pub fn test<R: AsRef<str>, B, F, H: Iterator<Item=u32>>() -> (H, B, String, &str)
                        where F: ToString,
                        B: Borrow<u32>

                { }
                "#
            ).unwrap();

            let expected = parse_str::<ItemFn>(
                r#"
                pub fn test<B, H: Iterator<Item=u32>>() -> (H, B, String, &str)
                        where B: Borrow<u32>
                { }
                "#
            ).unwrap();

            let cleaned = generics_clean_up(&item_fn.sig.generics, item_fn.sig.inputs.iter(), &item_fn.sig.output);

            assert_eq!(expected.sig.generics, cleaned);
        }

        #[test]
        fn not_remove_generics_used_in_arguments() {
            // Should remove all generics parameters that are not present in output
            let item_fn = parse_str::<ItemFn>(
                r#"
                pub fn test<R: AsRef<str>, B, F, H: Iterator<Item=u32>>
                    (h: H, it: impl Iterator<Item=R>, j: &[B])
                    where F: ToString,
                    B: Borrow<u32>
                { }
                "#
            ).unwrap();

            let expected = parse_str::<ItemFn>(
                r#"
                pub fn test<R: AsRef<str>, B, H: Iterator<Item=u32>>
                    (h: H, it: impl Iterator<Item=R>, j: &[B])
                    where
                    B: Borrow<u32>
                { }
                "#
            ).unwrap();

            let cleaned = generics_clean_up(&item_fn.sig.generics, item_fn.sig.inputs.iter(), &item_fn.sig.output);

            dbg!(item_fn.sig.inputs);

            assert_eq!(expected.sig.generics, cleaned);
        }

        mod fixture_should {
            use syn::{ItemImpl, ItemStruct};

            use crate::parse::{Attribute, Attributes};

            use super::{*, assert_eq};

            #[derive(Clone)]
            struct FixtureOutput {
                orig: ItemFn,
                fixture: ItemStruct,
                core_impl: ItemImpl,
            }

            impl Parse for FixtureOutput {
                fn parse(input: ParseStream) -> Result<Self> {
                    Ok(FixtureOutput {
                        fixture: input.parse()?,
                        core_impl: input.parse()?,
                        orig: input.parse()?,
                    })
                }
            }

            fn parse_fixture<S: AsRef<str>>(code: S) -> (ItemFn, FixtureOutput) {
                let item_fn = parse_str::<ItemFn>(
                    code.as_ref()
                ).unwrap();

                let tokens = render_fixture(item_fn.clone(), Default::default());
                (item_fn, parse2(tokens).unwrap())
            }

            fn test_maintains_function_visibility(code: &str) {
                let (item_fn, out) = parse_fixture(code);

                assert_eq!(item_fn.vis, out.fixture.vis);
                assert_eq!(item_fn.vis, out.orig.vis);
            }

            fn select_method<S: AsRef<str>>(impl_code: ItemImpl, name: S) -> Option<syn::ImplItemMethod> {
                impl_code.items.into_iter()
                    .filter_map(|ii| match ii {
                        syn::ImplItem::Method(f) => Some(f),
                        _ => None
                    })
                    .find(|f| f.sig.ident == name.as_ref())
            }

            #[test]
            fn maintains_pub_visibility() {
                test_maintains_function_visibility(
                    r#"pub fn test() { }"#
                );
            }

            #[test]
            fn maintains_no_pub_visibility() {
                test_maintains_function_visibility(
                    r#"fn test() { }"#
                );
            }

            #[test]
            fn implement_a_get_method_with_input_fixture_signature() {
                let (item_fn, out) = parse_fixture(
                    r#"
                    pub fn test<R: AsRef<str>, B>(mut s: String, v: &u32, a: &mut [i32], r: R) -> (u32, B, String, &str)
                            where B: Borrow<u32>
                    { }
                    "#);


                let mut signature = select_method(out.core_impl, "get")
                    .unwrap()
                    .sig;

                signature.ident = item_fn.sig.ident.clone();

                assert_eq!(item_fn.sig, signature);
            }

            #[test]
            fn implement_a_default_method_with_input_cleaned_fixture_signature_and_no_args() {
                let (item_fn, out) = parse_fixture(
                    r#"
                    pub fn test<R: AsRef<str>, B, F, H: Iterator<Item=u32>>(mut s: String, v: &u32, a: &mut [i32], r: R) -> (H, B, String, &str)
                        where F: ToString,
                        B: Borrow<u32>

                    { }
                    "#);

                let default_decl = select_method(out.core_impl, "default")
                    .unwrap()
                    .sig;

                let expected = parse_str::<ItemFn>(
                    r#"
                    pub fn default<B, H: Iterator<Item=u32>>() -> (H, B, String, &str)
                            where B: Borrow<u32>
                    { }
                    "#
                ).unwrap();


                assert_eq!(expected.sig.generics, default_decl.generics);
                assert_eq!(item_fn.sig.output, default_decl.output);
                assert!(default_decl.inputs.is_empty());
            }

            #[test]
            fn use_default_return_type_if_any() {
                let item_fn = parse_str::<ItemFn>(
                    r#"
                    pub fn test<R: AsRef<str>, B, F, H: Iterator<Item=u32>>() -> (H, B)
                            where F: ToString,
                            B: Borrow<u32>
                    { }
                    "#
                ).unwrap();

                let tokens = render_fixture(item_fn.clone(),
                                            FixtureInfo {
                                                attributes: Attributes {
                                                    attributes: vec![
                                                        Attribute::Type(
                                                            parse_str("default").unwrap(),
                                                            parse_str("(impl Iterator<Item=u32>, B)").unwrap(),
                                                        )
                                                    ]
                                                }.into(),
                                                ..Default::default()
                                            });
                let out: FixtureOutput = parse2(tokens).unwrap();

                let expected = parse_str::<syn::ItemFn>(
                    r#"
                    pub fn default<B>() -> (impl Iterator<Item=u32>, B)
                            where B: Borrow<u32>
                    { }
                    "#
                ).unwrap();

                let default_decl = select_method(out.core_impl, "default")
                    .unwrap()
                    .sig;

                assert_eq!(expected.sig, default_decl);
            }

            #[test]
            fn implement_partial_methods() {
                let (item_fn, out) = parse_fixture(
                    r#"
                    pub fn test(mut s: String, v: &u32, a: &mut [i32]) -> usize
                    { }
                    "#);

                let partials = (1..=3).map(|n|
                    select_method(out.core_impl.clone(), format!("partial_{}", n))
                        .unwrap()
                        .sig)
                    .collect::<Vec<_>>();

                // All 3 methods found

                assert!(select_method(out.core_impl, "partial_4").is_none());

                let expected_1 = parse_str::<ItemFn>(
                    r#"
                    pub fn partial_1(mut s: String) -> usize
                    { }
                    "#
                ).unwrap();


                assert_eq!(expected_1.sig, partials[0]);
                for p in partials {
                    assert_eq!(item_fn.sig.output, p.output);
                }
            }

            #[test]
            fn clean_generics_in_partial_methods() {
                let (_, out) = parse_fixture(
                    r#"
                    pub fn test<S: AsRef<str>, U: AsRef<u32>, F: ToString>(mut s: S, v: U) -> F
                    { }
                    "#);

                let partials = (1..=2).map(|n|
                    select_method(out.core_impl.clone(), format!("partial_{}", n))
                        .unwrap()
                        .sig)
                    .collect::<Vec<_>>();

                let expected = vec![
                    parse_str::<ItemFn>(
                        r#"
                        pub fn partial_1<S: AsRef<str>, F: ToString>(mut s: S) -> F
                        { }
                        "#
                    ).unwrap().sig,
                    parse_str::<ItemFn>(
                        r#"
                        pub fn partial_2<S: AsRef<str>, U: AsRef<u32>, F: ToString>(mut s: S, v: U) -> F
                        { }
                        "#
                    ).unwrap().sig];

                assert_eq!(expected, partials);
            }

            #[test]
            fn use_partial_return_type_if_any() {
                let item_fn = parse_str::<ItemFn>(
                    r#"
                    pub fn test<R: AsRef<str>, B, F, H: Iterator<Item=u32>>(h: H, b: B) -> (H, B)
                            where F: ToString,
                            B: Borrow<u32>
                    { }
                     "#
                ).unwrap();

                let tokens = render_fixture(item_fn.clone(),
                                            FixtureInfo {
                                                attributes: Attributes {
                                                    attributes: vec![
                                                        Attribute::Type(
                                                            parse_str("partial_1").unwrap(),
                                                            parse_str("(H, impl Iterator<Item=u32>)").unwrap(),
                                                        )
                                                    ]
                                                }.into(),
                                                ..Default::default()
                                            });
                let out: FixtureOutput = parse2(tokens).unwrap();

                let expected = parse_str::<syn::ItemFn>(
                    r#"
                    pub fn partial_1<H: Iterator<Item=u32>>(h: H) -> (H, impl Iterator<Item=u32>)
                    { }
                    "#
                ).unwrap();

                let partial = select_method(out.core_impl, "partial_1")
                    .unwrap();

                assert_eq!(expected.sig, partial.sig);
            }
        }
    }
}
