#![cfg(test)]

use syn::{
    parse::{Parse, ParseStream, Result},
    parse2, parse_str,
    visit::Visit,
    ItemFn, ItemMod,
};

use crate::test::{assert_eq, fixture, *};

use super::*;

mod arg_2_fixture_should {
    use super::{assert_eq, *};

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

mod single_test_should {
    use crate::test::{assert_eq, *};

    use super::*;

    #[test]
    fn add_return_type_if_any() {
        let input_fn: ItemFn = "fn function(fix: String) -> Result<i32, String> { Ok(42) }".ast();

        let tokens = single(input_fn.clone(), Default::default());

        let result: ItemFn = parse2(tokens).unwrap();

        assert_eq!(result.sig.output, input_fn.sig.output);
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

        let tokens = single(input_fn.clone(), Default::default());

        let result: ItemFn = parse2(tokens).unwrap();
        let first_stmt = result.block.stmts.get(0).unwrap();

        let inner_fn: ItemFn = parse_quote! {
            #first_stmt
        };

        assert_eq!(inner_fn, input_fn);
    }

    #[test]
    fn not_copy_should_panic_attribute() {
        let input_fn: ItemFn = r#"#[should_panic] pub fn test(_s: String){}"#.ast();

        let tokens = single(input_fn.clone(), Default::default());

        let result: ItemFn = parse2(tokens).unwrap();
        let first_stmt = result.block.stmts.get(0).unwrap();

        let inner_fn: ItemFn = parse_quote! {
            #first_stmt
        };

        assert!(!format!("{:?}", inner_fn.attrs).contains("should_panic"));
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

fn is_test_fn(item_fn: &ItemFn) -> bool {
    item_fn
        .attrs
        .iter()
        .filter(|&a| a.path == parse_str::<syn::Path>("test").unwrap())
        .next()
        .is_some()
}

impl TestFunctions {
    fn is_test_fn(item_fn: &ItemFn) -> bool {
        item_fn
            .attrs
            .iter()
            .filter(|&a| a.path == parse_str::<syn::Path>("test").unwrap())
            .next()
            .is_some()
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

trait Named {
    fn name(&self) -> String;
}

impl Named for Ident {
    fn name(&self) -> String {
        self.to_string()
    }
}

impl Named for ItemFn {
    fn name(&self) -> String {
        self.sig.ident.name()
    }
}

impl Named for ItemMod {
    fn name(&self) -> String {
        self.ident.name()
    }
}

trait Names {
    fn names(&self) -> Vec<String>;
}

impl<T: Named> Names for Vec<T> {
    fn names(&self) -> Vec<String> {
        self.iter().map(Named::name).collect()
    }
}

trait ModuleInspector {
    fn get_all_tests(&self) -> Vec<ItemFn>;
    fn get_tests(&self) -> Vec<ItemFn>;
    fn get_modules(&self) -> Vec<ItemMod>;
}

impl ModuleInspector for ItemMod {
    fn get_tests(&self) -> Vec<ItemFn> {
        self.content
            .as_ref()
            .map(|(_, items)| {
                items
                    .iter()
                    .filter_map(|it| match it {
                        syn::Item::Fn(item_fn) if is_test_fn(item_fn) => Some(item_fn.clone()),
                        _ => None,
                    })
                    .collect()
            })
            .unwrap_or_default()
    }

    fn get_all_tests(&self) -> Vec<ItemFn> {
        let mut f = TestFunctions(vec![]);
        f.visit_item_mod(&self);
        f.0
    }

    fn get_modules(&self) -> Vec<ItemMod> {
        self.content
            .as_ref()
            .map(|(_, items)| {
                items
                    .iter()
                    .filter_map(|it| match it {
                        syn::Item::Mod(item_mod) => Some(item_mod.clone()),
                        _ => None,
                    })
                    .collect()
            })
            .unwrap_or_default()
    }
}

impl ModuleInspector for TestsGroup {
    fn get_all_tests(&self) -> Vec<ItemFn> {
        self.module.get_all_tests()
    }

    fn get_tests(&self) -> Vec<ItemFn> {
        self.module.get_tests()
    }

    fn get_modules(&self) -> Vec<ItemMod> {
        self.module.get_modules()
    }
}

#[derive(Default, Debug)]
struct Assignments(HashMap<String, syn::Expr>);

impl<'ast> Visit<'ast> for Assignments {
    //noinspection RsTypeCheck
    fn visit_local(&mut self, assign: &syn::Local) {
        match &assign {
            syn::Local {
                pat: syn::Pat::Ident(pat),
                init: Some((_, expr)),
                ..
            } => {
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

    use super::{assert_eq, *};

    fn into_rstest_data(item_fn: &ItemFn) -> RsTestData {
        RsTestData {
            items: fn_args_idents(item_fn)
                .cloned()
                .map(RsTestItem::CaseArgName)
                .collect(),
        }
    }

    fn one_simple_case() -> (ItemFn, RsTestInfo) {
        let item_fn = r#"fn test(mut fix: String) { println!("user code") }"#.ast();
        let mut info: RsTestInfo = into_rstest_data(&item_fn).into();
        info.push_case(TestCase::from(r#"String::from("3")"#));
        (item_fn, info)
    }

    fn some_simple_cases(cases: i32) -> (ItemFn, RsTestInfo) {
        let item_fn = r#"fn test(mut fix: String) { println!("user code") }"#.ast();
        let mut info: RsTestInfo = into_rstest_data(&item_fn).into();
        info.extend((0..cases).map(|_| TestCase::from(r#"String::from("3")"#)));
        (item_fn, info)
    }

    #[test]
    fn create_a_module_named_as_test_function() {
        let item_fn = "fn should_be_the_module_name(mut fix: String) {}".ast();
        let data = into_rstest_data(&item_fn);
        let tokens = parametrize(item_fn.clone(), data.into());

        let output = TestsGroup::from(tokens);

        assert_eq!(output.module.ident, "should_be_the_module_name");
    }

    #[test]
    fn copy_user_function() {
        let item_fn =
            r#"fn should_be_the_module_name(mut fix: String) { println!("user code") }"#.ast();
        let data = into_rstest_data(&item_fn);
        let tokens = parametrize(item_fn.clone(), data.into());

        let mut output = TestsGroup::from(tokens);

        output.requested_test.attrs = vec![];
        assert_eq!(output.requested_test, item_fn);
    }

    #[test]
    fn should_not_copy_should_panic_attribute() {
        let item_fn =
            r#"#[should_panic] fn with_should_panic(mut fix: String) { println!("user code") }"#
                .ast();
        let data = into_rstest_data(&item_fn);
        let tokens = parametrize(item_fn.clone(), data.into());

        let output = TestsGroup::from(tokens);

        assert!(!format!("{:?}", output.requested_test.attrs).contains("should_panic"));
    }

    #[test]
    fn mark_user_function_as_test() {
        let item_fn =
            r#"fn should_be_the_module_name(mut fix: String) { println!("user code") }"#.ast();
        let data = into_rstest_data(&item_fn);
        let tokens = parametrize(item_fn.clone(), data.into());

        let output = TestsGroup::from(tokens);

        let expected = parse2::<ItemFn>(quote! {
            #[cfg(test)]
            fn some() {}
        })
        .unwrap()
        .attrs;

        assert_eq!(expected, output.requested_test.attrs);
    }

    #[test]
    fn mark_module_as_test() {
        let item_fn =
            r#"fn should_be_the_module_name(mut fix: String) { println!("user code") }"#.ast();
        let data = into_rstest_data(&item_fn);
        let tokens = parametrize(item_fn.clone(), data.into());

        let output = TestsGroup::from(tokens);

        let expected = parse2::<ItemMod>(quote! {
            #[cfg(test)]
            mod some {}
        })
        .unwrap()
        .attrs;

        assert_eq!(expected, output.module.attrs);
    }

    #[test]
    fn add_a_test_case() {
        let (item_fn, info) = one_simple_case();

        let tokens = parametrize(item_fn.clone(), info);

        let tests = TestsGroup::from(tokens).get_all_tests();

        assert_eq!(1, tests.len());
        assert!(&tests[0].sig.ident.to_string().starts_with("case_"))
    }

    #[test]
    fn add_return_type_if_any() {
        let item_fn: ItemFn = "fn function(fix: String) -> Result<i32, String> { Ok(42) }".ast();
        let mut info: RsTestInfo = into_rstest_data(&item_fn).into();
        info.push_case(TestCase::from(r#"String::from("3")"#));

        let tokens = parametrize(item_fn.clone(), info);

        let tests = TestsGroup::from(tokens).get_all_tests();

        assert_eq!(tests[0].sig.output, item_fn.sig.output);
    }

    #[test]
    fn not_copy_user_function() {
        let t_name = "test_name";
        let item_fn: ItemFn = format!(
            "fn {}(fix: String) -> Result<i32, String> {{ Ok(42) }}",
            t_name
        )
        .ast();
        let mut info: RsTestInfo = into_rstest_data(&item_fn).into();
        info.push_case(TestCase::from(r#"String::from("3")"#));

        let tokens = parametrize(item_fn, info);

        let test = &TestsGroup::from(tokens).get_all_tests()[0];
        let inner_functions = extract_inner_functions(&test.block);

        assert_eq!(0, inner_functions.filter(|f| f.sig.ident == t_name).count());
    }

    #[test]
    fn starts_case_number_from_1() {
        let (item_fn, info) = one_simple_case();

        let tokens = parametrize(item_fn.clone(), info);

        let tests = TestsGroup::from(tokens).get_all_tests();

        assert!(
            &tests[0].sig.ident.to_string().starts_with("case_1"),
            "Should starts with case_1 but is {}",
            tests[0].sig.ident.to_string()
        )
    }

    #[test]
    fn add_all_test_cases() {
        let (item_fn, info) = some_simple_cases(5);

        let tokens = parametrize(item_fn.clone(), info);

        let tests = TestsGroup::from(tokens).get_all_tests();

        let valid_names = tests
            .iter()
            .filter(|it| it.sig.ident.to_string().starts_with("case_"));
        assert_eq!(5, valid_names.count())
    }

    #[test]
    fn left_pad_case_number_by_zeros() {
        let (item_fn, info) = some_simple_cases(1000);

        let tokens = parametrize(item_fn.clone(), info);

        let tests = TestsGroup::from(tokens).get_all_tests();

        let first_name = tests[0].sig.ident.to_string();
        let last_name = tests[999].sig.ident.to_string();

        assert!(
            first_name.ends_with("_0001"),
            "Should ends by _0001 but is {}",
            first_name
        );
        assert!(
            last_name.ends_with("_1000"),
            "Should ends by _1000 but is {}",
            last_name
        );

        let valid_names = tests
            .iter()
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

        let tokens = parametrize(item_fn.clone(), info);

        let tests = TestsGroup::from(tokens).get_all_tests();

        assert!(tests[0]
            .sig
            .ident
            .to_string()
            .ends_with(&format!("_{}", description)));
    }
}

mod matrix_cases_should {
    use crate::parse::vlist::ValueList;

    /// Should test matrix tests render without take in account MatrixInfo to RsTestInfo
    /// transformation
    use super::{assert_eq, *};

    fn into_rstest_data(item_fn: &ItemFn) -> RsTestData {
        RsTestData {
            items: fn_args_idents(item_fn)
                .cloned()
                .map(|it| {
                    ValueList {
                        arg: it,
                        values: vec![],
                    }
                    .into()
                })
                .collect(),
        }
    }

    #[test]
    fn create_a_module_named_as_test_function() {
        let item_fn = "fn should_be_the_module_name(mut fix: String) {}".ast();
        let data = into_rstest_data(&item_fn);

        let tokens = matrix(item_fn.clone(), data.into());

        let output = TestsGroup::from(tokens);

        assert_eq!(output.module.ident, "should_be_the_module_name");
    }

    #[test]
    fn copy_user_function() {
        let item_fn =
            r#"fn should_be_the_module_name(mut fix: String) { println!("user code") }"#.ast();
        let data = into_rstest_data(&item_fn);

        let tokens = matrix(item_fn.clone(), data.into());

        let mut output = TestsGroup::from(tokens);

        output.requested_test.attrs = vec![];
        assert_eq!(output.requested_test, item_fn);
    }

    #[test]
    fn not_copy_user_function() {
        let t_name = "test_name";
        let item_fn: ItemFn = format!(
            "fn {}(fix: String) -> Result<i32, String> {{ Ok(42) }}",
            t_name
        )
        .ast();
        let info = RsTestInfo {
            data: RsTestData {
                items: vec![values_list("fix", &["1"]).into()].into(),
            },
            ..Default::default()
        };

        let tokens = matrix(item_fn, info);

        let test = &TestsGroup::from(tokens).get_all_tests()[0];
        let inner_functions = extract_inner_functions(&test.block);

        assert_eq!(0, inner_functions.filter(|f| f.sig.ident == t_name).count());
    }

    #[test]
    fn not_copy_should_panic_attribute() {
        let item_fn =
            r#"#[should_panic] fn with_should_panic(mut fix: String) { println!("user code") }"#
                .ast();
        let info = RsTestInfo {
            data: RsTestData {
                items: vec![values_list("fix", &["1"]).into()].into(),
            },
            ..Default::default()
        };

        let tokens = matrix(item_fn, info);

        let output = TestsGroup::from(tokens);

        assert!(!format!("{:?}", output.requested_test.attrs).contains("should_panic"));
    }

    #[test]
    fn add_return_type_if_any() {
        let item_fn: ItemFn = "fn function(fix: String) -> Result<i32, String> { Ok(42) }".ast();
        let info = RsTestInfo {
            data: RsTestData {
                items: vec![values_list("fix", &["1", "2", "3"]).into()].into(),
            },
            ..Default::default()
        };

        let tokens = matrix(item_fn.clone(), info);

        let tests = TestsGroup::from(tokens).get_tests();

        assert_eq!(tests[0].sig.output, item_fn.sig.output);
        assert_eq!(tests[1].sig.output, item_fn.sig.output);
        assert_eq!(tests[2].sig.output, item_fn.sig.output);
    }

    #[test]
    fn mark_user_function_as_test() {
        let item_fn =
            r#"fn should_be_the_module_name(mut fix: String) { println!("user code") }"#.ast();
        let data = into_rstest_data(&item_fn);

        let tokens = matrix(item_fn.clone(), data.into());

        let output = TestsGroup::from(tokens);

        let expected = parse2::<ItemFn>(quote! {
            #[cfg(test)]
            fn some() {}
        })
        .unwrap()
        .attrs;

        assert_eq!(expected, output.requested_test.attrs);
    }

    #[test]
    fn mark_module_as_test() {
        let item_fn =
            r#"fn should_be_the_module_name(mut fix: String) { println!("user code") }"#.ast();
        let data = into_rstest_data(&item_fn);

        let tokens = matrix(item_fn.clone(), data.into());

        let output = TestsGroup::from(tokens);

        let expected = parse2::<ItemMod>(quote! {
            #[cfg(test)]
            mod some {}
        })
        .unwrap()
        .attrs;

        assert_eq!(expected, output.module.attrs);
    }

    #[test]
    fn with_just_one_arg() {
        let arg_name = "fix";
        let info = RsTestInfo {
            data: RsTestData {
                items: vec![values_list(arg_name, &["1", "2", "3"]).into()].into(),
            },
            ..Default::default()
        };

        let item_fn = format!(r#"fn test({}: u32) {{ println!("user code") }}"#, arg_name).ast();

        let tokens = matrix(item_fn, info);

        let tests = TestsGroup::from(tokens).get_tests();

        assert_eq!(3, tests.len());
        assert!(&tests[0].sig.ident.to_string().starts_with("fix_"))
    }

    mod two_args_should {
        /// Should test matrix tests render without take in account MatrixInfo to RsTestInfo
        /// transformation
        use super::{assert_eq, *};

        fn fixture<'a>() -> (Vec<&'a str>, ItemFn, RsTestInfo) {
            let names = vec!["first", "second"];
            (
                names.clone(),
                format!(
                    r#"fn test({}: u32, {}: u32) {{ println!("user code") }}"#,
                    names[0], names[1]
                )
                .ast(),
                RsTestInfo {
                    data: RsTestData {
                        items: vec![
                            values_list(names[0], &["1", "2", "3"]).into(),
                            values_list(names[1], &["1", "2"]).into(),
                        ],
                    },
                    ..Default::default()
                },
            )
        }

        #[test]
        fn contain_a_module_for_each_first_arg() {
            let (names, item_fn, info) = fixture();

            let tokens = matrix(item_fn, info);

            let modules = TestsGroup::from(tokens).module.get_modules().names();

            let expected = (1..=3)
                .map(|i| format!("{}_{}", names[0], i))
                .collect::<Vec<_>>();

            assert_eq!(expected, modules);
        }

        #[test]
        fn create_all_tests() {
            let (_, item_fn, info) = fixture();

            let tokens = matrix(item_fn, info);

            let tests = TestsGroup::from(tokens).module.get_all_tests().names();

            assert_eq!(6, tests.len());
        }

        #[test]
        fn create_all_modules_with_the_same_functions() {
            let (_, item_fn, info) = fixture();

            let tokens = matrix(item_fn, info);

            let tests = TestsGroup::from(tokens)
                .module
                .get_modules()
                .into_iter()
                .map(|m| m.get_tests().names())
                .collect::<Vec<_>>();

            assert_eq!(tests[0], tests[1]);
            assert_eq!(tests[1], tests[2]);
        }

        #[test]
        fn test_name_should_contain_argument_name() {
            let (names, item_fn, info) = fixture();

            let tokens = matrix(item_fn, info);

            let tests = TestsGroup::from(tokens).module.get_modules()[0]
                .get_tests()
                .names();

            let expected = (1..=2)
                .map(|i| format!("{}_{}", names[1], i))
                .collect::<Vec<_>>();

            assert_eq!(expected, tests);
        }
    }

    #[test]
    fn three_args_should_create_all_function_4_mods_at_the_first_level_and_3_at_the_second() {
        let (first, second, third) = ("first", "second", "third");
        let info = RsTestInfo {
            data: RsTestData {
                items: vec![
                    values_list(first, &["1", "2", "3", "4"]).into(),
                    values_list(second, &["1", "2", "3"]).into(),
                    values_list(third, &["1", "2"]).into(),
                ],
            },
            ..Default::default()
        };
        let item_fn = format!(
            r#"fn test({}: u32, {}: u32, {}: u32) {{ println!("user code") }}"#,
            first, second, third
        )
        .ast();

        let tokens = matrix(item_fn, info);

        let tg = TestsGroup::from(tokens);

        assert_eq!(24, tg.module.get_all_tests().len());
        assert_eq!(4, tg.module.get_modules().len());
        assert_eq!(3, tg.module.get_modules()[0].get_modules().len());
        assert_eq!(3, tg.module.get_modules()[3].get_modules().len());
        assert_eq!(
            2,
            tg.module.get_modules()[0].get_modules()[0]
                .get_tests()
                .len()
        );
        assert_eq!(
            2,
            tg.module.get_modules()[3].get_modules()[1]
                .get_tests()
                .len()
        );
    }

    #[test]
    fn pad_case_index() {
        let item_fn: ItemFn =
            r#"fn test(first: u32, second: u32, third: u32) { println!("user code") }"#.ast();
        let values = (1..=100).map(|i| i.to_string()).collect::<Vec<_>>();
        let info = RsTestInfo {
            data: RsTestData {
                items: vec![
                    values_list("first", values.as_ref()).into(),
                    values_list("second", values[..10].as_ref()).into(),
                    values_list("third", values[..2].as_ref()).into(),
                ],
            },
            ..Default::default()
        };

        let tokens = matrix(item_fn.clone(), info);

        let tg = TestsGroup::from(tokens);

        let mods = tg.get_modules().names();

        assert_eq!(mods[0], "first_001");
        assert_eq!(mods[99], "first_100");

        let mods = tg.get_modules()[0].get_modules().names();

        assert_eq!(mods[0], "second_01");
        assert_eq!(mods[9], "second_10");

        let functions = tg.get_modules()[0].get_modules()[1].get_tests().names();

        assert_eq!(functions[0], "third_1");
        assert_eq!(functions[1], "third_2");
    }
}

mod complete_should {
    use super::{assert_eq, *};

    fn rendered_case(fn_name: &str) -> TestsGroup {
        let item_fn: ItemFn = format!(
            r#"
                        fn {}(
                            fix: u32,
                            a: f64, b: f32,
                            x: i32, y: i32) {{}}"#,
            fn_name
        )
        .ast();
        let data = RsTestData {
            items: vec![
                fixture("fix", vec!["2"]).into(),
                ident("a").into(),
                ident("b").into(),
                vec!["1f64", "2f32"]
                    .into_iter()
                    .collect::<TestCase>()
                    .into(),
                TestCase {
                    description: Some(ident("description")),
                    ..vec!["3f64", "4f32"].into_iter().collect::<TestCase>()
                }
                .into(),
                values_list("x", &["12", "-2"]).into(),
                values_list("y", &["-3", "42"]).into(),
            ],
        };

        matrix(item_fn.clone(), data.into()).into()
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

        assert_eq!(
            vec!["case_1", "case_2_description"],
            rendered
                .get_modules()
                .iter()
                .map(|m| m.ident.to_string())
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn implement_exactly_8_tests() {
        let rendered = test_case();

        assert_eq!(8, rendered.get_all_tests().len());
    }

    #[test]
    fn implement_exactly_4_tests_in_each_module() {
        let modules = test_case().module.get_modules();

        assert_eq!(4, modules[0].get_all_tests().len());
        assert_eq!(4, modules[1].get_all_tests().len());
    }

    #[test]
    fn assign_same_case_value_for_each_test() {
        let modules = test_case().module.get_modules();

        for f in modules[0].get_all_tests() {
            let assignments = Assignments::collect_assignments(&f);
            assert_eq!(assignments.0["a"], expr("1f64"));
            assert_eq!(assignments.0["b"], expr("2f32"));
        }

        for f in modules[1].get_all_tests() {
            let assignments = Assignments::collect_assignments(&f);
            assert_eq!(assignments.0["a"], expr("3f64"));
            assert_eq!(assignments.0["b"], expr("4f32"));
        }
    }

    #[test]
    fn assign_all_case_combination_in_tests() {
        let modules = test_case().module.get_modules();

        let cases = vec![("12", "-3"), ("12", "42"), ("-2", "-3"), ("-2", "42")];
        for module in modules {
            for ((x, y), f) in cases.iter().zip(module.get_all_tests().iter()) {
                let assignments = Assignments::collect_assignments(f);
                assert_eq!(assignments.0["x"], expr(x));
                assert_eq!(assignments.0["y"], expr(y));
            }
        }
    }
}

mod generics_clean_up_should {
    use super::{assert_eq, *};

    #[test]
    fn remove_generics_not_in_output() {
        // Should remove all generics parameters that are not present in output
        let item_fn: ItemFn = r#"
                pub fn test<R: AsRef<str>, B, F, H: Iterator<Item=u32>>() -> (H, B, String, &str)
                        where F: ToString,
                        B: Borrow<u32>

                { }
                "#
        .ast();

        let expected: ItemFn = r#"
                pub fn test<B, H: Iterator<Item=u32>>() -> (H, B, String, &str)
                        where B: Borrow<u32>
                { }
                "#
        .ast();

        let cleaned = generics_clean_up(
            &item_fn.sig.generics,
            item_fn.sig.inputs.iter(),
            &item_fn.sig.output,
        );

        assert_eq!(expected.sig.generics, cleaned);
    }

    #[test]
    fn not_remove_generics_used_in_arguments() {
        // Should remove all generics parameters that are not present in output
        let item_fn: ItemFn = r#"
                pub fn test<R: AsRef<str>, B, F, H: Iterator<Item=u32>>
                    (h: H, it: impl Iterator<Item=R>, j: &[B])
                    where F: ToString,
                    B: Borrow<u32>
                { }
                "#
        .ast();

        let expected: ItemFn = r#"
                pub fn test<R: AsRef<str>, B, H: Iterator<Item=u32>>
                    (h: H, it: impl Iterator<Item=R>, j: &[B])
                    where
                    B: Borrow<u32>
                { }
                "#
        .ast();

        let cleaned = generics_clean_up(
            &item_fn.sig.generics,
            item_fn.sig.inputs.iter(),
            &item_fn.sig.output,
        );

        dbg!(item_fn.sig.inputs);

        assert_eq!(expected.sig.generics, cleaned);
    }
}
