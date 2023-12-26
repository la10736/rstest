use super::*;
use crate::{
    parse::sys::DefaultSysEngine,
    test::{assert_eq, *},
};

mod parse_rstest_data {
    use super::assert_eq;
    use super::*;

    fn parse_rstest_data<S: AsRef<str>>(fixtures: S) -> RsTestData {
        parse_meta(fixtures)
    }

    #[test]
    fn one_arg() {
        let fixtures = parse_rstest_data("my_fixture(42)");

        let expected = RsTestData {
            items: vec![fixture("my_fixture", &["42"]).into()],
        };

        assert_eq!(expected, fixtures);
    }
}

#[test]
fn should_check_all_timeout_to_catch_the_right_errors() {
    let mut item_fn = r#"
            #[timeout(<some>)]
            #[timeout(42)]
            #[timeout]
            #[timeout(Duration::from_millis(20))]
            fn test_fn(#[case] arg: u32) {
            }
        "#
    .ast();

    let mut info = RsTestInfo::default();

    let errors = info
        .extend_with_function_attrs::<DefaultSysEngine>(&mut item_fn)
        .unwrap_err();

    assert_eq!(2, errors.len());
}

#[cfg(feature = "async-timeout")]
#[test]
fn should_parse_async_timeout() {
    let mut item_fn = r#"
            #[timeout(Duration::from_millis(20))]
            async fn test_fn(#[case] arg: u32) {
            }
        "#
    .ast();

    let mut info = RsTestInfo::default();

    info.extend_with_function_attrs::<DefaultSysEngine>(&mut item_fn)
        .unwrap();
}

#[cfg(not(feature = "async-timeout"))]
#[test]
fn should_return_error_for_async_timeout() {
    let mut item_fn = r#"
            #[timeout(Duration::from_millis(20))]
            async fn test_fn(#[case] arg: u32) {
            }
        "#
    .ast();

    let mut info = RsTestInfo::default();

    let errors = info
        .extend_with_function_attrs::<DefaultSysEngine>(&mut item_fn)
        .unwrap_err();

    assert_eq!(1, errors.len());
    assert!(format!("{:?}", errors).contains("async-timeout feature"))
}

fn parse_rstest<S: AsRef<str>>(rstest_data: S) -> RsTestInfo {
    parse_meta(rstest_data)
}

mod no_cases {
    use super::{assert_eq, *};
    use crate::parse::{Attribute, Attributes};

    #[test]
    fn happy_path() {
        let data = parse_rstest(
            r#"my_fixture(42, "other"), other(vec![42])
            :: trace :: no_trace(some)"#,
        );

        let expected = RsTestInfo {
            data: vec![
                fixture("my_fixture", &["42", r#""other""#]).into(),
                fixture("other", &["vec![42]"]).into(),
            ]
            .into(),
            attributes: Attributes {
                attributes: vec![
                    Attribute::attr("trace"),
                    Attribute::tagged("no_trace", vec!["some"]),
                ],
            }
            .into(),
            ..Default::default()
        };

        assert_eq!(expected, data);
    }

    mod fixture_extraction {
        use super::{assert_eq, *};

        #[test]
        fn rename() {
            let data = parse_rstest(
                r#"long_fixture_name(42, "other") as short, simple as s, no_change()"#,
            );

            let expected = RsTestInfo {
                data: vec![
                    fixture("short", &["42", r#""other""#])
                        .with_resolve("long_fixture_name")
                        .into(),
                    fixture("s", &[]).with_resolve("simple").into(),
                    fixture("no_change", &[]).into(),
                ]
                .into(),
                ..Default::default()
            };

            assert_eq!(expected, data);
        }

        #[test]
        fn rename_with_attributes() {
            let mut item_fn = r#"
                    fn test_fn(
                        #[from(long_fixture_name)] 
                        #[with(42, "other")] short: u32, 
                        #[from(simple)]
                        s: &str,
                        no_change: i32) {
                    }
                    "#
            .ast();

            let expected = RsTestInfo {
                data: vec![
                    fixture("short", &["42", r#""other""#])
                        .with_resolve("long_fixture_name")
                        .into(),
                    fixture("s", &[]).with_resolve("simple").into(),
                ]
                .into(),
                ..Default::default()
            };

            let mut data = RsTestInfo::default();

            data.extend_with_function_attrs::<DefaultSysEngine>(&mut item_fn)
                .unwrap();

            assert_eq!(expected, data);
        }

        #[test]
        fn defined_via_with_attributes() {
            let mut item_fn = r#"
                    fn test_fn(#[with(42, "other")] my_fixture: u32, #[with(vec![42])] other: &str) {
                    }
                    "#
            .ast();

            let expected = RsTestInfo {
                data: vec![
                    fixture("my_fixture", &["42", r#""other""#]).into(),
                    fixture("other", &["vec![42]"]).into(),
                ]
                .into(),
                ..Default::default()
            };

            let mut data = RsTestInfo::default();

            data.extend_with_function_attrs::<DefaultSysEngine>(&mut item_fn)
                .unwrap();

            assert_eq!(expected, data);
        }
    }

    #[test]
    fn empty_fixtures() {
        let data = parse_rstest(r#"::trace::no_trace(some)"#);

        let expected = RsTestInfo {
            attributes: Attributes {
                attributes: vec![
                    Attribute::attr("trace"),
                    Attribute::tagged("no_trace", vec!["some"]),
                ],
            }
            .into(),
            ..Default::default()
        };

        assert_eq!(expected, data);
    }

    #[test]
    fn empty_attributes() {
        let data = parse_rstest(r#"my_fixture(42, "other")"#);

        let expected = RsTestInfo {
            data: vec![fixture("my_fixture", &["42", r#""other""#]).into()].into(),
            ..Default::default()
        };

        assert_eq!(expected, data);
    }

    #[test]
    fn extract_notrace_args_atttribute() {
        let mut item_fn = r#"
            fn test_fn(#[notrace] a: u32, #[something_else] b: &str, #[notrace] c: i32) {
            }
            "#
        .ast();

        let mut info = RsTestInfo::default();

        info.extend_with_function_attrs::<DefaultSysEngine>(&mut item_fn)
            .unwrap();
        info.attributes.add_trace(ident("trace"));

        assert!(!info.attributes.trace_me(&ident("a")));
        assert!(info.attributes.trace_me(&ident("b")));
        assert!(!info.attributes.trace_me(&ident("c")));
        let b_args = item_fn
            .sig
            .inputs
            .into_iter()
            .nth(1)
            .and_then(|id| match id {
                syn::FnArg::Typed(arg) => Some(arg.attrs),
                _ => None,
            })
            .unwrap();
        assert_eq!(attrs("#[something_else]"), b_args);
    }

    #[rstest]
    fn extract_future() {
        let mut item_fn = "fn f(#[future] a: u32, b: u32) {}".ast();
        let expected = "fn f(a: u32, b: u32) {}".ast();

        let mut info = RsTestInfo::default();

        info.extend_with_function_attrs::<DefaultSysEngine>(&mut item_fn)
            .unwrap();

        assert_eq!(item_fn, expected);
        assert!(info.arguments.is_future(&ident("a")));
        assert!(!info.arguments.is_future(&ident("b")));
    }
}

mod parametrize_cases {
    use super::{assert_eq, *};
    use std::iter::FromIterator;

    #[test]
    fn one_simple_case_one_arg() {
        let data = parse_rstest(r#"arg, case(42)"#).data;

        let args = data.case_args().collect::<Vec<_>>();
        let cases = data.cases().collect::<Vec<_>>();

        assert_eq!(1, args.len());
        assert_eq!(1, cases.len());
        assert_eq!("arg", &args[0].to_string());
        assert_eq!(to_args!(["42"]), cases[0].args())
    }

    #[test]
    fn happy_path() {
        let info = parse_rstest(
            r#"
                my_fixture(42,"foo"),
                arg1, arg2, arg3,
                case(1,2,3),
                case(11,12,13),
                case(21,22,23)
            "#,
        );

        let data = info.data;
        let fixtures = data.fixtures().cloned().collect::<Vec<_>>();

        assert_eq!(vec![fixture("my_fixture", &["42", r#""foo""#])], fixtures);
        assert_eq!(
            to_strs!(vec!["arg1", "arg2", "arg3"]),
            data.case_args()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
        );

        let cases = data.cases().collect::<Vec<_>>();

        assert_eq!(3, cases.len());
        assert_eq!(to_args!(["1", "2", "3"]), cases[0].args());
        assert_eq!(to_args!(["11", "12", "13"]), cases[1].args());
        assert_eq!(to_args!(["21", "22", "23"]), cases[2].args());
    }

    mod defined_via_with_attributes {
        use super::{assert_eq, *};

        #[test]
        fn one_case() {
            let mut item_fn = r#"
                #[case::first(42, "first")]
                fn test_fn(#[case] arg1: u32, #[case] arg2: &str) {
                }
                "#
            .ast();

            let mut info = RsTestInfo::default();

            info.extend_with_function_attrs::<DefaultSysEngine>(&mut item_fn)
                .unwrap();

            let case_args = info.data.case_args().cloned().collect::<Vec<_>>();
            let cases = info.data.cases().cloned().collect::<Vec<_>>();

            assert_eq!(to_idents!(["arg1", "arg2"]), case_args);
            assert_eq!(
                vec![TestCase::from_iter(["42", r#""first""#].iter()).with_description("first"),],
                cases
            );
        }

        #[test]
        fn parse_tuple_value() {
            let mut item_fn = r#"
                #[case(42, (24, "first"))]
                fn test_fn(#[case] arg1: u32, #[case] tupled: (u32, &str)) {
                }
                "#
            .ast();

            let mut info = RsTestInfo::default();

            info.extend_with_function_attrs::<DefaultSysEngine>(&mut item_fn)
                .unwrap();

            let cases = info.data.cases().cloned().collect::<Vec<_>>();

            assert_eq!(
                vec![TestCase::from_iter(["42", r#"(24, "first")"#].iter()),],
                cases
            );
        }

        #[test]
        fn more_cases() {
            let mut item_fn = r#"
                #[case::first(42, "first")]
                #[case(24, "second")]
                #[case::third(0, "third")]
                fn test_fn(#[case] arg1: u32, #[case] arg2: &str) {
                }
                "#
            .ast();

            let mut info = RsTestInfo::default();

            info.extend_with_function_attrs::<DefaultSysEngine>(&mut item_fn)
                .unwrap();

            let case_args = info.data.case_args().cloned().collect::<Vec<_>>();
            let cases = info.data.cases().cloned().collect::<Vec<_>>();

            assert_eq!(to_idents!(["arg1", "arg2"]), case_args);
            assert_eq!(
                vec![
                    TestCase::from_iter(["42", r#""first""#].iter()).with_description("first"),
                    TestCase::from_iter(["24", r#""second""#].iter()),
                    TestCase::from_iter(["0", r#""third""#].iter()).with_description("third"),
                ],
                cases
            );
        }

        #[test]
        fn should_collect_attributes() {
            let mut item_fn = r#"
                    #[first]
                    #[first2(42)]
                    #[case(42)]
                    #[second]
                    #[case(24)]
                    #[global]
                    fn test_fn(#[case] arg: u32) {
                    }
                "#
            .ast();

            let mut info = RsTestInfo::default();

            info.extend_with_function_attrs::<DefaultSysEngine>(&mut item_fn)
                .unwrap();

            let cases = info.data.cases().cloned().collect::<Vec<_>>();

            assert_eq!(
                vec![
                    TestCase::from_iter(["42"].iter()).with_attrs(attrs(
                        "
                                #[first]
                                #[first2(42)]
                            "
                    )),
                    TestCase::from_iter(["24"].iter()).with_attrs(attrs(
                        "
                            #[second]
                        "
                    )),
                ],
                cases
            );
        }

        #[test]
        fn should_consume_all_used_attributes() {
            let mut item_fn = r#"
                    #[first]
                    #[first2(42)]
                    #[case(42)]
                    #[second]
                    #[case(24)]
                    #[global]
                    fn test_fn(#[case] arg: u32) {
                    }
                "#
            .ast();

            let mut info = RsTestInfo::default();

            info.extend_with_function_attrs::<DefaultSysEngine>(&mut item_fn)
                .unwrap();

            assert_eq!(
                item_fn.attrs,
                attrs(
                    "
                        #[global]
                        "
                )
            );
            assert!(!format!("{:?}", item_fn).contains("case"));
        }

        #[test]
        fn should_report_all_errors() {
            let mut item_fn = r#"
                    #[case(#case_error#)]
                    fn test_fn(#[case] arg: u32, #[with(#fixture_error#)] err_fixture: u32) {
                    }
                "#
            .ast();

            let mut info = RsTestInfo::default();

            let errors = info
                .extend_with_function_attrs::<DefaultSysEngine>(&mut item_fn)
                .unwrap_err();

            assert_eq!(2, errors.len());
        }
    }

    #[test]
    fn should_accept_comma_at_the_end_of_cases() {
        let data = parse_rstest(
            r#"
                arg,
                case(42),
            "#,
        )
        .data;

        let args = data.case_args().collect::<Vec<_>>();
        let cases = data.cases().collect::<Vec<_>>();

        assert_eq!(1, args.len());
        assert_eq!(1, cases.len());
        assert_eq!("arg", &args[0].to_string());
        assert_eq!(to_args!(["42"]), cases[0].args())
    }

    #[test]
    #[should_panic]
    fn should_not_accept_invalid_separator_from_args_and_cases() {
        parse_rstest(
            r#"
                ret
                case::should_success(Ok(())),
                case::should_fail(Err("Return Error"))
            "#,
        );
    }

    #[test]
    fn case_could_be_arg_name() {
        let data = parse_rstest(
            r#"
                case,
                case(42)
            "#,
        )
        .data;

        assert_eq!("case", &data.case_args().next().unwrap().to_string());

        let cases = data.cases().collect::<Vec<_>>();

        assert_eq!(1, cases.len());
        assert_eq!(to_args!(["42"]), cases[0].args());
    }
}

mod matrix_cases {
    use crate::parse::Attribute;

    use super::{assert_eq, *};

    #[test]
    fn happy_path() {
        let info = parse_rstest(
            r#"
                    expected => [12, 34 * 2],
                    input => [format!("aa_{}", 2), "other"],
                "#,
        );

        let value_ranges = info.data.list_values().collect::<Vec<_>>();
        assert_eq!(2, value_ranges.len());
        assert_eq!(to_args!(["12", "34 * 2"]), value_ranges[0].args());
        assert_eq!(
            to_args!([r#"format!("aa_{}", 2)"#, r#""other""#]),
            value_ranges[1].args()
        );
        assert_eq!(info.attributes, Default::default());
    }

    #[test]
    fn should_parse_attributes_too() {
        let info = parse_rstest(
            r#"
                                        a => [12, 24, 42]
                                        ::trace
                                    "#,
        );

        assert_eq!(
            info.attributes,
            Attributes {
                attributes: vec![Attribute::attr("trace")]
            }
            .into()
        );
    }

    #[test]
    fn should_parse_injected_fixtures_too() {
        let info = parse_rstest(
            r#"
                a => [12, 24, 42],
                fixture_1(42, "foo"),
                fixture_2("bar")
                "#,
        );

        let fixtures = info.data.fixtures().cloned().collect::<Vec<_>>();

        assert_eq!(
            vec![
                fixture("fixture_1", &["42", r#""foo""#]),
                fixture("fixture_2", &[r#""bar""#])
            ],
            fixtures
        );
    }

    #[test]
    #[should_panic(expected = "should not be empty")]
    fn should_not_compile_if_empty_expression_slice() {
        parse_rstest(
            r#"
                invalid => []
                "#,
        );
    }

    mod defined_via_with_attributes {
        use super::{assert_eq, *};

        #[test]
        fn one_arg() {
            let mut item_fn = r#"
                fn test_fn(#[values(1, 2, 1+2)] arg1: u32, #[values(format!("a"), "b b".to_owned(), String::new())] arg2: String) {
                }
                "#
            .ast();

            let mut info = RsTestInfo::default();

            info.extend_with_function_attrs::<DefaultSysEngine>(&mut item_fn)
                .unwrap();

            let list_values = info.data.list_values().cloned().collect::<Vec<_>>();

            assert_eq!(2, list_values.len());
            assert_eq!(to_args!(["1", "2", "1+2"]), list_values[0].args());
            assert_eq!(
                to_args!([r#"format!("a")"#, r#""b b".to_owned()"#, "String::new()"]),
                list_values[1].args()
            );
        }
    }
}

mod json {
    use std::{collections::HashSet, path::PathBuf};

    use hierarchy;
    use rstest_test::assert_in;

    use crate::parse::{
        rstest::{
            hierarchy::*,
            json::{JsonBody, StructField},
        },
        sys::{mock::*, MockSysEngine},
    };

    use super::{assert_eq, *};

    #[rstest]
    fn happy_path(#[from(sys_engine_lock)] _lock: SysEngineGuard) {
        let mut item_fn = r#"
            #[json("resources/tests/happy.json")]
            fn base(#[field] age: u16, #[data] user: User, #[field("first_name")] name: String) {
                assert!(age==user.age);
            }
            "#
        .ast();
        let crate_root = PathBuf::from("/fake/root");

        let _ctx = Context::default()
            .expected_crate_root(crate_root.clone())
            .expected_glob(
                "resources/tests/happy.json",
                vec![PathBuf::from("/fake/root/resources/tests/happy.json")],
            )
            .expected_file_context(&[(
                "/fake/root/resources/tests/happy.json",
                r#"[
                    {"age":42,"first_name":"Bob"},
                    {"age":24,"first_name":"Alice"}
                ]"#,
            )]);

        let mut info = RsTestInfo::default();

        info.extend_with_function_attrs::<MockSysEngine>(&mut item_fn)
            .unwrap();

        let files = info.data.files().unwrap();
        let expected_hierarchy = Hierarchy {
            folder: Folder::empty(&crate_root).add_folder(
                Folder::empty("resources").add_folder(
                    Folder::empty("tests").add_file(hierarchy::File {
                        name: std::ffi::OsStr::new("happy.json").to_owned(),
                        content: JsonBody::array()
                            .add(r#"{"age":42,"first_name":"Bob"}"#)
                            .add(r#"{"age":24,"first_name":"Alice"}"#),
                    }),
                ),
            ),
        };

        assert_eq!(&expected_hierarchy, files.hierarchy());
        assert_eq!([ident("user")], files.data());
        assert_eq!(
            HashSet::<&StructField>::from_iter(vec![
                &StructField::new(ident("age"), None),
                &StructField::new(ident("name"), Some("first_name".to_string())),
            ]),
            HashSet::from_iter(files.args())
        );
    }

    #[rstest]
    #[case::field_just_once(
        r#"
            #[json("resources/tests/*.json")]
            fn base(#[field] #[field("first_name")] age: u16) {}"#,
        &["field", "more than once"]
    )]
    #[case::field_without_files(
        r#"
            fn base(#[field] age: u16) {}"#,
        &["field", "files test set"]
    )]
    #[case::field_as_name_value(
        r#"
            #[json("resources/tests/*.json")]
            fn base(#[field = "first_name"] name: String) {}"#,
        &["field", "expected parentheses"]
    )]
    #[case::data_just_once(
        r#"
            #[json("resources/tests/*.json")]
            fn base(#[data] #[data] user: User) {}"#,
        &["data", "more than once"]
    )]
    #[case::data_wrong_syntax(
        r#"
            #[json("resources/tests/*.json")]
            fn base(#[data()] user: User) {}"#,
        &["unexpected token"]
    )]
    #[case::data_wrong_syntax(
        r#"
            #[json("resources/tests/*.json")]
            fn base(#[data = "some"] user: User) {}"#,
        &["unexpected token"]
    )]
    #[case::data_without_files(
        r#"
            fn base(#[data] user: User) {}"#,
        &["data", "files test set"]
    )]
    fn error(#[case] code: &str, #[case] expected: &[&str]) {
        let mut item_fn = code.ast();

        let mut info = RsTestInfo::default();

        let error_code = info
            .extend_with_function_attrs::<DefaultSysEngine>(&mut item_fn)
            .unwrap_err()
            .to_token_stream()
            .display_code();

        for &e in expected {
            assert_in!(error_code, e);
        }
    }
}

mod integrated {
    use super::{assert_eq, *};

    #[test]
    fn should_parse_fixture_cases_and_matrix_in_any_order() {
        let data = parse_rstest(
            r#"
                u,
                m => [1, 2],
                case(42, A{}, D{}),
                a,
                case(43, A{}, D{}),
                the_fixture(42),
                mm => ["f", "oo", "BAR"],
                d
            "#,
        )
        .data;

        let fixtures = data.fixtures().cloned().collect::<Vec<_>>();
        assert_eq!(vec![fixture("the_fixture", &["42"])], fixtures);

        assert_eq!(
            to_strs!(vec!["u", "a", "d"]),
            data.case_args()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
        );

        let cases = data.cases().collect::<Vec<_>>();
        assert_eq!(2, cases.len());
        assert_eq!(to_args!(["42", "A{}", "D{}"]), cases[0].args());
        assert_eq!(to_args!(["43", "A{}", "D{}"]), cases[1].args());

        let value_ranges = data.list_values().collect::<Vec<_>>();
        assert_eq!(2, value_ranges.len());
        assert_eq!(to_args!(["1", "2"]), value_ranges[0].args());
        assert_eq!(
            to_args!([r#""f""#, r#""oo""#, r#""BAR""#]),
            value_ranges[1].args()
        );
    }
}
