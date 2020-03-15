use std::path::Path;

use unindent::Unindent;

use crate::utils::*;

fn prj(res: impl AsRef<Path>) -> crate::prj::Project {
    let path = Path::new("rstest").join(res.as_ref());
    crate::prj().set_code_file(resources(path))
}

fn run_test(res: impl AsRef<Path>) -> (std::process::Output, String) {
    let prj = prj(res);
    (
        prj.run_tests().unwrap(),
        prj.get_name().to_owned().to_string(),
    )
}

#[test]
fn mutable_input() {
    let (output, _) = run_test("mut.rs");

    TestResults::new()
        .ok("should_success")
        .fail("should_fail")
        .ok("add_test::case_1")
        .ok("add_test::case_2")
        .fail("add_test::case_3")
        .assert(output);
}

#[test]
fn test_with_return_type() {
    let (output, _) = run_test("return_result.rs");

    TestResults::new()
        .ok("should_success")
        .fail("should_fail")
        .ok("return_type::case_1_should_success")
        .fail("return_type::case_2_should_fail")
        .assert(output);
}

#[test]
fn should_panic() {
    let (output, _) = run_test("panic.rs");

    TestResults::new()
        .ok("should_success")
        .fail("should_fail")
        .ok("fail::case_1")
        .ok("fail::case_2")
        .fail("fail::case_3")
        .assert(output);
}

#[test]
fn should_not_show_a_warning_for_should_panic_attribute() {
    let (output, _) = run_test("panic.rs");

    assert!(!output.stderr.str().contains("unused attribute"));
}

#[test]
fn should_map_fixture_by_remove_first_underscore_if_any() {
    let (output, _) = run_test("remove_underscore.rs");

    TestResults::new().ok("ignore_input").assert(output);
}

#[test]
fn generic_input() {
    let (output, _) = run_test("generic.rs");

    TestResults::new()
        .ok("simple")
        .ok("strlen_test::case_1")
        .ok("strlen_test::case_2")
        .assert(output);
}

#[test]
fn impl_input() {
    let (output, _) = run_test("impl_param.rs");

    TestResults::new()
        .ok("simple")
        .ok("strlen_test::case_1")
        .ok("strlen_test::case_2")
        .assert(output);
}

#[test]
fn should_reject_no_item_function() {
    let (output, name) = run_test("reject_no_item_function.rs");

    assert_in!(
        output.stderr.str(),
        format!(
            "
        error: expected `fn`
         --> {}/src/lib.rs:4:1
          |
        4 | struct Foo;
          | ^^^^^^
        ",
            name
        )
        .unindent()
    );

    assert_in!(
        output.stderr.str(),
        format!(
            "
        error: expected `fn`
         --> {}/src/lib.rs:7:1
          |
        7 | impl Foo {{}}
          | ^^^^
        ",
            name
        )
        .unindent()
    );

    assert_in!(
        output.stderr.str(),
        format!(
            "
        error: expected `fn`
          --> {}/src/lib.rs:10:1
           |
        10 | mod mod_baz {{}}
           | ^^^
        ",
            name
        )
        .unindent()
    );
}

mod dump_input_values {
    use super::*;

    #[test]
    fn if_implements_debug() {
        let (output, _) = run_test("dump_debug.rs");
        let out = output.stdout.str().to_string();

        TestResults::new()
            .fail("single_fail")
            .fail("cases_fail::case_1")
            .fail("cases_fail::case_2")
            .fail("matrix_fail::u_1::s_1::t_1")
            .fail("matrix_fail::u_1::s_1::t_2")
            .fail("matrix_fail::u_1::s_2::t_1")
            .fail("matrix_fail::u_1::s_2::t_2")
            .fail("matrix_fail::u_2::s_1::t_1")
            .fail("matrix_fail::u_2::s_1::t_2")
            .fail("matrix_fail::u_1::s_2::t_1")
            .fail("matrix_fail::u_2::s_2::t_2")
            .assert(output);

        assert_in!(out, "fu32 = 42");
        assert_in!(out, r#"fstring = "A String""#);
        assert_in!(out, r#"ftuple = (A, "A String", -12"#);

        assert_in!(out, "u = 42");
        assert_in!(out, r#"s = "str""#);
        assert_in!(out, r#"t = ("ss", -12)"#);

        assert_in!(out, "u = 24");
        assert_in!(out, r#"s = "trs""#);
        assert_in!(out, r#"t = ("tt", -24)"#);

        assert_in!(out, "u = 1");
        assert_in!(out, r#"s = "rst""#);
        assert_in!(out, r#"t = ("SS", -12)"#);

        assert_in!(out, "u = 2");
        assert_in!(out, r#"s = "srt""#);
        assert_in!(out, r#"t = ("TT", -24)"#);
    }

    #[test]
    fn should_not_compile_if_not_implement_debug() {
        let (output, name) = run_test("dump_not_debug.rs");

        assert_in!(
            output.stderr.str(),
            format!(
                "
             --> {}/src/lib.rs:9:11
              |
            9 | fn single(fixture: S) {{}}
              |           ^^^^^^^ `S` cannot be formatted using `{{:?}}`",
                name
            )
            .unindent()
        );

        assert_in!(
            output.stderr.str(),
            format!(
                "
              --> {}/src/lib.rs:15:10
               |
            15 | fn cases(s: S) {{}}
               |          ^ `S` cannot be formatted using `{{:?}}`",
                name
            )
            .unindent()
        );

        assert_in!(
            output.stderr.str().to_string(),
            format!(
                "
              --> {}/src/lib.rs:21:11
               |
            21 | fn matrix(s: S) {{}}
               |           ^ `S` cannot be formatted using `{{:?}}`",
                name
            )
            .unindent()
        );
    }

    #[test]
    fn can_exclude_some_inputs() {
        let (output, _) = run_test("dump_exclude_some_inputs.rs");
        let out = output.stdout.str().to_string();

        TestResults::new()
            .fail("simple")
            .fail("cases::case_1")
            .fail("matrix::a_1::b_1::dd_1")
            .assert(output);

        assert_in!(out, "fu32 = 42");
        assert_in!(out, "d = D");
        assert_in!(out, "fd = D");
        assert_in!(out, "dd = D");
    }

    #[test]
    fn should_be_enclosed_in_an_explicit_session() {
        let (output, _) = run_test(Path::new("single").join("dump_debug.rs"));
        let out = output.stdout.str().to_string();

        TestResults::new().fail("should_fail").assert(output);

        let lines = out
            .lines()
            .skip_while(|l| !l.contains("TEST ARGUMENTS"))
            .take_while(|l| !l.contains("TEST START"))
            .collect::<Vec<_>>();

        assert_eq!(
            4,
            lines.len(),
            "Not contains 3 lines but {}: '{}'",
            lines.len(),
            lines.join("\n")
        );
    }
}

mod single {
    use super::*;

    fn res(name: impl AsRef<Path>) -> impl AsRef<Path> {
        Path::new("single").join(name.as_ref())
    }

    #[test]
    fn one_success_and_one_fail() {
        let (output, _) = run_test(res("simple.rs"));

        TestResults::new()
            .ok("should_success")
            .fail("should_fail")
            .assert(output);
    }

    #[test]
    fn should_resolve_generics_fixture_outputs() {
        let (output, _) = run_test(res("resolve.rs"));

        TestResults::new()
            .ok("generics_u32")
            .ok("generics_i32")
            .assert(output);
    }

    #[test]
    fn should_apply_partial_fixture() {
        let (output, _) = run_test(res("partial.rs"));

        TestResults::new()
            .ok("default")
            .ok("partial_1")
            .ok("partial_2")
            .ok("complete")
            .assert(output);
    }

    #[test]
    fn should_run_async_function() {
        let prj = prj(res("async.rs"));
        prj.add_dependency("async-std", r#"{version="*", features=["attributes"]}"#);

        let output = prj.run_tests().unwrap();

        TestResults::new()
            .ok("should_pass")
            .fail("should_fail")
            .ok("should_panic_pass")
            .fail("should_panic_fail")
            .assert(output);
    }
}

mod cases {
    use super::*;

    fn res(name: impl AsRef<Path>) -> impl AsRef<Path> {
        Path::new("cases").join(name.as_ref())
    }

    #[test]
    fn should_compile() {
        let output = prj(res("happy_path.rs")).compile().unwrap();

        assert_eq!(
            Some(0),
            output.status.code(),
            "Compile error due: {}",
            output.stderr.str()
        )
    }

    #[test]
    fn happy_path() {
        let (output, _) = run_test(res("happy_path.rs"));

        TestResults::new()
            .ok("strlen_test::case_1")
            .ok("strlen_test::case_2")
            .assert(output);
    }

    #[test]
    fn case_description() {
        let (output, _) = run_test(res("description.rs"));

        TestResults::new()
            .ok("description::case_1_user_test_description")
            .ok("description::case_2")
            .fail("description::case_3_user_test_description_fail")
            .assert(output);
    }

    #[test]
    fn should_apply_partial_fixture() {
        let (output, _) = run_test(res("partial.rs"));

        TestResults::new()
            .ok("default::case_1")
            .ok("partial_1::case_1")
            .ok("partial_2::case_1")
            .ok("complete::case_1")
            .fail("default::case_2")
            .fail("partial_1::case_2")
            .fail("partial_2::case_2")
            .fail("complete::case_2")
            .assert(output);
    }

    #[test]
    fn should_use_case_attributes() {
        let (output, _) = run_test(res("case_attributes.rs"));

        TestResults::new()
            .ok("attribute_per_case::case_1_no_panic")
            .ok("attribute_per_case::case_2_panic")
            .ok("attribute_per_case::case_3_panic_with_message")
            .fail("attribute_per_case::case_4_no_panic_but_fail")
            .fail("attribute_per_case::case_5_panic_but_fail")
            .fail("attribute_per_case::case_6_panic_with_wrong_message")
            .assert(output);
    }

    mod not_compile_if_missed_arguments {
        use super::*;

        #[test]
        fn happy_path() {
            let (output, _) = run_test(res("missed_argument.rs"));
            let stderr = output.stderr.str();

            assert_ne!(Some(0), output.status.code());
            assert_in!(stderr, "Missed argument");
            assert_in!(
                stderr,
                "
                  |
                4 | #[rstest(f, case(42), case(24))]
                  |          ^
                "
                .unindent()
            );
        }

        #[test]
        fn should_reports_all() {
            let (output, _) = run_test(res("missed_some_arguments.rs"));
            let stderr = output.stderr.str();

            assert_in!(
                stderr,
                "
                  |
                4 | #[rstest(a,b,c, case(1,2,3), case(3,2,1))]
                  |          ^
                "
                .unindent()
            );
            assert_in!(
                stderr,
                "
                  |
                4 | #[rstest(a,b,c, case(1,2,3), case(3,2,1))]
                  |              ^
                "
                .unindent()
            );

            assert_eq!(
                2,
                stderr.count("Missed argument"),
                "Should contain message exactly 2 occurrences in error message:\n{}",
                stderr
            )
        }

        #[test]
        fn should_report_just_one_error_message_for_all_test_cases() {
            let (output, _) = run_test(res("missed_argument.rs"));
            let stderr = output.stderr.str();

            assert_eq!(
                1,
                stderr.count("Missed argument"),
                "More than one message occurrence in error message:\n{}",
                stderr
            )
        }

        #[test]
        fn should_not_report_error_in_macro_syntax() {
            let (output, _) = run_test(res("missed_argument.rs"));
            let stderr = output.stderr.str();

            assert!(!stderr.contains("macros that expand to items"));
        }
    }

    mod not_compile_if_a_case_has_a_wrong_signature {
        use std::process::Output;

        use lazy_static::lazy_static;

        use super::*;

        //noinspection RsTypeCheck
        fn execute() -> &'static (Output, String) {
            lazy_static! {
                static ref OUTPUT: (Output, String) = run_test(res("case_with_wrong_args.rs"));
            }
            assert_ne!(Some(0), OUTPUT.0.status.code(), "Should not compile");
            &OUTPUT
        }

        #[test]
        fn with_too_much_arguments() {
            let (output, _) = execute();
            let stderr = output.stderr.str();

            assert_in!(
                stderr,
                "
                  |
                8 | #[rstest(a, case(42, 43), case(12), case(24, 34))]
                  |                  ^^^^^^
                "
                .unindent()
            );

            assert_in!(
                stderr,
                "
                  |
                8 | #[rstest(a, case(42, 43), case(12), case(24, 34))]
                  |                                          ^^^^^^
                "
                .unindent()
            );
        }

        #[test]
        fn with_less_arguments() {
            let (output, _) = execute();
            let stderr = output.stderr.str();

            assert_in!(
                stderr,
                "
                  |
                4 | #[rstest(a, b, case(42), case(1, 2), case(43))]
                  |                     ^^
                "
                .unindent()
            );

            assert_in!(
                stderr,
                "
                  |
                4 | #[rstest(a, b, case(42), case(1, 2), case(43))]
                  |                                           ^^
                "
                .unindent()
            );
        }

        #[test]
        fn and_reports_all_errors() {
            let (output, _) = execute();
            let stderr = output.stderr.str();

            // Exactly 4 cases are wrong
            assert_eq!(
                4,
                stderr.count("Wrong case signature: should match the given parameters list."),
                "Should contain message exactly 4 occurrences in error message:\n{}",
                stderr
            );
        }
    }

    mod not_compile_if_args_but_no_cases {
        use std::process::Output;

        use lazy_static::lazy_static;

        use super::*;

        //noinspection RsTypeCheck
        fn execute() -> &'static (Output, String) {
            lazy_static! {
                static ref OUTPUT: (Output, String) = run_test(res("args_with_no_cases.rs"));
            }
            assert_ne!(Some(0), OUTPUT.0.status.code(), "Should not compile");
            &OUTPUT
        }

        #[test]
        fn report_error() {
            let (output, name) = execute();
            let stderr = output.stderr.str();

            assert_in!(
                stderr,
                format!(
                    "
                error: No cases for this argument.
                 --> {}/src/lib.rs:3:10
                  |
                3 | #[rstest(one, two, three)]
                  |          ^^^
                ",
                    name
                )
                .unindent()
            );
        }

        #[test]
        fn and_reports_all_errors() {
            let (output, _) = execute();
            let stderr = output.stderr.str();

            // Exactly 3 cases are wrong
            assert_eq!(
                3,
                stderr.count("No cases for this argument."),
                "Should contain message exactly 3 occurrences in error message:\n{}",
                stderr
            );
        }
    }
}

mod matrix {
    use super::*;

    fn res(name: impl AsRef<Path>) -> impl AsRef<Path> {
        Path::new("matrix").join(name.as_ref())
    }

    #[test]
    fn should_compile() {
        let output = prj(res("simple.rs")).compile().unwrap();

        assert_eq!(
            Some(0),
            output.status.code(),
            "Compile error due: {}",
            output.stderr.str()
        )
    }

    #[test]
    fn happy_path() {
        let (output, _) = run_test(res("simple.rs"));

        TestResults::new()
            .ok("strlen_test::expected_1::input_1")
            .ok("strlen_test::expected_1::input_2")
            .ok("strlen_test::expected_2::input_1")
            .ok("strlen_test::expected_2::input_2")
            .assert(output);
    }

    #[test]
    fn should_apply_partial_fixture() {
        let (output, _) = run_test(res("partial.rs"));

        TestResults::new()
            .ok("default::a_1::b_1")
            .ok("default::a_1::b_2")
            .ok("default::a_2::b_1")
            .ok("partial_2::a_2::b_2")
            .ok("complete::a_2::b_2")
            .fail("default::a_2::b_2")
            .fail("partial_1::a_1::b_1")
            .fail("partial_1::a_1::b_2")
            .fail("partial_1::a_2::b_1")
            .fail("partial_1::a_2::b_2")
            .fail("partial_2::a_1::b_1")
            .fail("partial_2::a_1::b_2")
            .fail("partial_2::a_2::b_1")
            .fail("complete::a_1::b_1")
            .fail("complete::a_1::b_2")
            .fail("complete::a_2::b_1")
            .assert(output);
    }
}

#[test]
fn happy_path() {
    let (output, _) = run_test("happy_path.rs");

    TestResults::new()
        .ok("happy::case_1::expected_1::input_1")
        .ok("happy::case_1::expected_1::input_2")
        .ok("happy::case_1::expected_2::input_1")
        .ok("happy::case_1::expected_2::input_2")
        .ok("happy::case_2_second::expected_1::input_1")
        .ok("happy::case_2_second::expected_1::input_2")
        .ok("happy::case_2_second::expected_2::input_1")
        .ok("happy::case_2_second::expected_2::input_2")
        .assert(output);
}

mod should_show_correct_errors {
    use std::process::Output;

    use lazy_static::lazy_static;

    use super::*;

    //noinspection RsTypeCheck
    fn execute() -> &'static (Output, String) {
        lazy_static! {
            static ref OUTPUT: (Output, String) = run_test("errors.rs");
        }
        &OUTPUT
    }

    #[test]
    fn if_no_fixture() {
        let (output, name) = execute();

        assert_in!(
            output.stderr.str(),
            format!(
                "
                error[E0433]: failed to resolve: use of undeclared type or module `no_fixture`
                  --> {}/src/lib.rs:11:33
                   |
                11 | fn error_cannot_resolve_fixture(no_fixture: u32, f: u32) {{}}",
                name
            )
            .unindent()
        );
    }

    #[test]
    fn if_inject_wrong_fixture() {
        let (output, name) = execute();

        assert_in!(
            output.stderr.str(),
            format!(
                "
                error: Missed argument: 'not_a_fixture' should be a test function argument.
                  --> {}/src/lib.rs:26:23
                   |
                26 | #[rstest(f, case(42), not_a_fixture(24))]
                   |                       ^^^^^^^^^^^^^
                ",
                name
            )
            .unindent()
        );
    }

    #[test]
    fn if_wrong_type() {
        let (output, name) = execute();

        assert_in!(
            output.stderr.str(),
            format!(
                r#"
                error[E0308]: mismatched types
                 --> {}/src/lib.rs:7:18
                  |
                7 |     let a: u32 = "";
                "#,
                name
            )
            .unindent()
        );
    }

    #[test]
    fn if_wrong_type_fixture() {
        let (output, name) = execute();

        assert_in!(
            output.stderr.str(),
            format!(
                "
                error[E0308]: mismatched types
                  --> {}/src/lib.rs:14:29
                   |
                14 | fn error_fixture_wrong_type(fixture: String, f: u32) {{}}
                   |                             ^^^^^^^
                   |                             |
                ",
                name
            )
            .unindent()
        );
    }

    #[test]
    fn if_wrong_type_case_param() {
        let (output, name) = execute();

        assert_in!(
            output.stderr.str(),
            format!(
                "
                error[E0308]: mismatched types
                  --> {}/src/lib.rs:17:26
                   |
                17 | fn error_case_wrong_type(f: &str) {{}}",
                name
            )
            .unindent()
        );
    }

    #[test]
    fn if_wrong_type_matrix_param() {
        let (output, name) = execute();

        assert_in!(
            output.stderr.str(),
            format!(
                "
                error[E0308]: mismatched types
                  --> {}/src/lib.rs:53:28
                   |
                53 | fn error_matrix_wrong_type(f: &str) {{}}",
                name
            )
            .unindent()
        );
    }

    #[test]
    fn if_arbitrary_rust_code_has_some_errors() {
        let (output, name) = execute();

        assert_in!(
            output.stderr.str(),
            format!(
                "
                error[E0308]: mismatched types
                  --> {}/src/lib.rs:20:31
                   |
                20 |     case(vec![1,2,3].contains(2)))
                   |                               ^
                   |                               |",
                name
            )
            .unindent()
        );

        assert_in!(
            output.stderr.str(),
            format!(
                "
                error[E0308]: mismatched types
                  --> {}/src/lib.rs:55:45
                   |
                55 | #[rstest(condition => [vec![1,2,3].contains(2)] )]
                   |                                             ^
                   |                                             |",
                name
            )
            .unindent()
        );
    }

    #[test]
    fn if_inject_a_fixture_that_is_already_a_case() {
        let (output, name) = execute();

        assert_in!(
            output.stderr.str(),
            format!(
                "
                error: Duplicate argument: 'f' is already defined.
                  --> {}/src/lib.rs:40:13
                   |
                40 | #[rstest(f, f(42), case(12))]
                   |             ^",
                name
            )
            .unindent()
        );
    }

    #[test]
    fn if_define_a_case_arg_that_is_already_an_injected_fixture() {
        let (output, name) = execute();

        assert_in!(
            output.stderr.str(),
            format!(
                "
                error: Duplicate argument: 'f' is already defined.
                  --> {}/src/lib.rs:44:17
                   |
                44 | #[rstest(f(42), f, case(12))]
                   |                 ^",
                name
            )
            .unindent()
        );
    }

    #[test]
    fn if_inject_a_fixture_more_than_once() {
        let (output, name) = execute();

        assert_in!(
            output.stderr.str(),
            format!(
                "
                error: Duplicate argument: 'f' is already defined.
                  --> {}/src/lib.rs:48:20
                   |
                48 | #[rstest(v, f(42), f(42), case(12))]
                   |                    ^",
                name
            )
            .unindent()
        );
    }

    #[test]
    fn if_list_argument_dont_match_function_signature() {
        let (output, name) = execute();

        assert_in!(
            output.stderr.str(),
            format!(
                "
                error: Missed argument: 'not_exist_1' should be a test function argument.
                  --> {}/src/lib.rs:63:10
                   |
                63 | #[rstest(not_exist_1 => [42],
                   |          ^^^^^^^^^^^",
                name
            )
            .unindent()
        );

        assert_in!(
            output.stderr.str(),
            format!(
                "
                error: Missed argument: 'not_exist_2' should be a test function argument.
                  --> {}/src/lib.rs:64:10
                   |
                64 |          not_exist_2 => [42])]
                   |          ^^^^^^^^^^^",
                name
            )
            .unindent()
        );
    }

    #[test]
    fn if_inject_a_fixture_that_is_already_a_value_list() {
        let (output, name) = execute();

        assert_in!(
            output.stderr.str(),
            format!(
                "
                error: Duplicate argument: 'f' is already defined.
                  --> {}/src/lib.rs:67:25
                   |
                67 | #[rstest(f => [41, 42], f(42))]
                   |                         ^",
                name
            )
            .unindent()
        );
    }

    #[test]
    fn if_define_value_list_more_that_once() {
        let (output, name) = execute();

        assert_in!(
            output.stderr.str(),
            format!(
                "
                error: Duplicate argument: 'a' is already defined.
                  --> {}/src/lib.rs:81:25
                   |
                81 | #[rstest(a => [42, 24], a => [24, 42])]
                   |                         ^",
                name
            )
            .unindent()
        );
    }

    #[test]
    fn if_define_value_list_that_is_already_an_injected_fixture() {
        let (output, name) = execute();

        assert_in!(
            output.stderr.str(),
            format!(
                "
                error: Duplicate argument: 'f' is already defined.
                  --> {}/src/lib.rs:71:17
                   |
                71 | #[rstest(f(42), f => [41, 42])]
                   |                 ^",
                name
            )
            .unindent()
        );
    }

    #[test]
    fn if_define_value_list_that_is_already_a_case_arg() {
        let (output, name) = execute();

        assert_in!(
            output.stderr.str(),
            format!(
                "
                error: Duplicate argument: 'a' is already defined.
                  --> {}/src/lib.rs:75:23
                   |
                75 | #[rstest(a, case(42), a => [42])]
                   |                       ^",
                name
            )
            .unindent()
        );
    }

    #[test]
    fn if_define_a_case_arg_that_is_already_a_value_list() {
        let (output, name) = execute();

        assert_in!(
            output.stderr.str(),
            format!(
                "
                error: Duplicate argument: 'a' is already defined.
                  --> {}/src/lib.rs:78:21
                   |
                78 | #[rstest(a => [42], a, case(42))]
                   |                     ^",
                name
            )
            .unindent()
        );
    }

    #[test]
    fn if_define_a_case_arg_more_that_once() {
        let (output, name) = execute();

        assert_in!(
            output.stderr.str(),
            format!(
                "
                error: Duplicate argument: 'a' is already defined.
                  --> {}/src/lib.rs:85:13
                   |
                85 | #[rstest(a, a, case(42))]
                   |             ^",
                name
            )
            .unindent()
        );
    }

    #[test]
    fn if_a_value_contains_empty_list() {
        let (output, name) = execute();

        assert_in!(
            output.stderr.str(),
            format!(
                "
                error: Values list should not be empty
                  --> {}/src/lib.rs:60:19
                   |
                60 | #[rstest(empty => [])]
                   |                   ^^",
                name
            )
            .unindent()
        );
    }
}
