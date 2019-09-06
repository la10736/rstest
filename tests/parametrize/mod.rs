use std::path::Path;
use unindent::Unindent;

pub use crate::utils::{*, CountMessageOccurrence};

fn prj(res: &str) -> crate::prj::Project {
    let path = Path::new("parametrize").join(res);
    crate::prj().set_code_file(resources(path))
}

fn run_test(res: &str) -> (std::process::Output, String) {
    let prj = prj(res);
    (prj.run_tests().unwrap(), prj.get_name().to_owned().to_string())
}

#[test]
fn should_compile() {
    let output = prj("simple.rs")
        .compile()
        .unwrap();

    assert_eq!(Some(0), output.status.code(), "Compile error due: {}", output.stderr.str())
}

#[test]
fn happy_path() {
    let (output, _) = run_test("simple.rs");

    TestResults::new()
        .ok("strlen_test::case_1")
        .ok("strlen_test::case_2")
        .assert(output);
}

#[test]
fn mut_input() {
    let (output, _) = run_test("mut.rs");

    TestResults::new()
        .ok("add_test::case_1")
        .ok("add_test::case_2")
        .assert(output);
}


#[test]
fn generic_input() {
    let (output, _) = run_test("generic.rs");

    TestResults::new()
        .ok("strlen_test::case_1")
        .ok("strlen_test::case_2")
        .assert(output);
}

#[test]
fn should_understand_arbitrary_rust_code() {
    let (output, _) = run_test("arbitrary_rust_code.rs");
    let stderr = output.stderr.str().to_string();

    TestResults::new()
        .ok("arbitrary::case_1")
        .fail("arbitrary::case_2")
        .assert(output);

    assert_in!(stderr, "Case argument accepts arbitrary rust code now")
}

#[test]
fn impl_input() {
    let (output, _) = run_test("impl_param.rs");

    TestResults::new()
        .ok("strlen_test::case_1")
        .ok("strlen_test::case_2")
        .assert(output);
}

#[test]
fn fallback_to_fixture_lookup() {
    let (output, _) = run_test("fallback.rs");

    TestResults::new()
        .ok("sum::case_1")
        .ok("sum::case_2")
        .assert(output);
}

#[test]
fn should_panic() {
    let (output, _) = run_test("panic.rs");

    TestResults::new()
        .ok("fail::case_1")
        .ok("fail::case_2")
        .fail("fail::case_3")
        .assert(output);
}

#[test]
fn bool_input() {
    let (output, _) = run_test("bool.rs");

    TestResults::new()
        .ok("bool::case_1")
        .fail("bool::case_2")
        .assert(output);
}

#[test]
fn test_with_return_type() {
    let (output, _) = run_test("return_result.rs");

    TestResults::new()
        .ok("return_type::case_1_should_success")
        .fail("return_type::case_2_should_fail")
        .assert(output);
}

#[test]
fn case_description() {
    let (output, _) = run_test("description.rs");

    TestResults::new()
        .ok("description::case_1_user_test_description")
        .ok("description::case_2")
        .fail("description::case_3_user_test_description_fail")
        .assert(output);
}

#[test]
fn should_apply_partial_fixture() {
    let (output, _) = run_test("partial.rs");

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

mod not_compile_if_missed_arguments {
    use super::*;

    #[test]
    fn happy_path() {
        let (output, _) = run_test("missed_argument.rs");
        let stderr = output.stderr.str();

        assert_ne!(Some(0), output.status.code());
        assert_in!(stderr, "Missed argument");
        assert_in!(stderr, "
      |
    4 | #[rstest_parametrize(f, case(42), case(24))]
      |                      ^
    ".unindent());
    }

    #[test]
    fn should_reports_all() {
        let (output, _) = run_test("missed_some_arguments.rs");
        let stderr = output.stderr.str();

        assert_in!(stderr, "
      |
    4 | #[rstest_parametrize(a,b,c, case(1,2,3), case(3,2,1))]
      |                      ^
    ".unindent());
        assert_in!(stderr, "
      |
    4 | #[rstest_parametrize(a,b,c, case(1,2,3), case(3,2,1))]
      |                          ^
    ".unindent());

        assert_eq!(2, stderr.count("Missed argument"),
                   "Should contain message exactly 2 occurrences in error message:\n{}", stderr)

    }

    #[test]
    fn should_report_just_one_error_message_for_all_test_cases() {
        let (output, _) = run_test("missed_argument.rs");
        let stderr = output.stderr.str();

        assert_eq!(1, stderr.count("Missed argument"),
                   "More than one message occurrence in error message:\n{}", stderr)
    }

    #[test]
    fn should_not_report_error_in_macro_syntax() {
        let (output, _) = run_test("missed_argument.rs");
        let stderr = output.stderr.str();

        assert!(!stderr.contains("macros that expand to items"));
    }
}

mod not_compile_if_a_case_has_a_wrong_signature {
    use super::*;

    use lazy_static::lazy_static;
    use std::process::Output;

    fn execute() -> &'static (Output, String) {
        lazy_static! {
            static ref OUTPUT: (Output, String) =
                run_test("case_with_wrong_args.rs");
        }
        assert_ne!(Some(0), OUTPUT.0.status.code(), "Should not compile");
        &OUTPUT
    }

    #[test]
    fn with_too_much_arguments() {
        let (output, _) = execute();
        let stderr = output.stderr.str();

        assert_in!(stderr, "
          |
        8 | #[rstest_parametrize(a, case(42, 43), case(12), case(24, 34))]
          |                              ^^^^^^
        ".unindent());

        assert_in!(stderr, "
          |
        8 | #[rstest_parametrize(a, case(42, 43), case(12), case(24, 34))]
          |                                                      ^^^^^^
        ".unindent());
    }

    #[test]
    fn with_less_arguments() {
        let (output, _) = execute();
        let stderr = output.stderr.str();

        assert_in!(stderr, "
          |
        4 | #[rstest_parametrize(a, b, case(42), case(1, 2), case(43))]
          |                                 ^^
        ".unindent());

        assert_in!(stderr, "
          |
        4 | #[rstest_parametrize(a, b, case(42), case(1, 2), case(43))]
          |                                                       ^^
        ".unindent());
    }

    #[test]
    fn and_reports_all_errors() {
        let (output, _) = execute();
        let stderr = output.stderr.str();

        // Exactly 4 cases are wrong
        assert_eq!(4, stderr.count("Wrong case signature: should match the given parameters list."),
                   "Should contain message exactly 4 occurrences in error message:\n{}", stderr);
    }
}

mod dump_input_values {
    use super::*;

    #[test]
    fn if_implement_debug() {
        let (output, _) = run_test("dump_debug.rs");
        let out = output.stdout.str().to_string();

        TestResults::new()
            .fail("should_fail::case_1")
            .fail("should_fail::case_2")
            .assert(output);

        assert_in!(out, "u = 42");
        assert_in!(out, r#"s = "str""#);
        assert_in!(out, r#"t = ("ss", -12)"#);

        assert_in!(out, "u = 24");
        assert_in!(out, r#"s = "trs""#);
        assert_in!(out, r#"t = ("tt", -24)"#);
    }

    #[test]
    fn should_not_compile_if_not_implement_debug() {
        let (output, name) = run_test("dump_not_debug.rs");

        assert_in!(output.stderr.str().to_string(), format!("
        error[E0277]: `S` doesn't implement `std::fmt::Debug`
         --> {}/src/lib.rs:9:18
          |
        9 | fn test_function(s: S) {{}}
          |                  ^ `S` cannot be formatted using `{{:?}}`", name).unindent());
    }

    #[test]
    fn can_exclude_some_inputs() {
        let (output, _) = run_test("dump_exclude_some_fixtures.rs");
        let out = output.stdout.str().to_string();

        TestResults::new()
            .fail("should_fail::case_1")
            .assert(output);

        assert_in!(out, "u = 42");
        assert_in!(out, "d = D");
    }

    #[test]
    fn should_be_enclosed_in_an_explicit_session() {
        let (output, _) = run_test("dump_exclude_some_fixtures.rs");
        let out = output.stdout.str().to_string();

        TestResults::new()
            .fail("should_fail::case_1")
            .assert(output);

        let lines = out.lines()
            .skip_while(|l|
                !l.contains("TEST ARGUMENTS"))
            .take_while(|l|
                !l.contains("TEST START"))
            .collect::<Vec<_>>();

        assert_eq!(3, lines.len(),
                   "Not contains 3 lines but {}: '{}'",
                   lines.len(), lines.join("\n"));
    }
}

mod should_show_correct_errors {
    use super::*;
    use lazy_static::lazy_static;
    use std::process::Output;

    fn execute() -> &'static (Output, String) {
        lazy_static! {
            static ref OUTPUT: (Output, String) =
                run_test("errors.rs");
        }
        &OUTPUT
    }

    #[test]
    fn if_no_fixture() {
        let (output, name) = execute();

        assert_in!(output.stderr.str(), format!("
        error[E0433]: failed to resolve: use of undeclared type or module `no_fixture`
          --> {}/src/lib.rs:11:33
           |
        11 | fn error_cannot_resolve_fixture(no_fixture: u32, f: u32) {{}}", name).unindent());
    }

    #[test]
    fn if_wrong_type() {
        let (output, name) = execute();

        assert_in!(output.stderr.str(), format!(r#"
        error[E0308]: mismatched types
         --> {}/src/lib.rs:7:18
          |
        7 |     let a: u32 = "";
          |                  ^^ expected u32, found reference
          |
          = note: expected type `u32`
                     found type `&'static str`
        "#, name).unindent());
    }

    #[test]
    fn if_wrong_type_fixture() {
        let (output, name) = execute();

        assert_in!(output.stderr.str(), format!("
        error[E0308]: mismatched types
          --> {}/src/lib.rs:14:29
           |
        14 | fn error_fixture_wrong_type(fixture: String, f: u32) {{}}
           |                             ^^^^^^^
           |                             |
           |                             expected struct `std::string::String`, found u32
           |                             help: try using a conversion method: `fixture.to_string()`
           |
           = note: expected type `std::string::String`
                      found type `u32`
        ", name).unindent());
    }

    #[test]
    fn if_wrong_type_param() {
        let (output, name) = execute();

        assert_in!(output.stderr.str(), format!("
        error[E0308]: mismatched types
          --> {}/src/lib.rs:17:27
           |
        17 | fn error_param_wrong_type(f: &str) {{}}", name).unindent());
    }

    #[test]
    fn if_arbitrary_rust_code_has_some_errors() {
        let (output, name) = execute();

        assert_in!(output.stderr.str(), format!("
        error[E0308]: mismatched types
          --> {}/src/lib.rs:20:31
           |
        20 |     case(vec![1,2,3].contains(2)))
           |                               ^
           |                               |",
           name).unindent());
    }
}

#[test]
fn should_reject_no_item_function() {
    let prj = prj("reject_no_item_function.rs");
    let (output, name) = (prj.compile().unwrap(), prj.get_name());

    assert_in!(output.stderr.str(), format!("
        error: expected `fn`
         --> {}/src/lib.rs:4:1
          |
        4 | struct Foo;
          | ^^^^^^
        ", name).unindent());

    assert_in!(output.stderr.str(), format!("
        error: expected `fn`
         --> {}/src/lib.rs:7:1
          |
        7 | impl Foo {{}}
          | ^^^^
        ", name).unindent());

    assert_in!(output.stderr.str(), format!("
        error: expected `fn`
          --> {}/src/lib.rs:10:1
           |
        10 | mod mod_baz {{}}
           | ^^^
        ", name).unindent());
}
