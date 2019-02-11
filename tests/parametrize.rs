use std::path::Path;

use crate::utils::{*, deindent::Deindent};

pub mod prj;
pub mod utils;
pub mod root;

fn prj(res: &str) -> prj::Project {
    let path = Path::new("parametrize").join(res);
    root::prj().set_code_file(resources(path))
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
        .ok("strlen_test_case_0")
        .ok("strlen_test_case_1")
        .assert(output);
}

#[test]
fn mut_input() {
    let (output, _) = run_test("mut.rs");

    TestResults::new()
        .ok("add_test_case_0")
        .ok("add_test_case_1")
        .assert(output);
}


#[test]
fn generic_input() {
    let (output, _) = run_test("generic.rs");

    TestResults::new()
        .ok("strlen_test_case_0")
        .ok("strlen_test_case_1")
        .assert(output);
}

#[test]
fn impl_input() {
    let (output, _) = run_test("impl_param.rs");

    TestResults::new()
        .ok("strlen_test_case_0")
        .ok("strlen_test_case_1")
        .assert(output);
}

#[test]
fn fallback_to_fixture_lookup() {
    let (output, _) = run_test("fallback.rs");

    TestResults::new()
        .ok("sum_case_0")
        .ok("sum_case_1")
        .assert(output);
}

#[test]
fn should_panic() {
    let (output, _) = run_test("panic.rs");

    TestResults::new()
        .ok("fail_case_0")
        .ok("fail_case_1")
        .fail("fail_case_2")
        .assert(output);
}

#[test]
fn bool_input() {
    let (output, _) = run_test("bool.rs");

    println!("*** stderr: {}", output.stderr.str());

    TestResults::new()
        .ok("bool_case_0")
        .fail("bool_case_1")
        .assert(output);
}


mod not_compile_if_missed_arguments {
    use super::*;

    trait CountMessageOccurrence {
        fn count<S: AsRef<str>>(&self, message: S) -> usize;
    }

    impl<ST> CountMessageOccurrence for ST where ST: AsRef<str> {
        fn count<S: AsRef<str>>(&self, message: S) -> usize {
            self.as_ref().lines()
                .filter(|line| line.contains(
                    message.as_ref()))
                .count()
        }
    }

    #[test]
    fn happy_path() {
        let (output, _) = run_test("missed_argument.rs");
        let stderr = output.stderr.str();

        assert_ne!(Some(0), output.status.code());
        assert_in!(stderr, "Missed argument");
        assert_in!(stderr, r#"
      |
    4 | #[rstest_parametrize(f, case(42), case(24))]
      |                      ^
    "#.deindent());
    }

    #[test]
    fn should_reports_all() {
        let (output, _) = run_test("missed_some_arguments.rs");
        let stderr = output.stderr.str();

        assert_in!(stderr, r#"
      |
    4 | #[rstest_parametrize(a,b,c, case(1,2,3), case(3,2,1))]
      |                      ^
    "#.deindent());
        assert_in!(stderr, r#"
      |
    4 | #[rstest_parametrize(a,b,c, case(1,2,3), case(3,2,1))]
      |                          ^
    "#.deindent());

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
}

mod dump_input_values {
    use super::{
        assert_in, run_test, TestResults,
        utils::{
            deindent::Deindent,
            Stringable,
        },
    };

    #[test]
    fn if_implement_debug() {
        let (output, _) = run_test("dump_debug.rs");
        let out = output.stdout.str().to_string();

        TestResults::new()
            .fail("should_fail_case_0")
            .fail("should_fail_case_1")
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

        assert_in!(output.stderr.str().to_string(), format!(r#"
        error[E0277]: `S` doesn't implement `std::fmt::Debug`
         --> {}/src/lib.rs:5:1
          |
        5 | / #[rstest_parametrize(s,
        6 | |     case(Unwrap("S{{}}"))
        7 | |     ::trace
        8 | | )]
          | |__^ `S` cannot be formatted using `{{:?}}`
          |
          = help: the trait `std::fmt::Debug` is not implemented for `S`
          = note: add `#[derive(Debug)]` or manually implement `std::fmt::Debug`
          = note: required by `std::fmt::Debug::fmt`
        "#, name).deindent());
    }

    #[test]
    fn can_exclude_some_inputs() {
        let (output, _) = run_test("dump_exclude_some_fixtures.rs");
        let out = output.stdout.str().to_string();

        TestResults::new()
            .fail("should_fail_case_0")
            .assert(output);

        assert_in!(out, "u = 42");
        assert_in!(out, "d = D");
    }

    #[test]
    fn should_be_enclosed_in_an_explicit_session() {
        let (output, _) = run_test("dump_exclude_some_fixtures.rs");
        let out = output.stdout.str().to_string();

        TestResults::new()
            .fail("should_fail_case_0")
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

#[test]
fn should_show_correct_errors() {
    let (output, name) = run_test("errors.rs");

    assert_in!(output.stderr.str(), format!("
        error[E0425]: cannot find function `no_fixture` in this scope
          --> {}/src/lib.rs:10:1
           |
        10 | #[rstest_parametrize(f, case(42))]", name).deindent());

    assert_in!(output.stderr.str(), format!(r#"
        error[E0308]: mismatched types
         --> {}/src/lib.rs:7:18
          |
        7 |     let a: u32 = "";
          |                  ^^ expected u32, found reference
          |
          = note: expected type `u32`
                     found type `&'static str`
        "#, name).deindent());

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
        ", name).deindent());

    assert_in!(output.stderr.str(), format!("
        error[E0308]: mismatched types
          --> {}/src/lib.rs:17:27
           |
        17 | fn error_param_wrong_type(f: &str) {{}}", name).deindent());
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
        ", name).deindent());

    assert_in!(output.stderr.str(), format!("
        error: expected `fn`
         --> {}/src/lib.rs:7:1
          |
        7 | impl Foo {{}}
          | ^^^^
        ", name).deindent());

    assert_in!(output.stderr.str(), format!("
        error: expected `fn`
          --> {}/src/lib.rs:10:1
           |
        10 | mod mod_baz {{}}
           | ^^^
        ", name).deindent());
}
