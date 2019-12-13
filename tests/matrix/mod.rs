use std::path::Path;
use unindent::Unindent;

pub use crate::utils::{*, CountMessageOccurrence};

fn prj(res: &str) -> crate::prj::Project {
    let path = Path::new("matrix").join(res);
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
        .ok("strlen_test::expected_1::input_1")
        .ok("strlen_test::expected_1::input_2")
        .ok("strlen_test::expected_2::input_1")
        .ok("strlen_test::expected_2::input_2")
        .assert(output);
}

#[test]
fn should_apply_partial_fixture() {
    let (output, _) = run_test("partial.rs");

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

mod dump_input_values {
    use super::{run_test, Unindent};
    use crate::utils::*;

    #[test]
    fn if_implement_debug() {
        let (output, _) = run_test("dump_debug.rs");
        let out = output.stdout.str().to_string();

        TestResults::new()
            .fail("should_fail::u_1::s_1::t_1")
            .fail("should_fail::u_1::s_1::t_2")
            .fail("should_fail::u_1::s_2::t_1")
            .fail("should_fail::u_1::s_2::t_2")
            .fail("should_fail::u_2::s_1::t_1")
            .fail("should_fail::u_2::s_1::t_2")
            .fail("should_fail::u_1::s_2::t_1")
            .fail("should_fail::u_2::s_2::t_2")
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
          --> {}/src/lib.rs:19:52
           |
        19 | #[rstest_matrix(condition => [vec![1,2,3].contains(2)] )]
           |                                                    ^
           |                                                    |",
           name).unindent());
    }

    #[test]
    fn if_a_value_contains_empty_list() {
        let (output, name) = execute();

        assert_in!(output.stderr.str(), format!("
        error: Values list should not be empty
          --> {}/src/lib.rs:24:26
           |
        24 | #[rstest_matrix(empty => [])]
           |                          ^^",
           name).unindent());
    }

    #[test]
    fn if_argument_dont_match_function_signature() {
        let (output, name) = execute();

        assert_in!(output.stderr.str(), format!("
        error: Missed argument: 'not_exist_1' should be a test function argument.
          --> {}/src/lib.rs:27:17
           |
        27 | #[rstest_matrix(not_exist_1 => [42],
           |                 ^^^^^^^^^^^",
           name).unindent());

        assert_in!(output.stderr.str(), format!("
        error: Missed argument: 'not_exist_2' should be a test function argument.
          --> {}/src/lib.rs:28:17
           |
        28 |                 not_exist_2 => [42])]
           |                 ^^^^^^^^^^^",
           name).unindent());

    }

    #[test]
    fn if_inject_wrong_fixture() {
        let (output, name) = execute();

        assert_in!(output.stderr.str(), format!("
        error: Missed argument: 'not_a_fixture' should be a test function argument.
          --> {}/src/lib.rs:31:32
           |
        31 | #[rstest_matrix(f => [41, 42], not_a_fixture(24))]
           |                                ^^^^^^^^^^^^^
        ", name).unindent());
    }

    #[test]
    fn if_inject_a_fixture_that_is_already_a_value_list() {
        let (output, name) = execute();

        assert_in!(output.stderr.str(), format!("
        error: Duplicate argument: 'f' is already defined.
          --> {}/src/lib.rs:45:32
           |
        45 | #[rstest_matrix(f => [41, 42], f(42))]
           |                                ^",
           name).unindent());
    }

    #[test]
    fn if_define_value_list_that_is_already_an_injected_fixture() {
        let (output, name) = execute();

        assert_in!(output.stderr.str(), format!("
        error: Duplicate argument: 'f' is already defined.
          --> {}/src/lib.rs:49:24
           |
        49 | #[rstest_matrix(f(42), f => [41, 42])]
           |                        ^",
           name).unindent());
    }

    #[test]
    fn if_inject_a_fixture_more_than_once() {
        let (output, name) = execute();

        assert_in!(output.stderr.str(), format!("
        error: Duplicate argument: 'f' is already defined.
          --> {}/src/lib.rs:53:24
           |
        53 | #[rstest_matrix(f(42), f(42), v => [41, 42])]
           |                        ^",
           name).unindent());
    }
}

#[test]
fn should_reject_no_item_function() {
    let (output, name) = run_test("reject_no_item_function.rs");

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

#[test]
#[cfg_attr(not(deprecate_parametrize_matrix), ignore)]
fn should_deprecate() {
    let output = prj("simple.rs")
        .compile()
        .unwrap();

    assert_in!(output.stderr.str(), "deprecated");
    assert_in!(output.stderr.str(), "rstest_matrix");
    assert_in!(output.stderr.str(), "use #[rstest(...)] instead");
}