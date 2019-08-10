use std::path::Path;

pub use crate::utils::{*, deindent::Deindent, CountMessageOccurrence};

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
        .ok("strlen_test::case_1__1_1")
        .ok("strlen_test::case_2__2_1")
        .ok("strlen_test::case_3__1_2")
        .ok("strlen_test::case_4__2_2")
        .assert(output);
}

mod dump_input_values {
    use super::run_test;
    use crate::utils::{
        *,
        deindent::Deindent,
    };

    #[test]
    fn if_implement_debug() {
        let (output, _) = run_test("dump_debug.rs");
        let out = output.stdout.str().to_string();

        TestResults::new()
            .fail("should_fail::case_1__1_1_1")
            .fail("should_fail::case_2__2_1_1")
            .fail("should_fail::case_3__1_2_1")
            .fail("should_fail::case_4__2_2_1")
            .fail("should_fail::case_5__1_1_2")
            .fail("should_fail::case_6__2_1_2")
            .fail("should_fail::case_7__1_2_2")
            .fail("should_fail::case_8__2_2_2")
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
         --> {}/src/lib.rs:9:18
          |
        9 | fn test_function(s: S) {{}}
          |                  ^ `S` cannot be formatted using `{{:?}}`"#, name).deindent());
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
        11 | fn error_cannot_resolve_fixture(no_fixture: u32, f: u32) {{}}", name).deindent());
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
        "#, name).deindent());
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
        ", name).deindent());
    }

    #[test]
    fn if_wrong_type_param() {
        let (output, name) = execute();

        assert_in!(output.stderr.str(), format!("
        error[E0308]: mismatched types
          --> {}/src/lib.rs:17:27
           |
        17 | fn error_param_wrong_type(f: &str) {{}}", name).deindent());
    }

    #[test]
    fn if_arbitrary_rust_code_has_some_errors() {
        let (output, name) = execute();

        assert_in!(output.stderr.str(), format!(r##"
        error[E0308]: mismatched types
          --> {}/src/lib.rs:19:52
           |
        19 | #[rstest_matrix(condition => [vec![1,2,3].contains(2)] )]
           |                                                    ^
           |                                                    |"##,
           name).deindent());
    }

    #[test]
    fn if_a_value_contains_empty_list() {
        let (output, name) = execute();

        assert_in!(output.stderr.str(), format!(r##"
        error: Values list should not be empty
          --> {}/src/lib.rs:24:26
           |
        24 | #[rstest_matrix(empty => [])]
           |                          ^^"##,
           name).deindent());
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
