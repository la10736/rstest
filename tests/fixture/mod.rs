
pub use crate::utils::{self, *, deindent::Deindent};
use crate::prj::Project;
use std::path::Path;

fn prj(res: &str) -> Project {
    let path = Path::new("fixture").join(res);
    crate::prj()
        .set_code_file(resources(path))
}

fn run_test(res: &str) -> std::process::Output {
    prj(res).run_tests()
        .unwrap()
}

#[test]
fn one_success_and_one_fail() {
    let output = run_test("simple.rs");

    TestResults::new()
        .ok("should_success")
        .fail("should_fail")
        .assert(output);
}

#[test]
fn mutable_input() {
    let output = run_test("mut.rs");

    TestResults::new()
        .ok("should_success")
        .fail("should_fail")
        .assert(output);
}

#[test]
fn test_with_return_type() {
    let output = run_test("return_result.rs");

    TestResults::new()
        .ok("should_success")
        .fail("should_fail")
        .assert(output);
}

#[test]
fn should_panic() {
    let output = run_test("panic.rs");

    TestResults::new()
        .ok("should_success")
        .fail("should_fail")
        .assert(output);
}

mod dump_input_values {
    use super::{
        prj, run_test, TestResults,
        utils::{
            Stringable, deindent::Deindent
        },
    };

    #[test]
    fn if_implements_debug() {
        let output = run_test("dump_debug.rs");
        let out = output.stdout.str().to_string();

        TestResults::new()
            .fail("should_fail")
            .assert(output);

        assert_in!(out, "fu32 = 42");
        assert_in!(out, r#"fstring = "A String""#);
        assert_in!(out, r#"ftuple = (A, "A String", -12"#);
    }

    #[test]
    fn should_not_compile_if_not_implement_debug() {
        let prj = prj("dump_not_debug.rs");
        let name = prj.get_name();

        let output = prj.run_tests().unwrap();

        assert_in!(output.stderr.str(), format!(r#"
         --> {}/src/lib.rs:7:1
          |
        7 | #[rstest(trace)]
          | ^^^^^^^^^^^^^^^^ `S` cannot be formatted using `{{:?}}`
          |
          = help: the trait `std::fmt::Debug` is not implemented for `S`
          = note: add `#[derive(Debug)]` or manually implement `std::fmt::Debug`
          = note: required by `std::fmt::Debug::fmt`
        "#, name).deindent());
    }

    #[test]
    fn can_exclude_some_inputs() {
        let output = run_test("dump_exclude_some_fixtures.rs");
        let out = output.stdout.str().to_string();

        TestResults::new()
            .fail("should_fail")
            .assert(output);

        assert_in!(out, "fu32 = 42");
        assert_in!(out, "fd = D");
    }

    #[test]
    fn should_be_enclosed_in_an_explicit_session() {
        let output = run_test("dump_exclude_some_fixtures.rs");
        let out = output.stdout.str().to_string();

        TestResults::new()
            .fail("should_fail")
            .assert(output);

        let lines = out.lines()
            .skip_while(|l|
                !l.contains("TEST ARGUMENTS"))
            .take_while(|l|
                !l.contains("TEST START"))
            .collect::<Vec<_>>();

        assert_eq!(3, lines.len(),
                   "Not contains 3 lines but {}: '{}'",
                   lines.len(), lines.join("\n")
        );
    }
}

#[test]
fn should_show_correct_errors() {
    let prj = prj("errors.rs");
    let output = prj.run_tests().unwrap();
    let name = prj.get_name();

    assert_in!(output.stderr.str(), format!("
        error[E0425]: cannot find function `no_fixture` in this scope
          --> {}/src/lib.rs:10:1
           |
        10 | #[rstest]", name).deindent());

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
          --> {}/src/lib.rs:15:29
           |
        15 | fn error_fixture_wrong_type(fixture: String) {{
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
fn should_reject_no_item_function() {
    let prj = prj("reject_no_item_function.rs");
    let output = prj.compile().unwrap();
    let name = prj.get_name();

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
