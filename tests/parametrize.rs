extern crate temp_testdir;
extern crate toml_edit;

#[macro_use] extern crate lazy_static;

pub mod prj;
pub mod utils;
pub mod root;

use self::utils::{*, deindent::Deindent};
use self::root::prj;

#[test]
fn one_success_test() {
    let project = prj();

    project.append_code(
        r#"
        #[test]
        fn success() {
            assert!(true);
        }
        "#
    );

    let output = project.run_tests().unwrap();

    TestResults::new()
        .ok("success")
        .assert(output);
}

#[test]
fn one_fail_test() {
    let project = prj();

    project.append_code(
        r#"
        #[test]
        fn fail() {
            assert!(false);
        }
        "#
    );

    let output = project.run_tests().unwrap();

    TestResults::new()
        .fail("fail")
        .assert(output);
}

#[test]
fn parametrize_simple_should_compile() {
    let output = prj()
        .set_code_file(resources("parametrize_simple.rs"))
        .compile()
        .unwrap();

    assert_eq!(Some(0), output.status.code(), "Compile error due: {}", output.stderr.str())
}

fn run_test(res: &str) -> (std::process::Output, String) {
    let prj = prj().set_code_file(resources(res));

    (prj.run_tests().unwrap(), prj.get_name().to_owned().to_string())
}

#[test]
fn parametrize_simple_happy_path() {
    let (output, _) = run_test("parametrize_simple.rs");

    TestResults::new()
        .ok("strlen_test_case_0")
        .ok("strlen_test_case_1")
        .assert(output);
}

#[test]
fn parametrize_mut() {
    let (output, _) = run_test("parametrize_mut.rs");

    TestResults::new()
        .ok("add_test_case_0")
        .ok("add_test_case_1")
        .assert(output);
}


#[test]
fn parametrize_generic() {
    let (output, _) = run_test("parametrize_generic.rs");

    TestResults::new()
        .ok("strlen_test_case_0")
        .ok("strlen_test_case_1")
        .assert(output);
}

#[test]
fn parametrize_fallback() {
    let (output, _) = run_test("parametrize_fallback.rs");

    TestResults::new()
        .ok("sum_case_0")
        .ok("sum_case_1")
        .assert(output);
}

#[test]
fn parametrize_should_panic() {
    let (output, _) = run_test("parametrize_panic.rs");

    TestResults::new()
        .ok("fail_case_0")
        .ok("fail_case_1")
        .fail("fail_case_2")
        .assert(output);
}

#[test]
fn parametrize_bool() {
    let (output, _) = run_test("parametrize_bool.rs");

    TestResults::new()
        .ok("bool_case_0")
        .fail("bool_case_1")
        .assert(output);
}

#[test]
fn should_show_correct_errors() {
    let (output, name) = run_test("parametrize_errors.rs");

    assert_in!(output.stderr.str(), format!("
        error[E0425]: cannot find function `no_fixture` in this scope
          --> {}/src/lib.rs:12:1
           |
        12 | #[rstest_parametrize(f, case(42))]
           | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ did you mean `fixture`?
        ", name).deindent());

    assert_in!(output.stderr.str(), format!(r#"
        error[E0308]: mismatched types
         --> {}/src/lib.rs:9:18
          |
        9 |     let a: u32 = "";
          |                  ^^ expected u32, found reference
          |
          = note: expected type `u32`
                     found type `&'static str`
        "#, name).deindent());

    assert_in!(output.stderr.str(), format!("
        error[E0308]: mismatched types
          --> {}/src/lib.rs:16:29
           |
        16 | fn error_fixture_wrong_type(fixture: String, f: u32) {{}}
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
          --> {}/src/lib.rs:19:27
           |
        19 | fn error_param_wrong_type(f: &str) {{}}
           |                           ^ expected &str, found integral variable
           |
           = note: expected type `&str`
                      found type `{{integer}}`
        ", name).deindent());
}

#[test]
fn should_reject_no_item_function() {
    let prj = prj().set_code_file(resources("parametrize_reject_no_item_function.rs"));
    let (output, name) = (prj.compile().unwrap(), prj.get_name());

    assert_in!(output.stderr.str(), format!("
        error: expected `fn`
         --> {}/src/lib.rs:6:1
          |
        6 | struct Foo;
          | ^^^^^^
        ", name).deindent());

    assert_in!(output.stderr.str(), format!("
        error: expected `fn`
         --> {}/src/lib.rs:9:1
          |
        9 | impl Foo {{}}
          | ^^^^
        ", name).deindent());

    assert_in!(output.stderr.str(), format!("
        error: expected `fn`
          --> {}/src/lib.rs:12:1
           |
        12 | mod mod_baz {{}}
           | ^^^
        ", name).deindent());
}
