extern crate temp_testdir;
extern crate toml_edit;
#[macro_use]
extern crate lazy_static;

pub mod prj;
pub mod utils;
pub mod root;

use self::utils::{*, deindent::Deindent};
use crate::prj::Project;

fn prj(res: &str) -> Project {
    root::prj()
        .set_code_file(resources(res))
}

fn run_test(res: &str) -> std::process::Output {
    prj(res).run_tests()
        .unwrap()
}

#[test]
fn happy_path_one_success_and_one_fail() {
    let output = run_test("fixture_simple.rs");

    TestResults::new()
        .ok("should_success")
        .fail("should_fail")
        .assert(output);
}

#[test]
fn mutable_fixture() {
    let output = run_test("fixture_mut.rs");

    TestResults::new()
        .ok("should_success")
        .fail("should_fail")
        .assert(output);
}

#[test]
fn should_panic() {
    let output = run_test("fixture_panic.rs");

    TestResults::new()
        .ok("should_success")
        .fail("should_fail")
        .assert(output);
}

#[test]
fn should_show_correct_errors() {
    let prj = prj("fixture_errors.rs");
    let output = prj.run_tests().unwrap();
    let name = prj.get_name();

    assert_in!(output.stderr.str(), format!("
        error[E0425]: cannot find function `no_fixture` in this scope
          --> {}/src/lib.rs:12:1
           |
        12 | #[rstest]
           | ^^^^^^^^^ did you mean `fixture`?
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
          --> {}/src/lib.rs:17:29
           |
        17 | fn error_fixture_wrong_type(fixture: String) {{
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
    let prj = prj("fixture_reject_no_item_function.rs");
    let output = prj.run_tests().unwrap();
    let name = prj.get_name();

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
