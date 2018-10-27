extern crate temp_testdir;
extern crate toml_edit;

#[macro_use] extern crate lazy_static;

pub mod prj;
pub mod utils;
pub mod root;

use utils::*;
use root::prj;

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

fn run_test(res: &str) -> std::process::Output {
    prj()
        .set_code_file(resources(res))
        .run_tests()
        .unwrap()
}

#[test]
fn parametrize_simple_happy_path() {
    let output = run_test("parametrize_simple.rs");

    TestResults::new()
        .ok("strlen_test_case_0")
        .ok("strlen_test_case_1")
        .assert(output);
}

#[test]
fn parametrize_mut() {
    let output = run_test("parametrize_mut.rs");

    TestResults::new()
        .ok("add_test_case_0")
        .ok("add_test_case_1")
        .assert(output);
}


#[test]
fn parametrize_generic() {
    let output = run_test("parametrize_generic.rs");

    TestResults::new()
        .ok("strlen_test_case_0")
        .ok("strlen_test_case_1")
        .assert(output);
}

#[test]
fn parametrize_fallback() {
    let output = run_test("parametrize_fallback.rs");

    TestResults::new()
        .ok("sum_case_0")
        .ok("sum_case_1")
        .assert(output);
}

#[test]
fn parametrize_should_panic() {
    let output = run_test("parametrize_panic.rs");

    TestResults::new()
        .ok("fail_case_0")
        .ok("fail_case_1")
        .fail("fail_case_2")
        .assert(output);
}

#[test]
fn parametrize_bool() {
    let output = run_test("parametrize_bool.rs");

    TestResults::new()
        .ok("bool_case_0")
        .fail("bool_case_1")
        .assert(output);
}
