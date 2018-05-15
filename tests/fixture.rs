extern crate temp_testdir;
#[macro_use]
extern crate rstest_util;

use temp_testdir::TempDir;

pub mod prj;
pub mod utils;

use utils::*;
use prj::Project;

fn run_test(res: &str) -> std::process::Output {
    let root = TempDir::default().permanent();
    Project::new(&root)
        .create()
        .set_code_file(resources(res))
        .run_tests()
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
