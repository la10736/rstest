extern crate temp_testdir;
extern crate toml_edit;

#[macro_use] extern crate lazy_static;

use temp_testdir::TempDir;

pub mod prj;
pub mod utils;

use utils::*;
use prj::Project;

lazy_static! {
    static ref root_dir: TempDir = TempDir::default().permanent();
    static ref root_project: Project = Project::new(root_dir.as_ref()).create();
}


fn run_test(res: &str) -> std::process::Output {
    let prj_name = testname();

    root_project.workspace_add(&prj_name);

    Project::new(root_project.path())
        .name(prj_name)
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
