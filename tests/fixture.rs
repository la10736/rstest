extern crate temp_testdir;
#[macro_use]
extern crate rstest_util;

use temp_testdir::TempDir;

pub mod prj;
pub mod utils;

use utils::*;
use prj::Project;

#[test]
fn happy_path_one_success_and_one_fail() {
    let root = TempDir::default();
    let project = Project::new(&root).create();

    project.set_code_file(resources("fixture_simple.rs"));

    let output = project.compile().unwrap();

    TestResults::new()
        .ok("should_success")
        .ok("should_fail")
        .assert(output);
}
