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
    let output = Project::new(&root)
        .create()
        .set_code_file(resources("fixture_simple.rs"))
        .run_tests()
        .unwrap();

    TestResults::new()
        .ok("should_success")
        .fail("should_fail")
        .assert(output);
}
