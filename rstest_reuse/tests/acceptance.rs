use rstest_test::{sanitize_name, testname, Project, TestResults};

use lazy_static::lazy_static;

use std::path::{Path, PathBuf};
use temp_testdir::TempDir;

pub fn resources<O: AsRef<Path>>(name: O) -> PathBuf {
    Path::new("tests").join("resources").join(name)
}

fn prj(res: impl AsRef<Path>) -> Project {
    let prj_name = sanitize_name(testname());

    let prj = ROOT_PROJECT.subproject(&prj_name);
    prj.add_local_dependency("rstest_reuse");
    prj.add_dependency("rstest", r#""*""#);
    prj.set_code_file(resources(res))
}

fn run_test(res: impl AsRef<Path>) -> (std::process::Output, String) {
    let prj = prj(res);
    (
        prj.run_tests().unwrap(),
        prj.get_name().to_owned().to_string(),
    )
}

#[test]
fn simple_example() {
    let (output, _) = run_test("simple_example.rs");

    TestResults::new()
        .ok("it_works::case_1")
        .ok("it_works::case_2")
        .fail("it_fail::case_1")
        .fail("it_fail::case_2")
        .ok("it_fail_but_ok::case_1")
        .ok("it_fail_but_ok::case_2")
        .assert(output);
}

#[test]
fn in_mod() {
    let (output, _) = run_test("in_mod.rs");

    TestResults::new()
        .ok("sub::it_works::case_1")
        .ok("sub::it_works::case_2")
        .fail("sub::it_fail::case_1")
        .fail("sub::it_fail::case_2")
        .assert(output);
}

#[test]
fn import_from_mod() {
    let (output, _) = run_test("import_from_mod.rs");

    TestResults::new()
        .ok("user::it_works::case_1")
        .ok("user::it_works::case_2")
        .assert(output);
}

lazy_static! {
    static ref ROOT_DIR: TempDir = TempDir::new(std::env::temp_dir().join("rstest_reuse"), false);
    static ref ROOT_PROJECT: Project = Project::new(ROOT_DIR.as_ref());
}
