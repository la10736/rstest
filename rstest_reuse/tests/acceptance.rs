use rstest_test::{
    assert_in, assert_not_in, sanitize_name, testname, Project, Stringable, TestResults,
};

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
fn not_show_any_warning() {
    let (output, _) = run_test("simple_example.rs");

    assert_not_in!(output.stderr.str(), "warning:");
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

#[test]
fn deny_docs() {
    let (output, _) = run_test("deny_docs.rs");

    TestResults::new()
        .ok("it_works::case_1")
        .ok("it_works::case_2")
        .assert(output);
}

#[test]
fn enable_export_macros() {
    let (output, _) = run_test("export_template.rs");

    TestResults::new()
        .ok("foo::bar::test::case_1")
        .ok("test::case_1")
        .assert(output);
}

#[test]
fn use_same_name_for_more_templates() {
    let (output, _) = run_test("templates_with_same_name.rs");

    TestResults::new()
        .ok("inner1::it_works::case_1")
        .ok("inner1::it_works::case_2")
        .ok("inner2::it_works::case_1")
        .ok("inner2::it_works::case_2")
        .assert(output);
}

#[test]
fn no_local_macro_should_not_compile() {
    let (output, _) = run_test("no_local_macro_should_not_compile.rs");

    assert!(!output.status.success());
}

lazy_static! {
    static ref ROOT_DIR: TempDir = TempDir::new(std::env::temp_dir().join("rstest_reuse"), false);
    static ref ROOT_PROJECT: Project = Project::new(ROOT_DIR.as_ref());
}
