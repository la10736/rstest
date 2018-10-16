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

fn prj(res: &str) -> Project {
    let prj_name = testname();

    root_project.workspace_add(&prj_name);

    let mut prj = Project::new(root_project.path())
        .name(prj_name)
        .create();
    prj.set_code_file(resources(res));
    prj
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
    let output = prj("fixture_errors.rs").run_tests().unwrap();

    eprintln!("{}", output.stderr.str());

    assert_in!(output.stderr.str(), "12 | #[rstest]\n   | ^^^^^^^^^ did you mean `fixture`?\n");
    assert_in!(output.stderr.str(), "9 |     let a: u32 = \"\";\n  |                  ^^ expected u32, found reference");
    assert_in!(output.stderr.str(), "17 | fn error_fixture_wrong_type(fixture: String) {\n   |                             ^^^^^^^\n   |                             |\n   |                             expected struct `std::string::String`, found u32");
}
