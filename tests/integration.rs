pub mod prj;
#[macro_use]
pub mod utils;

/// Test Framework
mod framework;

/// Single's integration tests
mod single;

/// Single's integration tests
mod rstest {
    use std::path::Path;
    use crate::utils::*;

    fn prj(res: &str) -> crate::prj::Project {
        let path = Path::new("rstest").join(res);
        crate::prj().set_code_file(resources(path))
    }

    fn run_test(res: &str) -> (std::process::Output, String) {
        let prj = prj(res);
        (prj.run_tests().unwrap(), prj.get_name().to_owned().to_string())
    }


    #[test]
    fn happy_path_cases() {
        let (output, _) = run_test("happy_path_cases.rs");

        TestResults::new()
            .ok("strlen_test::case_1")
            .ok("strlen_test::case_2")
            .assert(output);
    }
}

/// Parametrize's integration tests
mod parametrize;

/// Matrix's integration tests
mod matrix;

/// Fixture's integration tests
mod fixture;

use prj::Project;
use temp_testdir::TempDir;
use lazy_static::lazy_static;

lazy_static! {
    static ref ROOT_DIR: TempDir = TempDir::default().permanent();
    static ref ROOT_PROJECT: Project = Project::new(ROOT_DIR.as_ref());
}

fn sanitize_project_name<S: AsRef<str>>(s: S) -> String {
    s.as_ref().replace(":", "_")
}

pub fn prj() -> Project {
    let prj_name = sanitize_project_name(utils::testname());

    ROOT_PROJECT.subproject(&prj_name)
}

