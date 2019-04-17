pub mod prj;
#[macro_use]
pub mod utils;

/// Test Framework
mod framework;

/// Single's integration tests
mod single;

/// Parametrize's integration tests
mod parametrize;

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

