pub mod prj;
#[macro_use]
pub mod utils;

/// Test Framework
mod framework;

/// Rstest integration tests
mod rstest;

/// Fixture's integration tests
mod fixture;

use lazy_static::lazy_static;
use prj::Project;
use temp_testdir::TempDir;

lazy_static! {
    static ref ROOT_DIR: TempDir = TempDir::default().permanent();
    static ref ROOT_PROJECT: Project = Project::new(ROOT_DIR.as_ref());
}

fn sanitize_project_name<S: AsRef<str>>(s: S) -> String {
    s.as_ref().replace(":", "_").replace("__", "_")
}

pub fn prj() -> Project {
    let prj_name = sanitize_project_name(utils::testname());

    ROOT_PROJECT.subproject(&prj_name)
}
