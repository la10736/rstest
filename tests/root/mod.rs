
use temp_testdir::TempDir;
use crate::prj::Project;
use crate::utils::testname;
use lazy_static::lazy_static;

lazy_static! {
    static ref ROOT_DIR: TempDir = TempDir::default().permanent();
    static ref ROOT_PROJECT: Project = Project::new(ROOT_DIR.as_ref()).create();
}

fn sanitize_project_name<S: AsRef<str>>(s: S) -> String {
    s.as_ref().replace(":", "_")
}

pub fn prj() -> Project {
    let prj_name = sanitize_project_name(testname());

    ROOT_PROJECT.workspace_add(&prj_name);

    Project::new(ROOT_PROJECT.path())
        .name(prj_name)
        .create()
}
