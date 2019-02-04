
use temp_testdir::TempDir;
use crate::prj::Project;
use crate::utils::testname;

lazy_static! {
    static ref ROOT_DIR: TempDir = TempDir::default().permanent();
    static ref ROOT_PROJECT: Project = Project::new(ROOT_DIR.as_ref()).create();
}

pub fn prj() -> Project {
    let prj_name = testname();

    ROOT_PROJECT.workspace_add(&prj_name);

    Project::new(ROOT_PROJECT.path())
        .name(prj_name)
        .create()
}
