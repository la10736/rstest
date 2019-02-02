
use temp_testdir::TempDir;
use crate::prj::Project;
use crate::utils::testname;

lazy_static! {
    static ref ROOT_DIR: TempDir = TempDir::default().permanent();
    static ref ROOT_RPOJECT: Project = Project::new(ROOT_DIR.as_ref()).create();
}

pub fn prj() -> Project {
    let prj_name = testname();

    ROOT_RPOJECT.workspace_add(&prj_name);

    Project::new(ROOT_RPOJECT.path())
        .name(prj_name)
        .create()
}
