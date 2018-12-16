
use temp_testdir::TempDir;
use crate::prj::Project;
use crate::utils::testname;

lazy_static! {
    static ref root_dir: TempDir = TempDir::default().permanent();
    static ref root_project: Project = Project::new(root_dir.as_ref()).create();
}

pub fn prj() -> Project {
    let prj_name = testname();

    root_project.workspace_add(&prj_name);

    Project::new(root_project.path())
        .name(prj_name)
        .create()
}
