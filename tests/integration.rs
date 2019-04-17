pub mod prj;
#[macro_use]
pub mod utils;

/// Test Framework
mod framework;

/// Single's integration tests
mod single;

/// Parametrize's integration tests
mod parametrize;

/// Fixture's integration tests
mod fixture {
    use super::*;
    use crate::utils::TestResults;
    
    #[test]
    fn should_use_other_fixtures() {
        let project = prj();

        project.append_code(
            r#"use rstest::{rstest, fixture};
            
               #[fixture]
               fn root() -> u32 { 21 }

               #[fixture]
               fn incepted(root: u32) -> u32 { 2 * root }
                
               #[rstest]
               fn success(incepted: u32) {
                   assert_eq!(42, incepted);
               }

               #[rstest]
               fn fail(incepted: u32) {
                   assert_eq!(41, incepted);
               }
               "#
        );

        let output = project.run_tests().unwrap();

        TestResults::new()
            .ok("success")
            .fail("fail")
            .assert(output);
    }
}

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

