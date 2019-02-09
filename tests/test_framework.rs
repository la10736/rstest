pub mod prj;
pub mod utils;
pub mod root;

use crate::utils::TestResults;
use crate::root::prj;

#[test]
fn one_success() {
    let project = prj();

    project.append_code(
        r#"
        #[test]
        fn success() {
            assert!(true);
        }
        "#
    );

    let output = project.run_tests().unwrap();

    TestResults::new()
        .ok("success")
        .assert(output);
}

#[test]
fn one_fail() {
    let project = prj();

    project.append_code(
        r#"
        #[test]
        fn fail() {
            assert!(false);
        }
        "#
    );

    let output = project.run_tests().unwrap();

    TestResults::new()
        .fail("fail")
        .assert(output);
}
