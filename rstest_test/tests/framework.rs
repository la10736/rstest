use lazy_static::lazy_static;
use temp_testdir::TempDir;

use rstest_test::*;

lazy_static! {
    static ref ROOT_DIR: TempDir = TempDir::default().permanent();
    static ref ROOT_PROJECT: Project = Project::new(ROOT_DIR.as_ref());
}

fn prj() -> Project {
    let prj_name = sanitize_name(utils::testname());

    ROOT_PROJECT.subproject(&prj_name)
}

#[test]
fn one_success() {
    let project = prj();

    project.append_code(
        r#"
        #[test]
        fn success() {
            assert!(true);
        }
        "#,
    );

    let output = project.run_tests().unwrap();

    TestResults::new().ok("success").assert(output);
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
        "#,
    );

    let output = project.run_tests().unwrap();

    TestResults::new().fail("fail").assert(output);
}

#[test]
fn more_tests() {
    let project = prj();

    project.append_code(
        r#"
        #[test]
        fn success() {
            assert!(true);
        }
        #[test]
        fn fail() {
            assert!(false);
        }
        #[test]
        fn eq() {
            assert_eq!(1, 1);
        }
        #[test]
        fn no_eq() {
            assert_eq!(1, 2);
        }
        "#,
    );

    let output = project.run_tests().unwrap();

    TestResults::new()
        .ok("success")
        .ok("eq")
        .fail("fail")
        .fail("no_eq")
        .assert(output);
}

#[test]
fn tests_with_should_panic() {
    let project = prj();

    project.append_code(
        r#"
        #[test]
        #[should_panic]
        fn success() {
            assert!(false);
        }
        #[test]
        #[should_panic]
        fn fail() {
            assert!(true);
        }
        "#,
    );

    let output = project.run_tests().unwrap();

    TestResults::new().ok("success").fail("fail").assert(output);
}

#[test]
fn nocapture_in_tests() {
    let project = prj().with_nocapture();

    project.append_code(
        r#"
        #[test]
        fn success() {
            println!("Some output");
        }
        "#,
    );

    let output = project.run_tests().unwrap();

    assert_in!(output.stdout.str(), "Some output")
}

#[test]
fn add_local_dependency() {
    let project = prj();
    project.add_local_dependency("rstest_test");

    project.append_code(
        r#"
        use rstest_test::assert_in;
        #[test]
        fn success() {
            assert_in!("foo bar baz", "bar");
        }
        "#,
    );

    let output = project.run_tests().unwrap();

    TestResults::new().ok("success").assert(output);
}
