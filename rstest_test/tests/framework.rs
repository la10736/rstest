use lazy_static::lazy_static;
use temp_testdir::TempDir;

use rstest::rstest;
use rstest_test::*;

lazy_static! {
    static ref ROOT_DIR: TempDir = TempDir::default().permanent();
    static ref ROOT_PROJECT: Project = Project::new(ROOT_DIR.as_ref());
}

fn prj() -> Project {
    let prj_name = sanitize_name(utils::testname());

    ROOT_PROJECT.subproject(&prj_name)
}

#[rstest]
#[case::default_conf(TestResults::new())]
#[case::contains(TestResults::new().with_contains(true))]
fn one_success(#[case] results: TestResults<&str>) {
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

    results.ok("success").assert(output);
}

#[rstest]
#[case::default_conf(TestResults::new())]
#[case::contains(TestResults::new().with_contains(true))]
fn one_fail(#[case] results: TestResults<&str>) {
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

    results.fail("fail").assert(output);
}

#[rstest]
#[case::default_conf(TestResults::new())]
#[case::contains(TestResults::new().with_contains(true))]
fn more_tests(#[case] results: TestResults<&str>) {
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

    results
        .ok("success")
        .ok("eq")
        .fail("fail")
        .fail("no_eq")
        .assert(output);
}

#[rstest]
#[case::default_conf(TestResults::new())]
#[case::contains(TestResults::new().with_contains(true))]
fn tests_with_should_panic(#[case] results: TestResults<&str>) {
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

    results.ok("success").fail("fail").assert(output);
}

#[test]
fn should_check_just_contains_conf() {
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
        fn two_eq() {
            assert_eq!(2, 2);
        }
        #[test]
        fn enq() {
            assert_eq!(1, 2);
        }
        #[test]
        fn two_enq() {
            assert_eq!(1, 3);
        }
        "#,
    );

    let output = project.run_tests().unwrap();
    let results = TestResults::new().with_contains(true);

    results
        .ok("suc")
        .ok_times("eq", 2)
        .fail("fai")
        .fail_times("enq", 2)
        .assert(output);
}

#[test]
fn should_check_just_contains_on_some_test() {
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
        fn two_eq() {
            assert_eq!(2, 2);
        }
        #[test]
        fn enq() {
            assert_eq!(1, 2);
        }
        #[test]
        fn two_enq() {
            assert_eq!(1, 3);
        }
        "#,
    );

    let output = project.run_tests().unwrap();
    let results = TestResults::new();

    results
        .ok("success")
        .ok_with("eq", false, 2)
        .fail("fail")
        .fail("enq")
        .fail("two_enq")
        .assert(output);
}

#[test]
#[should_panic(expected = "but wrong count")]
fn should_detect_wrong_contains() {
    let project = prj();

    project.append_code(
        r#"
        #[test]
        fn case_1() {
            assert!(true);
        }
        #[test]
        fn case_2() {
            assert!(true);
        }
        #[test]
        fn case_3() {
            assert!(true);
        }
        "#,
    );

    let output = project.run_tests().unwrap();
    let results = TestResults::new();

    results
        .ok("case_3")
        .ok_with("case", false, 2)
        .assert(output);
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
