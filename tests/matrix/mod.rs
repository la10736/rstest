use std::path::Path;

pub use crate::utils::{*, deindent::Deindent, CountMessageOccurrence};

fn prj(res: &str) -> crate::prj::Project {
    let path = Path::new("matrix").join(res);
    crate::prj().set_code_file(resources(path))
}

fn run_test(res: &str) -> (std::process::Output, String) {
    let prj = prj(res);
    (prj.run_tests().unwrap(), prj.get_name().to_owned().to_string())
}

#[test]
fn should_compile() {
    let output = prj("simple.rs")
        .compile()
        .unwrap();

    assert_eq!(Some(0), output.status.code(), "Compile error due: {}", output.stderr.str())
}

#[test]
fn happy_path() {
    let (output, _) = run_test("simple.rs");

    TestResults::new()
        .ok("strlen_test::case_1__1_1")
        .ok("strlen_test::case_2__2_1")
        .ok("strlen_test::case_3__1_2")
        .ok("strlen_test::case_4__2_2")
        .assert(output);
}
