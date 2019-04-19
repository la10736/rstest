use crate::utils::{TestResults, resources};
use std::path::Path;

fn prj(res: &str) -> crate::prj::Project {
    let path = Path::new("fixture").join(res);
    crate::prj().set_code_file(resources(path))
}

fn run_test(res: &str) -> (std::process::Output, String) {
    let prj = prj(res);
    (prj.run_tests().unwrap(), prj.get_name().to_owned().to_string())
}


#[test]
fn should_use_other_fixtures() {
    let (output, _) = run_test("simple_injection.rs");

    TestResults::new()
        .ok("success")
        .fail("fail")
        .assert(output);
}

#[test]
fn should_create_a_struct_that_generate_the_fixture() {
    let (output, _) = run_test("fixture_struct.rs");

    TestResults::new()
        .ok("resolve_new")
        .ok("resolve_default")
        .assert(output);
}
