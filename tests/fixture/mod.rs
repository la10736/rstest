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
fn should_create_a_struct_that_retur_the_fixture() {
    let (output, _) = run_test("fixture_struct.rs");

    TestResults::new()
        .ok("resolve_new")
        .ok("resolve_default")
        .ok("injected_new")
        .ok("injected_default")
        .assert(output);
}

#[test]
fn should_be_accessible_from_other_module() {
    let (output, _) = run_test("from_other_module.rs");

    TestResults::new()
        .ok("struct_access")
        .assert(output);
}

#[test]
fn should_accept_and_return_impl_traits() {
    let (output, _) = run_test("impl.rs");

    TestResults::new()
        .ok("base_impl_return")
        .ok("nested_impl_return")
        .ok("nested_multiple_impl_return")
        .ok("base_impl_input")
        .ok("nested_impl_input")
        .ok("nested_multiple_impl_input")
        .assert(output);
}

#[test]
fn should_accept_and_return_dyn_traits() {
    let (output, _) = run_test("dyn.rs");

    TestResults::new()
        .ok("test_dyn_box")
        .ok("test_dyn_ref")
        .ok("test_dyn_box_resolve")
        .ok("test_dyn_ref_resolve")
        .assert(output);
}

#[test]
fn should_resolve_fixture_generics_by_fixture_input() {
    let (output, _) = run_test("resolve.rs");

    TestResults::new()
        .ok("test_u32")
        .ok("test_i32")
        .assert(output);

}

#[test]
fn should_use_default_return_type_if_any() {
    let (output, _) = run_test("default_return_type.rs");

    TestResults::new()
        .ok("resolve")
        .assert(output);
}

#[test]
fn should_clean_up_default_from_unused_generics() {
    let (output, _) = run_test("clean_up_default_generics.rs");

    TestResults::new()
        .ok("resolve")
        .assert(output);
}
