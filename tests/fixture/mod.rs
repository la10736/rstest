use std::path::Path;
use unindent::Unindent;

use crate::utils::{TestResults, resources, Stringable};

fn prj(res: &str) -> crate::prj::Project {
    let path = Path::new("fixture").join(res);
    crate::prj().set_code_file(resources(path))
}

fn run_test(res: &str) -> (std::process::Output, String) {
    let prj = prj(res);
    (prj.run_tests().unwrap(), prj.get_name().to_owned().to_string())
}

mod should {
    use super::*;

    #[test]
    fn use_input_fixtures() {
        let (output, _) = run_test("simple_injection.rs");

        TestResults::new()
            .ok("success")
            .fail("fail")
            .assert(output);
    }

    #[test]
    fn create_a_struct_that_return_the_fixture() {
        let (output, _) = run_test("fixture_struct.rs");

        TestResults::new()
            .ok("resolve_new")
            .ok("resolve_default")
            .ok("injected_new")
            .ok("injected_default")
            .assert(output);
    }

    #[test]
    fn be_accessible_from_other_module() {
        let (output, _) = run_test("from_other_module.rs");

        TestResults::new()
            .ok("struct_access")
            .assert(output);
    }

    mod accept_and_return {
        use super::*;

        #[test]
        fn impl_traits() {
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
        fn dyn_traits() {
            let (output, _) = run_test("dyn.rs");

            TestResults::new()
                .ok("test_dyn_box")
                .ok("test_dyn_ref")
                .ok("test_dyn_box_resolve")
                .ok("test_dyn_ref_resolve")
                .assert(output);
        }
    }

    #[test]
    fn resolve_fixture_generics_by_fixture_input() {
        let (output, _) = run_test("resolve.rs");

        TestResults::new()
            .ok("test_u32")
            .ok("test_i32")
            .assert(output);

    }

    #[test]
    fn use_default_return_type_if_any() {
        let (output, _) = run_test("default_return_type.rs");

        TestResults::new()
            .ok("resolve")
            .assert(output);
    }

    #[test]
    fn clean_up_default_from_unused_generics() {
        let (output, _) = run_test("clean_up_default_generics.rs");

        TestResults::new()
            .ok("resolve")
            .assert(output);
    }

    #[test]
    fn apply_partial_fixture() {
        let (output, _) = run_test("partial.rs");

        TestResults::new()
            .ok("default")
            .ok("t_partial_1")
            .ok("t_partial_2")
            .ok("t_complete")
            .assert(output);
    }

    #[test]
    fn show_correct_errors() {
        let prj = prj("errors.rs");
        let output = prj.run_tests().unwrap();
        let name = prj.get_name();

        assert_in!(output.stderr.str(), format!("
        error[E0433]: failed to resolve: use of undeclared type or module `no_fixture`
          --> {}/src/lib.rs:12:33
           |
        12 | fn error_cannot_resolve_fixture(no_fixture: u32) {{", name).unindent());

        assert_in!(output.stderr.str(), format!(r#"
        error[E0308]: mismatched types
         --> {}/src/lib.rs:8:18
          |
        8 |     let a: u32 = "";
          |                  ^^ expected u32, found reference
          |
          = note: expected type `u32`
                     found type `&'static str`
        "#, name).unindent());

        assert_in!(output.stderr.str(), format!("
        error[E0308]: mismatched types
          --> {}/src/lib.rs:16:29
           |
        16 | fn error_fixture_wrong_type(fixture: String) {{
           |                             ^^^^^^^
           |                             |
           |                             expected struct `std::string::String`, found u32
           |                             help: try using a conversion method: `fixture.to_string()`
           |
           = note: expected type `std::string::String`
                      found type `u32`
        ", name).unindent());

        assert_in!(output.stderr.str(), format!("
        error: Missed argument: 'not_a_fixture' should be a test function argument.
          --> {}/src/lib.rs:19:11
           |
        19 | #[fixture(not_a_fixture(24))]
           |           ^^^^^^^^^^^^^
        ", name).unindent());
    }
}
