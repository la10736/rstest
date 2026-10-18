use rstest_test::{sanitize_name, testname, Project};

/// Rstest integration tests
mod rstest;

/// Fixture's integration tests
mod fixture;

use lazy_static::lazy_static;

use std::path::{Path, PathBuf};
use temp_testdir::TempDir;

lazy_static! {
    static ref ROOT_DIR: TempDir = TempDir::default().permanent();
    static ref ROOT_PROJECT: Project = Project::new(ROOT_DIR.as_ref());
}

pub fn base_prj() -> Project {
    let prj_name = sanitize_name(testname());

    ROOT_PROJECT.subproject(&prj_name)
}

pub fn prj() -> Project {
    let prj_name = sanitize_name(testname());

    let prj = ROOT_PROJECT.subproject(&prj_name);
    prj.add_local_dependency("rstest");
    prj
}

pub fn resources<O: AsRef<Path>>(name: O) -> PathBuf {
    Path::new("tests").join("resources").join(name)
}

fn warning_diagnostics(stderr: &str) -> Vec<&str> {
    // Match human-readable diagnostic headers, not tool notices containing
    // "warning:" in a crate name. Ignore SGR styling only for recognition;
    // retain the original lines and stderr for failure diagnostics.
    let ansi = rstest_test::regex::Regex::new(r"\x1b\[[0-9;:]*m").expect("valid ANSI SGR pattern");
    let header = rstest_test::regex::Regex::new(r"^warning(?:\[[^\]\r\n]+\])?:")
        .expect("valid warning diagnostic pattern");
    stderr
        .lines()
        .filter(|line| header.is_match(ansi.replace_all(line, "").trim_start()))
        .collect()
}

fn assert_no_warning_diagnostics(stderr: &str) {
    assert!(
        warning_diagnostics(stderr).is_empty(),
        "unexpected warning diagnostics:\n{stderr}"
    );
}

mod warning_checks {
    // A root glob would also import the sibling `rstest` test module into
    // generated tests, where it conflicts with the external crate name.
    use super::{assert_no_warning_diagnostics, warning_diagnostics};

    #[mytest::rstest]
    #[case::plain("warning: unused variable", &["warning: unused variable"])]
    #[case::coded("warning[unused_variables]: unused variable", &["warning[unused_variables]: unused variable"])]
    #[case::indented("  warning: unused variable", &["  warning: unused variable"])]
    #[case::tabbed("\twarning[unused_variables]: unused variable", &["\twarning[unused_variables]: unused variable"])]
    #[case::styled("\x1b[1m\x1b[33mwarning\x1b[0m\x1b[1m:\x1b[0m text", &["\x1b[1m\x1b[33mwarning\x1b[0m\x1b[1m:\x1b[0m text"])]
    #[case::styled_coded("\x1b[33mwarning[unused_braces]\x1b[0m: text", &["\x1b[33mwarning[unused_braces]\x1b[0m: text"])]
    #[case::summary("warning: crate generated 1 warning", &["warning: crate generated 1 warning"])]
    #[case::empty("", &[])]
    #[case::adapter("[build-limits] fixture_should_not_show_any_warning: the managed sccache server is unreachable; compiling without the cache", &[])]
    #[case::path("/tmp/fixture_should_not_show_any_warning: compiling", &[])]
    #[case::source("  let message = \"warning: quoted text\";", &[])]
    #[case::mixed(concat!("[build-limits] fixture_should_not_show_any_warning: compiling without the cache\n", "warning[unused_braces]: real diagnostic"), &["warning[unused_braces]: real diagnostic"])]
    fn recognizes_warning_diagnostic_headers(#[case] stderr: &str, #[case] expected: &[&str]) {
        assert_eq!(warning_diagnostics(stderr).as_slice(), expected);
    }

    #[mytest::rstest]
    #[case::plain("warning: unused variable")]
    #[case::coded("warning[unused_braces]: unused braces")]
    #[case::styled("\x1b[33mwarning\x1b[0m: unused variable")]
    #[should_panic(expected = "unexpected warning diagnostics")]
    fn rejects_genuine_warning_diagnostics(#[case] stderr: &str) {
        assert_no_warning_diagnostics(stderr);
    }

    #[test]
    fn accepts_adapter_notice_with_warning_in_crate_name() {
        assert_no_warning_diagnostics(
        "[build-limits] fixture_should_not_show_any_warning: the managed sccache server is unreachable; compiling without the cache",
    );
    }
}
