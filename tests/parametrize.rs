extern crate temp_testdir;

use std::process::Command;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::ffi::OsStr;
use std::ffi::OsString;
use std::borrow::Cow;
use std::process::Stdio;
use temp_testdir::TempDir;

fn resources<O: AsRef<Path>>(name: O) -> PathBuf {
    Path::new("resources").join(name)
}

#[derive(Clone)]
pub enum TestResult<S: AsRef<str>> {
    Ok(S),
    Fail(S),
}

impl<S: AsRef<str>> TestResult<S> {
    pub fn is_fail(&self) -> bool {
        use TestResult::*;
        match *self {
            Fail(_) => true,
            _ => false
        }
    }

    pub fn is_ok(&self) -> bool {
        use TestResult::*;
        match *self {
            Ok(_) => true,
            _ => false
        }
    }

    pub fn name(&self) -> String {
        use TestResult::*;
        match self {
            &Ok(ref s) => s.as_ref().to_owned(),
            &Fail(ref s) => s.as_ref().to_owned()
        }
    }

    pub fn msg(&self) -> &'static str {
        use TestResult::*;
        match *self {
            Ok(_) => "ok",
            Fail(_) => "FAILED",
        }
    }
}

macro_rules! assert_in {
    ($text:expr, $message:expr) => ({
        match (&$text, &$message) {
            (text_val, message_val) => {
                if !text_val.contains(message_val) {
                    panic!(r#"assertion failed: `text not contains message`
         text: `{:?}`,
         message: `{:?}`"#, text_val, message_val)
                }
            }
        }
        });
    ($message:expr, $expected:expr, ) => (
        assert_in!($message, $expected)
    );
    ($text:expr, $message:expr, $($arg:tt)+) => ({
        match (&$text, &$message) {
            (text_val, message_val) => {
                if !text_val.contains(message_val) {
                    panic!(r#"assertion failed: `text not contains message`
         text: `{:?}`,
         message: `{:?}`: {}"#, text_val, message_val, format_args!($($arg)+))
                }
            }
        }
        });
}


#[derive(Default, Clone)]
struct TestResults<S>(Vec<TestResult<S>>) where S: AsRef<str> + Clone;

impl<S> TestResults<S>
    where S: AsRef<str> + Clone
{
    fn new() -> Self {
        TestResults(vec![])
    }

    fn ok(self, name: S) -> Self {
        self.append(TestResult::Ok(name))
    }

    fn fail(self, name: S) -> Self {
        self.append(TestResult::Fail(name))
    }

    fn append(mut self, test: TestResult<S>) -> Self {
        self.0.push(test);
        self
    }

    fn assert(&self, output: std::process::Output) {
        let tests = &self.0;

        let expected_code = if !self.should_fail() { 0 } else { 101 };
        assert_eq!(Some(expected_code), output.status.code(),
                   "Console: {}", output.stderr.str());

        let output = output.stdout.str();
        assert_in!(output, format!("running {} test", tests.len()));

        self.for_each(
            |t| assert!(output.contains(&format!("test {} ... {}", t.name(), t.msg())))
        );

        if self.should_fail() {
            assert_in!(output, format!("failures:"));
        }

        self.for_each_failed(
            |t| assert_in!(output, format!("    {}", t.name()))
        );
    }

    fn should_fail(&self) -> bool {
        self.0.iter().filter(|r| r.is_fail()).next().is_some()
    }

    fn for_each<F: FnMut(&TestResult<S>) -> ()>(&self, action: F) {
        self.0.iter().for_each(action)
    }

    fn for_each_failed<F: FnMut(&TestResult<S>) -> ()>(&self, action: F) {
        self.0.iter().filter(|r| r.is_fail()).for_each(action)
    }
}

trait Stringable {
    fn str(&self) -> Cow<str>;
}

impl<B: AsRef<[u8]>> Stringable for B {
    fn str(&self) -> Cow<str> {
        String::from_utf8_lossy(self.as_ref())
    }
}

#[test]
fn one_success_test() {
    let root = TempDir::default();
    let project = Project::new(&root).create();

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
fn one_fail_test() {
    let root = TempDir::default();
    let project = Project::new(&root).create();

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

#[test]
fn parametrize_simple_should_compile() {
    let root = TempDir::default();
    let project = Project::new(&root).create();

    project.set_code_file(resources("parametrize_simple.rs"));

    let output = project.compile().unwrap();

    assert_eq!(Some(0), output.status.code(), "Compile error due: {}", output.stderr.str())
}

#[test]
fn parametrize_simple_happy_path() {
    let root = TempDir::default().permanent();
    let project = Project::new(&root).create();

    project.set_code_file(resources("parametrize_simple.rs"));

    let output = project.run_tests().unwrap();

    TestResults::new()
        .ok("strlen_test_case_0")
        .ok("strlen_test_case_1")
        .assert(output);
}

#[derive(Clone)]
pub struct Project {
    root: PathBuf,
    name: OsString,
}

impl Project {
    pub fn new<P: AsRef<Path>>(root: P) -> Self {
        Self {
            root: root.as_ref().to_owned(),
            name: "project".into(),
        }
    }

    pub fn name<O: AsRef<OsStr>>(mut self, name: O) -> Self {
        self.name = name.as_ref().to_owned();
        self
    }

    pub fn path(&self) -> PathBuf {
        self.root.join(&self.name)
    }

    pub fn run_tests(&self) -> Result<std::process::Output, std::io::Error> {
        Command::new("cargo")
            .current_dir(&self.path())
            .arg("test")
            .output()
    }

    pub fn compile(&self) -> Result<std::process::Output, std::io::Error> {
        Command::new("cargo")
            .current_dir(&self.path())
            .arg("build")
            .output()
    }

    pub fn create(self) -> Self {
        if 0 != Command::new("cargo")
            .current_dir(&self.root)
            .arg("init")
            .arg(&self.name)
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .unwrap()
            .wait()
            .unwrap()
            .code()
            .unwrap() {
            panic!("cargo init return an error code");
        }
        self.add_dependency();
        std::fs::File::create(self.code_path()).unwrap();
        self
    }

    pub fn set_code_file<P: AsRef<Path>>(&self, src: P) {
        std::fs::copy(
            src,
            self.code_path(),
        ).unwrap();
    }

    pub fn append_code<S: AsRef<str>>(&self, code: S) {
        std::fs::OpenOptions::new()
            .append(true)
            .open(self.code_path())
            .unwrap()
            .write_all(code.as_ref().as_ref())
            .unwrap()
    }

    fn code_path(&self) -> PathBuf {
        self.path()
            .join("src")
            .join("lib.rs")
    }

    fn add_dependency(&self) {
        if 0 != Command::new("cargo")
            .current_dir(&self.path())
            .arg("add")
            .arg("rstest")
            .arg(&format!("--path={}",
                          std::env::current_dir().unwrap().as_os_str().to_str().unwrap()))
            .spawn()
            .unwrap()
            .wait()
            .unwrap()
            .code()
            .unwrap() {
            panic!("cargo add return an error code");
        }
    }
}
