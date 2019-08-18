use std::path::{Path, PathBuf};
use std::borrow::Cow;
use std::thread;


pub fn resources<O: AsRef<Path>>(name: O) -> PathBuf {
    Path::new("resources").join(name)
}

#[derive(Clone)]
pub enum TestResult<S: AsRef<str>> {
    Ok(S),
    Fail(S),
}

#[macro_export]
macro_rules! assert_in {
    ($text:expr, $message:expr) => ({
        match (&$text, &$message) {
            (text_val, message_val) => {
                if !text_val.contains(message_val) {
                    panic!(r#"assertion failed: `text not contains message`
         text: `{}`,
         message: `{}`"#, text_val, message_val)
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
         text: `{}`,
         message: `{}`: {}"#, text_val, message_val, format_args!($($arg)+))
                }
            }
        }
        });
}

impl<S: AsRef<str>> TestResult<S> {
    pub fn is_fail(&self) -> bool {
        use self::TestResult::*;
        match *self {
            Fail(_) => true,
            _ => false
        }
    }

    pub fn is_ok(&self) -> bool {
        use self::TestResult::*;
        match *self {
            Ok(_) => true,
            _ => false
        }
    }

    pub fn name(&self) -> String {
        use self::TestResult::*;
        match self {
            &Ok(ref s) => s.as_ref().to_owned(),
            &Fail(ref s) => s.as_ref().to_owned()
        }
    }

    pub fn msg(&self) -> &'static str {
        use self::TestResult::*;
        match *self {
            Ok(_) => "ok",
            Fail(_) => "FAILED",
        }
    }
}

#[derive(Default, Clone)]
pub struct TestResults<S>(Vec<TestResult<S>>) where S: AsRef<str> + Clone;

impl<S> TestResults<S>
    where S: AsRef<str> + Clone
{
    pub fn new() -> Self {
        TestResults(vec![])
    }

    pub fn ok(self, name: S) -> Self {
        self.append(TestResult::Ok(name))
    }

    pub fn fail(self, name: S) -> Self {
        self.append(TestResult::Fail(name))
    }

    pub fn append(mut self, test: TestResult<S>) -> Self {
        self.0.push(test);
        self
    }

    pub fn assert(&self, output: ::std::process::Output) {
        let tests = &self.0;

        let (expected_code, msg) = if !self.should_fail()
            { (0, "Unexpected fails!") } else { (101, "Some test should fail!") };
        assert_eq!(Some(expected_code), output.status.code(),
                   "{}\n Console: \nOUT:\n{}\nERR:\n{}\n",
                   msg, output.stdout.str(), output.stderr.str());

        let stderr = output.stderr.str();
        let output = output.stdout.str();
        if output.is_empty() {
            eprintln!("Stderr: {}", stderr);
            panic!("Empty stdout!");
        }

        assert_in!(output, format!("running {} test", tests.len()));

        self.for_each(
            |t| assert_in!(output, format!("test {} ... {}", t.name(), t.msg()))
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

pub trait Stringable {
    fn str(&self) -> Cow<str>;
}

impl<B: AsRef<[u8]>> Stringable for B {
    fn str(&self) -> Cow<str> {
        String::from_utf8_lossy(self.as_ref())
    }
}

pub fn testname() -> String {
    thread::current().name().unwrap().to_string()
}

pub trait CountMessageOccurrence {
    fn count<S: AsRef<str>>(&self, message: S) -> usize;
}

impl<ST> CountMessageOccurrence for ST where ST: AsRef<str> {
    fn count<S: AsRef<str>>(&self, message: S) -> usize {
        self.as_ref().lines()
            .filter(|line| line.contains(
                message.as_ref()))
            .count()
    }
}
