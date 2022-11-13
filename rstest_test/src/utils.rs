use std::borrow::Cow;
use std::process::Output;
use std::thread;

#[macro_export]
macro_rules! assert_in {
    ($text:expr, $message:expr) => ({
        match (&$text, &$message) {
            (text_val, message_val) => {
                if !text_val.contains(message_val) {
                    panic!(r#"assertion failed: `text don't contain message`
         text: `{}`,
         message: `{}`"#, text_val, message_val)
                }
            }
        }
        });
    ($text:expr, $message:expr, ) => (
        assert_in!($text, $message)
    );
    ($text:expr, $message:expr, $($arg:tt)+) => ({
        match (&$text, &$message) {
            (text_val, message_val) => {
                if !text_val.contains(message_val) {
                    panic!(r#"assertion failed: `text don't contain message`
         text: `{}`,
         message: `{}`: {}"#, text_val, message_val, format_args!($($arg)+))
                }
            }
        }
        });
}

#[macro_export]
macro_rules! assert_all_in {
    ($text:expr, $expected:expr) => (
        assert_in!($text, $expected)
    );
    ($text:expr, $expected:expr, ) => (
        assert_in!($text, $message)
    );
    ($text:expr, $expected:expr, $( $others:expr ) ,+) => (
        {
            assert_in!($text, $expected);
            assert_all_in!($text $(, $others)*);
        }
    );
}

#[macro_export]
macro_rules! assert_not_in {
    ($text:expr, $message:expr) => ({
        match (&$text, &$message) {
            (text_val, message_val) => {
                if text_val.contains(message_val) {
                    panic!(r#"assertion failed: `text contains message`
         text: `{}`,
         message: `{}`"#, text_val, message_val)
                }
            }
        }
        });
    ($message:expr, $expected:expr, ) => (
        assert_not_in!($message, $expected)
    );
    ($text:expr, $message:expr, $($arg:tt)+) => ({
        match (&$text, &$message) {
            (text_val, message_val) => {
                if text_val.contains(message_val) {
                    panic!(r#"assertion failed: `text contains message`
         text: `{}`,
         message: `{}`: {}"#, text_val, message_val, format_args!($($arg)+))
                }
            }
        }
        });
}

#[macro_export]
macro_rules! assert_regex {
    ($regex:expr, $text:expr) => ({
        match (&$text, &$regex) {
            (text_val, regex_val) => {
                use $crate::regex::Regex;
                if !Regex::new(regex_val).unwrap().is_match(text_val) {
                    panic!(r#"assertion failed: `text don't satisfy regex`
         regex: `{}`,
         text: `{}`"#, regex_val, text_val)
                }
            }
        }
        });
    ($regex:expr, $message:expr, ) => (
        assert_regex_in!($regex, $message)
    );
    ($regex:expr, $text:expr, $($arg:tt)+) => ({
        match (&$text, &$regex) {
            (text_val, regex_val) => {
                use $crate::regex::Regex;
                if !!Regex::new(regex_val).unwrap().is_match(text_val) {
                    panic!(r#"assertion failed: `text don't satisfy regex`
         regex: `{}`,
         text: `{}`: {}"#, regex_val, text_val, format_args!($($arg)+))
                }
            }
        }
        });
}

#[derive(Clone)]
pub(crate) struct TestOccurence {
    exactly: bool,
    occurence: usize,
}

impl Default for TestOccurence {
    fn default() -> Self {
        Self {
            exactly: true,
            occurence: 1,
        }
    }
}

#[derive(Clone)]
pub(crate) enum TestResult<S: AsRef<str>> {
    Ok(S, TestOccurence),
    Fail(S, TestOccurence),
}

impl<S: AsRef<str>> TestResult<S> {
    fn assert(&self, output: impl AsRef<str>) {
        let (name, occurence) = match self {
            TestResult::Ok(n, o) => (n, o),
            TestResult::Fail(n, o) => (n, o),
        };
        let regex = if occurence.exactly {
            format!("test {}( - should panic)? ... {}", self.name(), self.msg())
        } else {
            format!(
                "test .*{}.*( - should panic)? ... {}",
                self.name(),
                self.msg()
            )
        };
        match occurence.occurence {
            0 => {}
            1 => {
                assert_regex!(regex, output.as_ref());
            }
            n => {
                assert_regex!(regex, output.as_ref());
                assert_eq!(
                    n,
                    output.count_regex(regex),
                    "test {} is present but wrong count",
                    self.name()
                );
            }
        }
    }

    fn ok(name: S, exactly: bool, occurence: usize) -> Self {
        Self::Ok(name, TestOccurence { exactly, occurence })
    }

    fn fail(name: S, exactly: bool, occurence: usize) -> Self {
        Self::Fail(name, TestOccurence { exactly, occurence })
    }
}

impl<S: AsRef<str>> TestResult<S> {
    pub fn is_fail(&self) -> bool {
        use self::TestResult::*;
        matches!(*self, Fail(_, _))
    }

    pub fn is_ok(&self) -> bool {
        use self::TestResult::*;
        matches!(*self, Ok(_, _))
    }

    pub fn name(&self) -> String {
        use self::TestResult::*;
        match *self {
            Ok(ref s, _) => s.as_ref().to_owned(),
            Fail(ref s, _) => s.as_ref().to_owned(),
        }
    }

    pub fn msg(&self) -> &'static str {
        use self::TestResult::*;
        match *self {
            Ok(_, _) => "ok",
            Fail(_, _) => "FAILED",
        }
    }
}

#[derive(Default, Clone)]
pub struct TestResults<S>
where
    S: AsRef<str> + Clone,
{
    results: Vec<TestResult<S>>,
    contains: bool,
}

impl<S> TestResults<S>
where
    S: AsRef<str> + Clone,
{
    pub fn new() -> Self {
        TestResults {
            results: vec![],
            contains: false,
        }
    }

    pub fn with_contains(self, contains: bool) -> Self {
        Self {
            results: self.results,
            contains,
        }
    }

    pub fn ok(self, name: S) -> Self {
        let contains = self.contains;
        self.append(TestResult::ok(name, !contains, 1))
    }

    pub fn fail(self, name: S) -> Self {
        let contains = self.contains;
        self.append(TestResult::fail(name, !contains, 1))
    }

    pub(crate) fn append(mut self, test: TestResult<S>) -> Self {
        self.results.push(test);
        self
    }

    pub fn assert(&self, output: ::std::process::Output) {
        let tests = &self.results;

        let (expected_code, msg) = if !self.should_fail() {
            (0, "Unexpected fails!")
        } else {
            (101, "Some test should fail!")
        };
        assert_eq!(
            Some(expected_code),
            output.status.code(),
            "{}\n Console: \nOUT:\n{}\nERR:\n{}\n",
            msg,
            output.stdout.str(),
            output.stderr.str()
        );

        let stderr = output.stderr.str();
        let output = output.stdout.str();
        if output.is_empty() {
            eprintln!("Stderr: {}", stderr);
            panic!("Empty stdout!");
        }

        assert_in!(output, format!("running {} test", tests.len()));

        self.for_each(|t| t.assert(output.as_ref()));

        if self.should_fail() {
            assert_in!(output, "failures:".to_string());
        }

        self.for_each_failed(|t| assert_in!(output, format!("    {}", t.name())));
    }

    fn should_fail(&self) -> bool {
        self.results.iter().any(|r| r.is_fail())
    }

    fn for_each<F: FnMut(&TestResult<S>)>(&self, action: F) {
        self.results.iter().for_each(action)
    }

    fn for_each_failed<F: FnMut(&TestResult<S>)>(&self, action: F) {
        self.results.iter().filter(|r| r.is_fail()).for_each(action)
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

    fn count_regex<S: AsRef<str>>(&self, message: S) -> usize;
}

impl<ST> CountMessageOccurrence for ST
where
    ST: AsRef<str>,
{
    fn count<S: AsRef<str>>(&self, message: S) -> usize {
        self.as_ref()
            .lines()
            .filter(|line| line.contains(message.as_ref()))
            .count()
    }

    fn count_regex<S: AsRef<str>>(&self, regex: S) -> usize {
        let regex = regex::Regex::new(regex.as_ref()).unwrap();
        self.as_ref()
            .lines()
            .filter(|line| regex.is_match(line))
            .count()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn should_count_message_occurence() {
        let foo_occurences = "
        foobaz
        bar
        foobazfoo
        baz
        foo
        "
        .count("foo");

        assert_eq!(3, foo_occurences);
    }

    #[test]
    fn should_count_regex_occurence() {
        let message = "
        123
        aa2bb
        abc
        2aa
        foo
        "
        .count_regex(r"\d+");

        assert_eq!(3, message);
    }

    #[test]
    fn should_get_test_path() {
        use super::*;

        assert_eq!("utils::test::should_get_test_path", testname());
    }
}

pub fn sanitize_name<S: AsRef<str>>(s: S) -> String {
    s.as_ref().replace(':', "_").replace("__", "_")
}
