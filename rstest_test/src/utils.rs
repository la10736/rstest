use std::borrow::Cow;
use std::thread;

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

impl<S: AsRef<str>> TestResult<S> {
    pub fn is_fail(&self) -> bool {
        use self::TestResult::*;
        matches!(*self, Fail(_))
    }

    pub fn is_ok(&self) -> bool {
        use self::TestResult::*;
        matches!(*self, Ok(_))
    }

    pub fn name(&self) -> String {
        use self::TestResult::*;
        match *self {
            Ok(ref s) => s.as_ref().to_owned(),
            Fail(ref s) => s.as_ref().to_owned(),
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
pub struct TestResults<S>(Vec<TestResult<S>>)
where
    S: AsRef<str> + Clone;

impl<S> TestResults<S>
where
    S: AsRef<str> + Clone,
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

        self.for_each(|t| {
            assert_regex!(
                format!("test {}( - should panic)? ... {}", t.name(), t.msg()),
                output
            )
        });

        if self.should_fail() {
            assert_in!(output, "failures:".to_string());
        }

        self.for_each_failed(|t| assert_in!(output, format!("    {}", t.name())));
    }

    fn should_fail(&self) -> bool {
        self.0.iter().any(|r| r.is_fail())
    }

    fn for_each<F: FnMut(&TestResult<S>)>(&self, action: F) {
        self.0.iter().for_each(action)
    }

    fn for_each_failed<F: FnMut(&TestResult<S>)>(&self, action: F) {
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
    fn should_get_test_path() {
        use super::*;

        assert_eq!("utils::test::should_get_test_path", testname());
    }
}

pub fn sanitize_name<S: AsRef<str>>(s: S) -> String {
    s.as_ref().replace(':', "_").replace("__", "_")
}
