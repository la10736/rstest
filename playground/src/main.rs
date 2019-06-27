//use std::cell::RefCell;
//use std::rc::Rc;
use rstest::*;

struct Entry { name: String, age: u8 }

trait Repository {
    fn create(&mut self, name: &str, age: u8) -> &Entry;
    fn entries<'a>(&'a self) -> Box<dyn Iterator<Item=&'a Entry> + 'a>;
}

trait Processor {
    fn send(&self, entry: &Entry, message: &str);
    fn send_all<'a>(&self, entries: impl Iterator<Item=&'a Entry>, message: &str) {
        entries.map(|e| self.send(e, message)).count();
    }
}

struct RepositoryProcessor<R, P>
    where R: Repository,
          P: Processor
{
    repository: R,
    processor: P,
}

impl<R, P> RepositoryProcessor<R, P>
    where R: Repository,
          P: Processor
{
    pub fn new(repository: R, processor: P) -> Self {
        RepositoryProcessor { repository, processor }
    }

    pub fn send_all(&self, message: &str) {
        self.processor.send_all(self.repository.entries(), message)
    }
}

#[derive(Default)]
struct InMemoryRepository(Vec<Entry>);

impl Repository for InMemoryRepository {
    fn create(&mut self, name: &str, age: u8) -> &Entry {
        self.0.push(Entry { name: name.to_owned(), age });
        self.0.last().unwrap()
    }

    fn entries<'a>(&'a self) -> Box<dyn Iterator<Item=&'a Entry> + 'a> {
        Box::new(self.0.iter())
    }
}

#[derive(Default)]
struct FakeProcessor {
    output: RefCell<String>
}

impl Processor for FakeProcessor {
    fn send(&self, entry: &Entry, message: &str) {
        self.output
            .borrow_mut()
            .push_str(&format!("[{} {}]: {}\n", entry.name, entry.age, message))
    }
}

impl Processor for Rc<FakeProcessor> {
    fn send(&self, entry: &Entry, message: &str) {
        self.as_ref().send(entry, message)
    }
}

#[fixture]
fn empty_repository() -> InMemoryRepository {
    Default::default()
}

#[fixture]
fn alice_and_bob(mut empty_repository: impl Repository) -> impl Repository {
    empty_repository.create("Bob", 21);
    empty_repository.create("Alice", 22);
    empty_repository
}

#[fixture]
fn string_processor() -> Rc<FakeProcessor> {
    Rc::new(Default::default())
}

#[rstest]
fn should_process_two_users(alice_and_bob: impl Repository, string_processor: Rc<FakeProcessor>) {
    let processor =
        RepositoryProcessor::new(alice_and_bob, string_processor.clone());

    processor.send_all("Good Morning");

    let out = string_processor.output.borrow();

    assert_eq!(2, out.matches("Good Morning").count());
    assert!(out.contains("Bob"));
    assert!(out.contains("Alice"));
}

#[rstest_parametrize(input, expected,
    case("foo", 3),
    case(String::from("bar"), 3),
)]
fn len<S: AsRef<str>>(input: S, expected: usize) {
    assert_eq!(expected, input.as_ref().len())
}

#[rstest_parametrize(input, expected,
case("foo", 3),
case(String::from("bar"), 3),
)]
fn len_by_impl(input: impl AsRef<str>, expected: usize) {
    assert_eq!(expected, input.as_ref().len())
}
