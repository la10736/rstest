use rstest::*;
use lazy_static::lazy_static;
use std::path::{PathBuf, Path};
use std::str::FromStr;

use rstest::*;

struct Entry { name: String, age: u8 }
trait Repository {
    fn add(&mut self, name: &str, age: u8) -> &Entry;
    fn entries<'a, 'b: 'a>(&'b self) -> Box<dyn Iterator<Item=&'b Entry> + 'a>;
}
trait Processor { fn send(&mut self, message: &str, entry: &Entry); }

#[derive(Default)]
struct Rep(Vec<Entry>);
impl Repository for Rep {
    fn add(&mut self, name: &str, age: u8) -> &Entry {
        self.0.push(Entry { name: name.to_owned() , age });
        self.0.last().unwrap()
    }

    fn entries<'a, 'b: 'a>(&'b self) -> Box<dyn Iterator<Item=&'b Entry> + 'a> {
        Box::new(self.0.iter())
    }
}

struct RepositoryProcessor<R, P>
    where R: Repository,
        P: Processor
{
    repository: R,
    processor: P
}

impl<R, P> RepositoryProcessor<R, P>
    where R: Repository,
          P: Processor
{
    pub fn new(repository: R, processor: P) -> Self {
         RepositoryProcessor { repository, processor }
    }

    pub fn send_all(&mut self, message: &str){
        unimplemented!()
    }
}

#[fixture]
fn empty_repository() -> Rep {
    Default::default()
}

#[fixture]
fn alice_and_bob(mut empty_repository: impl Repository) -> impl Repository {
     empty_repository.add("Bob", 21);
     empty_repository.add("Alice", 22);
     empty_repository
}

#[derive(Default)]
struct FakeProcessor{
    output: String
}

impl Processor for FakeProcessor {
    fn send(&mut self, message: &str, entry: &Entry) {
        unimplemented!()
    }
}

#[fixture]
fn string_processor() -> FakeProcessor {
    Default::default()
}


#[rstest]
fn should_process_two_users(alice_and_bob: impl Repository, string_processor: FakeProcessor) {
    let mut processor = RepositoryProcessor::new(alice_and_bob, string_processor);

    processor.send_all("Good Morning");

    assert_eq!(2, processor.processor.output.matches("Good Morning").count());
    assert!(processor.processor.output.contains("Bob"));
    assert!(processor.processor.output.contains("Alice"));
}
