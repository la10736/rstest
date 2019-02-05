use rstest::rstest;

#[derive(Debug)]
struct A {}

fn fu32() -> u32 { 42 }
fn fstring() -> String { "A String".to_string() }
fn ftuple() -> (A, String, i32) { (A{}, "A String".to_string(), -12) }

#[rstest(trace)]
fn should_fail(fu32: u32, fstring: String, ftuple: (A, String, i32)) {
    assert!(false);
}
