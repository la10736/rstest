use rstest::rstest;

struct A;
struct B;
#[derive(Debug)]
struct D;

fn fu32() -> u32 { 42 }
fn fd() -> D { D {} }
fn fa() -> A { A {} }
fn fa2() -> A { A {} }
fn fb() -> B { B {} }

#[rstest(trace::notrace(fa,fa2,fb))]
fn should_fail(fu32: u32, fa: A, fd: D, fa2: A, fb: B) {
    assert!(false);
}
