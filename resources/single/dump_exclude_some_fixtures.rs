use rstest::*;

struct A;
struct B;
#[derive(Debug)]
struct D;

#[fixture]
fn fu32() -> u32 { 42 }
#[fixture]
fn fd() -> D { D {} }
#[fixture]
fn fa() -> A { A {} }
#[fixture]
fn fa2() -> A { A {} }
#[fixture]
fn fb() -> B { B {} }

#[rstest(trace::notrace(fa,fa2,fb))]
fn should_fail(fu32: u32, fa: A, fd: D, fa2: A, fb: B) {
    assert!(false);
}
