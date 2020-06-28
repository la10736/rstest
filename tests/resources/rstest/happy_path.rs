use rstest::*;

#[fixture]
fn inject() -> u32 { 0 }

#[fixture]
fn ex() -> u32 { 42 }

#[fixture]
fn fix(inject: u32, ex: u32) -> bool { (inject * 2) == ex }

#[rstest(
    fix(21),
    a, b,
    case(21, 2),
    case::second(14, 3),
    expected => [4, 2*3-2],
    input => ["ciao", "buzz"],
)]
fn happy(fix: bool, a: u32, b: u32, expected: usize, input: &str) {
    assert!(fix);
    assert_eq!(a*b, 42);
    assert_eq!(expected, input.len());
}
