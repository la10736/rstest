[![Crate][crate-image]][crate-link]
[![Docs][docs-image]][docs-link]
[![Status][test-action-image]][test-action-link]
[![Apache 2.0 Licensed][license-apache-image]][license-apache-link]
[![MIT Licensed][license-mit-image]][license-mit-link]
![Rust 1.32+][rustc-image]

# Fixture-based test framework for Rust

`rstest` uses procedural macros to help you on writing 
fixtures and table-based tests. To use it, add the 
following lines to your `Cargo.toml` file:

```
[dev-dependencies]
rstest = "0.5"
```

The core idea is that you can inject your test dependency
by pass them as your test arguments. In follow example
you define a `fixture` and then use it in two tests by
simply indicate it as argument:

```rust
use rstest::*;

#[fixture]
pub fn fixture() -> u32 { 42 }

#[rstest]
fn should_success(fixture: u32) {
    assert_eq!(fixture, 42);
}

#[rstest]
fn should_fail(fixture: u32) {
    assert_ne!(fixture, 42);
}
```

You can also inject values in some other ways. For instance you can
create a set of test by simply indicate the injected values for each
case: `rstest` will generate an indipendent test for each case.

```rust
use rstest::rstest;

#[rstest(input, expected,
    case(0, 0),
    case(1, 1),
    case(2, 1),
    case(3, 2),
    case(4, 3)
)]
fn fibonacci_test(input: u32, expected: u32) {
    assert_eq!(expected, fibonacci(input))
}
```

Running `cargo test` in this case executes five tests:

```bash
running 5 tests
test fibonacci_test::case_1 ... ok
test fibonacci_test::case_2 ... ok
test fibonacci_test::case_3 ... ok
test fibonacci_test::case_4 ... ok
test fibonacci_test::case_5 ... ok

test result: ok. 5 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

If you need to just indicate a bounch of values for which you
need to run your test you can use `var => [list, of, values]`
syntax:

```rust
use rstest::rstest;

#[rstest(
    value => [None, Some(""), Some("    ")]
)]
fn should_be_invalid(value: Option<&str>) {
    assert!(!valid(value))
}
```

Or create a _matrix_ test by use _list of values_ for some
variables that will generate a cartesian products of all
values.

All these features can be used together by mix fixture variables,
fixed cases and bounch of values. For istance you need to
tests that given your repository in cases of loged in or guest 
user should return an invalid query error.

```rust
use rstest::*;

#[fixture]
fn repository() -> InMemoryRepository {
    let mut r = InMemoryRepository::default();
    // fill repo by some date
    r
}

#[fixture]
fn alice() -> User {
    User::Logged("Alice", "2001-10-04".into(), "London", "UK")
}

#[rstest(user,
    case::logged_user(alice()), // We can use `fixture` also as standard function
    case::guest(User::Guest),   // We can give a name to every case : `guest` in this case
    query => ["     ", "^%$#@!", "...." ]
)]
#[should_panic(expected = "Invalid query error")] // We whould test a panic
fn should_be_invalid_query_error(repository: impl Repository, user: impl User, query: &str) {
    repository.find_items(user, query).unwrap()
}
```

This example'll generate exactly 6 tests grupped by 2 different cases:

```
should_be_invalid_query_error::case_1_logged_user::query_1
should_be_invalid_query_error::case_1_logged_user::query_2
should_be_invalid_query_error::case_1_logged_user::query_3
should_be_invalid_query_error::case_2_guest::query_1
should_be_invalid_query_error::case_2_guest::query_2
should_be_invalid_query_error::case_2_guest::query_3
```

Is it all? Not yet!

Fixture can be injected by other fixture and you can call
them by just some of it's arguments.

```rust
#[fixture]
fn name() -> &'static str {
    "Alice"
}

#[fixture]
fn age() -> u8 {
    22
}

#[fixture]
fn user(name: &str, age: u8) -> User {
    User::new(name, age)
}

#[rstest]
fn is_alice(user: User) {
    assert_eq!(user.name(), "Alice")
}

#[rstest]
fn is_22(user: User) {
    assert_eq!(user.age(), 22)
}

#[rstest(user("Bob"))]
fn is_bob(user: User) {
    assert_eq!(user.name(), "Bob")
}

#[rstest(user("", 42))]
fn is_42(user: User) {
    assert_eq!(user.age(), 42)
}
```

Now you should use fixture also to just provide _default
value_ but it'll change soon by introduce a syntax
for dafault values without the need of the fixture function
definition. 

Finally if you need traceing the input values you can just
add `trace` attribute to your test to enable the dump of all input
variables. 

```rust
#[rstest_parametrize(number, name, tuple,
    case(42, "FortyTwo", ("minus twelve", -12)),
    case(24, "TwentyFour", ("minus twentyfour", -24))
    ::trace //This attribute enable traceing
)]
fn should_fail(number: u32, name: &str, tuple: (&str, i32)) {
    assert!(false); // <- stdout come out just for failed tests
}
```

```
running 2 tests
test should_fail::case_1 ... FAILED
test should_fail::case_2 ... FAILED

failures:

---- should_fail::case_1 stdout ----
------------ TEST ARGUMENTS ------------
number = 42
name = "FortyTwo"
tuple = ("minus twelve", -12)
-------------- TEST START --------------
thread 'should_fail::case_1' panicked at 'assertion failed: false', src/main.rs:64:5
note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace.

---- should_fail::case_2 stdout ----
------------ TEST ARGUMENTS ------------
number = 24
name = "TwentyFour"
tuple = ("minus twentyfour", -24)
-------------- TEST START --------------
thread 'should_fail::case_2' panicked at 'assertion failed: false', src/main.rs:64:5


failures:
    should_fail::case_1
    should_fail::case_2

test result: FAILED. 0 passed; 2 failed; 0 ignored; 0 measured; 0 filtered out
```

In case some variables don't implement `Debug` trait you have an error
but you can also exclude a variable by use 
`notrace(var,list,that,not,implement,Debug)` attribute.

You can learn more on [Docs](docs-link) and find more 
examples in [`resources`](resources) directory and in 
[`rs8080`](https://github.com/la10736/rs8080/blob/master/src/cpu/test.rs) 
which uses this module in-depth.

## Changelog

See [CHANGELOG.md](CHANGELOG.md)

## License

Licensed under either of

* Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or 
[license-apache-link])

* MIT license ([LICENSE-MIT](LICENSE-MIT) or [license-MIT-link])
at your option.

[//]: # (links)

[crate-image]: https://img.shields.io/crates/v/rstest.svg
[crate-link]: https://crates.io/crates/rstest
[docs-image]: https://docs.rs/rstest/badge.svg
[docs-link]: https://docs.rs/rstest/
[test-action-image]: https://github.com/la10736/rstest/workflows/Test/badge.svg
[test-action-link]: https://github.com/la10736/rstest/actions?query=workflow:Test
[license-apache-image]: https://img.shields.io/badge/license-Apache2.0-blue.svg
[license-mit-image]: https://img.shields.io/badge/license-MIT-blue.svg
[license-apache-link]: http://www.apache.org/licenses/LICENSE-2.0
[license-MIT-link]: http://opensource.org/licenses/MIT
[rustc-image]: https://img.shields.io/badge/rustc-1.32+-blue.svg
